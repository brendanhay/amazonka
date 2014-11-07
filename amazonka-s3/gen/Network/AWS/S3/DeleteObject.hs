{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.DeleteObject
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes the null version (if there is one) of an object and inserts a
-- delete marker, which becomes the latest version of the object. If there
-- isn't a null version, Amazon S3 does not remove any objects.
module Network.AWS.S3.DeleteObject
    (
    -- * Request
      DeleteObject
    -- ** Request constructor
    , deleteObject
    -- ** Request lenses
    , dor1Bucket
    , dor1Key
    , dor1MFA
    , dor1VersionId

    -- * Response
    , DeleteObjectOutput
    -- ** Response constructor
    , deleteObjectOutput
    -- ** Response lenses
    , dooDeleteMarker
    , dooVersionId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

data DeleteObject = DeleteObject
    { _dor1Bucket    :: Text
    , _dor1Key       :: Text
    , _dor1MFA       :: Maybe Text
    , _dor1VersionId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteObject' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dor1Bucket' @::@ 'Text'
--
-- * 'dor1Key' @::@ 'Text'
--
-- * 'dor1MFA' @::@ 'Maybe' 'Text'
--
-- * 'dor1VersionId' @::@ 'Maybe' 'Text'
--
deleteObject :: Text -- ^ 'dor1Bucket'
             -> Text -- ^ 'dor1Key'
             -> DeleteObject
deleteObject p1 p2 = DeleteObject
    { _dor1Bucket    = p1
    , _dor1Key       = p2
    , _dor1MFA       = Nothing
    , _dor1VersionId = Nothing
    }

dor1Bucket :: Lens' DeleteObject Text
dor1Bucket = lens _dor1Bucket (\s a -> s { _dor1Bucket = a })

dor1Key :: Lens' DeleteObject Text
dor1Key = lens _dor1Key (\s a -> s { _dor1Key = a })

-- | The concatenation of the authentication device's serial number, a space,
-- and the value that is displayed on your authentication device.
dor1MFA :: Lens' DeleteObject (Maybe Text)
dor1MFA = lens _dor1MFA (\s a -> s { _dor1MFA = a })

-- | VersionId used to reference a specific version of the object.
dor1VersionId :: Lens' DeleteObject (Maybe Text)
dor1VersionId = lens _dor1VersionId (\s a -> s { _dor1VersionId = a })

instance ToPath DeleteObject where
    toPath DeleteObject{..} = mconcat
        [ "/"
        , toText _dor1Bucket
        , "/"
        , toText _dor1Key
        ]

instance ToQuery DeleteObject where

instance ToHeaders DeleteObject where
    toHeaders DeleteObject{..} = mconcat
        [ "x-amz-mfa" =: _dor1MFA
        ]

data DeleteObjectOutput = DeleteObjectOutput
    { _dooDeleteMarker :: Maybe Bool
    , _dooVersionId    :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteObjectOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dooDeleteMarker' @::@ 'Maybe' 'Bool'
--
-- * 'dooVersionId' @::@ 'Maybe' 'Text'
--
deleteObjectOutput :: DeleteObjectOutput
deleteObjectOutput = DeleteObjectOutput
    { _dooDeleteMarker = Nothing
    , _dooVersionId    = Nothing
    }

-- | Specifies whether the versioned object that was permanently deleted was
-- (true) or was not (false) a delete marker.
dooDeleteMarker :: Lens' DeleteObjectOutput (Maybe Bool)
dooDeleteMarker = lens _dooDeleteMarker (\s a -> s { _dooDeleteMarker = a })

-- | Returns the version ID of the delete marker created as a result of the
-- DELETE operation.
dooVersionId :: Lens' DeleteObjectOutput (Maybe Text)
dooVersionId = lens _dooVersionId (\s a -> s { _dooVersionId = a })

instance AWSRequest DeleteObject where
    type Sv DeleteObject = S3
    type Rs DeleteObject = DeleteObjectOutput

    request  = delete'
    response = const . xmlResponse $ \h x -> DeleteObjectOutput
        <$> h ~:? "x-amz-delete-marker"
        <*> h ~:? "x-amz-version-id"
