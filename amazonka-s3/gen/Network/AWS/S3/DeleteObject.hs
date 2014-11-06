{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

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
import Network.AWS.Request.XML
import Network.AWS.S3.Types

data DeleteObject = DeleteObject
    { _dor1Bucket    :: BucketName
    , _dor1Key       :: ObjectKey
    , _dor1MFA       :: Maybe Text
    , _dor1VersionId :: Maybe ObjectVersionId
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteObject' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dor1Bucket' @::@ 'BucketName'
--
-- * 'dor1Key' @::@ 'ObjectKey'
--
-- * 'dor1MFA' @::@ 'Maybe' 'Text'
--
-- * 'dor1VersionId' @::@ 'Maybe' 'ObjectVersionId'
--
deleteObject :: BucketName -- ^ 'dor1Bucket'
             -> ObjectKey -- ^ 'dor1Key'
             -> DeleteObject
deleteObject p1 p2 = DeleteObject
    { _dor1Bucket    = p1
    , _dor1Key       = p2
    , _dor1MFA       = Nothing
    , _dor1VersionId = Nothing
    }

dor1Bucket :: Lens' DeleteObject BucketName
dor1Bucket = lens _dor1Bucket (\s a -> s { _dor1Bucket = a })

dor1Key :: Lens' DeleteObject ObjectKey
dor1Key = lens _dor1Key (\s a -> s { _dor1Key = a })

-- | The concatenation of the authentication device's serial number, a space,
-- and the value that is displayed on your authentication device.
dor1MFA :: Lens' DeleteObject (Maybe Text)
dor1MFA = lens _dor1MFA (\s a -> s { _dor1MFA = a })

-- | VersionId used to reference a specific version of the object.
dor1VersionId :: Lens' DeleteObject (Maybe ObjectVersionId)
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
    , _dooVersionId    :: Maybe ObjectVersionId
    } deriving (Eq, Ord, Show, Generic)

instance AWSRequest DeleteObject where
    type Sv DeleteObject = S3
    type Rs DeleteObject = DeleteObjectOutput

    request  = delete
    response = const . xmlResponse $ \h x ->
        <$> h ~: "x-amz-delete-marker"
        <*> h ~: "x-amz-version-id"
