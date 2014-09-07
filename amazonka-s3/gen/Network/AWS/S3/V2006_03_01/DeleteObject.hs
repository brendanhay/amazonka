{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.DeleteObject
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
module Network.AWS.S3.V2006_03_01.DeleteObject
    (
    -- * Request
      DeleteObject
    -- ** Request constructor
    , mkDeleteObject
    -- ** Request lenses
    , doBucket
    , doKey
    , doMFA
    , doVersionId

    -- * Response
    , DeleteObjectResponse
    -- ** Response lenses
    , dorsDeleteMarker
    , dorsVersionId
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

data DeleteObject = DeleteObject
    { _doBucket :: BucketName
    , _doKey :: ObjectKey
    , _doMFA :: Maybe Text
    , _doVersionId :: Maybe ObjectVersionId
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteObject' request.
mkDeleteObject :: BucketName -- ^ 'doBucket'
               -> ObjectKey -- ^ 'doKey'
               -> DeleteObject
mkDeleteObject p1 p2 = DeleteObject
    { _doBucket = p1
    , _doKey = p2
    , _doMFA = Nothing
    , _doVersionId = Nothing
    }

doBucket :: Lens' DeleteObject BucketName
doBucket = lens _doBucket (\s a -> s { _doBucket = a })

doKey :: Lens' DeleteObject ObjectKey
doKey = lens _doKey (\s a -> s { _doKey = a })

-- | The concatenation of the authentication device's serial number, a space,
-- and the value that is displayed on your authentication device.
doMFA :: Lens' DeleteObject (Maybe Text)
doMFA = lens _doMFA (\s a -> s { _doMFA = a })

-- | VersionId used to reference a specific version of the object.
doVersionId :: Lens' DeleteObject (Maybe ObjectVersionId)
doVersionId = lens _doVersionId (\s a -> s { _doVersionId = a })

instance ToPath DeleteObject where
    toPath DeleteObject{..} = mconcat
        [ "/"
        , toBS _doBucket
        , "/"
        , toBS _doKey
        ]

instance ToQuery DeleteObject where
    toQuery DeleteObject{..} = mconcat
        [ "versionId" =? _doVersionId
        ]

instance ToHeaders DeleteObject where
    toHeaders DeleteObject{..} = concat
        [ "x-amz-mfa" =: _doMFA
        ]

instance ToBody DeleteObject

data DeleteObjectResponse = DeleteObjectResponse
    { _dorsDeleteMarker :: Maybe Bool
    , _dorsVersionId :: Maybe ObjectVersionId
    } deriving (Show, Generic)

-- | Specifies whether the versioned object that was permanently deleted was
-- (true) or was not (false) a delete marker.
dorsDeleteMarker :: Lens' DeleteObjectResponse (Maybe Bool)
dorsDeleteMarker =
    lens _dorsDeleteMarker (\s a -> s { _dorsDeleteMarker = a })

-- | Returns the version ID of the delete marker created as a result of the
-- DELETE operation.
dorsVersionId :: Lens' DeleteObjectResponse (Maybe ObjectVersionId)
dorsVersionId = lens _dorsVersionId (\s a -> s { _dorsVersionId = a })

instance AWSRequest DeleteObject where
    type Sv DeleteObject = S3
    type Rs DeleteObject = DeleteObjectResponse

    request = delete
    response _ = headerResponse $ \hs ->
        pure DeleteObjectResponse
            <*> hs ~:? "x-amz-delete-marker"
            <*> hs ~:? "x-amz-version-id"
