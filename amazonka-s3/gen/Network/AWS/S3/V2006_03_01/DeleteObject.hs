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
    , mkDeleteObjectRequest
    -- ** Request lenses
    , dorBucket
    , dorKey
    , dorMFA
    , dorVersionId

    -- * Response
    , DeleteObjectResponse
    -- ** Response lenses
    , dooDeleteMarker
    , dooVersionId
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteObject' request.
mkDeleteObjectRequest :: BucketName -- ^ 'dorBucket'
                      -> ObjectKey -- ^ 'dorKey'
                      -> DeleteObject
mkDeleteObjectRequest p1 p2 = DeleteObject
    { _dorBucket = p1
    , _dorKey = p2
    , _dorMFA = Nothing
    , _dorVersionId = Nothing
    }
{-# INLINE mkDeleteObjectRequest #-}

data DeleteObject = DeleteObject
    { _dorBucket :: BucketName
    , _dorKey :: ObjectKey
    , _dorMFA :: Maybe Text
      -- ^ The concatenation of the authentication device's serial number, a
      -- space, and the value that is displayed on your authentication
      -- device.
    , _dorVersionId :: Maybe ObjectVersionId
      -- ^ VersionId used to reference a specific version of the object.
    } deriving (Show, Generic)

dorBucket :: Lens' DeleteObject (BucketName)
dorBucket = lens _dorBucket (\s a -> s { _dorBucket = a })
{-# INLINE dorBucket #-}

dorKey :: Lens' DeleteObject (ObjectKey)
dorKey = lens _dorKey (\s a -> s { _dorKey = a })
{-# INLINE dorKey #-}

-- | The concatenation of the authentication device's serial number, a space,
-- and the value that is displayed on your authentication device.
dorMFA :: Lens' DeleteObject (Maybe Text)
dorMFA = lens _dorMFA (\s a -> s { _dorMFA = a })
{-# INLINE dorMFA #-}

-- | VersionId used to reference a specific version of the object.
dorVersionId :: Lens' DeleteObject (Maybe ObjectVersionId)
dorVersionId = lens _dorVersionId (\s a -> s { _dorVersionId = a })
{-# INLINE dorVersionId #-}

instance ToPath DeleteObject where
    toPath DeleteObject{..} = mconcat
        [ "/"
        , toBS _dorBucket
        , "/"
        , toBS _dorKey
        ]

instance ToQuery DeleteObject where
    toQuery DeleteObject{..} = mconcat
        [ "versionId" =? _dorVersionId
        ]

instance ToHeaders DeleteObject

instance ToBody DeleteObject

data DeleteObjectResponse = DeleteObjectResponse
    { _dooDeleteMarker :: Maybe Bool
      -- ^ Specifies whether the versioned object that was permanently
      -- deleted was (true) or was not (false) a delete marker.
    , _dooVersionId :: Maybe ObjectVersionId
      -- ^ Returns the version ID of the delete marker created as a result
      -- of the DELETE operation.
    } deriving (Show, Generic)

-- | Specifies whether the versioned object that was permanently deleted was
-- (true) or was not (false) a delete marker.
dooDeleteMarker :: Lens' DeleteObjectResponse (Maybe Bool)
dooDeleteMarker = lens _dooDeleteMarker (\s a -> s { _dooDeleteMarker = a })
{-# INLINE dooDeleteMarker #-}

-- | Returns the version ID of the delete marker created as a result of the
-- DELETE operation.
dooVersionId :: Lens' DeleteObjectResponse (Maybe ObjectVersionId)
dooVersionId = lens _dooVersionId (\s a -> s { _dooVersionId = a })
{-# INLINE dooVersionId #-}

instance AWSRequest DeleteObject where
    type Sv DeleteObject = S3
    type Rs DeleteObject = DeleteObjectResponse

    request = delete
    response _ = headerResponse $ \hs ->
        pure DeleteObjectResponse
            <*> hs ~:? "x-amz-delete-marker"
            <*> hs ~:? "x-amz-version-id"
