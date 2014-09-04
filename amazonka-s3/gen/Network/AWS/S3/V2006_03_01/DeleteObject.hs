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
    , deleteObject
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

-- | Minimum specification for a 'DeleteObject' request.
deleteObject :: BucketName -- ^ 'dorBucket'
             -> ObjectKey -- ^ 'dorKey'
             -> DeleteObject
deleteObject p1 p2 = DeleteObject
    { _dorBucket = p1
    , _dorKey = p2
    , _dorMFA = Nothing
    , _dorVersionId = Nothing
    }
{-# INLINE deleteObject #-}

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
dorBucket f x =
    f (_dorBucket x)
        <&> \y -> x { _dorBucket = y }
{-# INLINE dorBucket #-}

dorKey :: Lens' DeleteObject (ObjectKey)
dorKey f x =
    f (_dorKey x)
        <&> \y -> x { _dorKey = y }
{-# INLINE dorKey #-}

-- | The concatenation of the authentication device's serial number, a space,
-- and the value that is displayed on your authentication device.
dorMFA :: Lens' DeleteObject (Maybe Text)
dorMFA f x =
    f (_dorMFA x)
        <&> \y -> x { _dorMFA = y }
{-# INLINE dorMFA #-}

-- | VersionId used to reference a specific version of the object.
dorVersionId :: Lens' DeleteObject (Maybe ObjectVersionId)
dorVersionId f x =
    f (_dorVersionId x)
        <&> \y -> x { _dorVersionId = y }
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

instance ToHeaders DeleteObject where
    toHeaders DeleteObject{..} = concat
        [ "x-amz-mfa" =: _dorMFA
        ]

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
dooDeleteMarker f x =
    f (_dooDeleteMarker x)
        <&> \y -> x { _dooDeleteMarker = y }
{-# INLINE dooDeleteMarker #-}

-- | Returns the version ID of the delete marker created as a result of the
-- DELETE operation.
dooVersionId :: Lens' DeleteObjectResponse (Maybe ObjectVersionId)
dooVersionId f x =
    f (_dooVersionId x)
        <&> \y -> x { _dooVersionId = y }
{-# INLINE dooVersionId #-}

instance AWSRequest DeleteObject where
    type Sv DeleteObject = S3
    type Rs DeleteObject = DeleteObjectResponse

    request = delete
    response _ = headerResponse $ \hs ->
        pure DeleteObjectResponse
            <*> hs ~:? "x-amz-delete-marker"
            <*> hs ~:? "x-amz-version-id"
