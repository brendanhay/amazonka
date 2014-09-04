{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.DeleteBucket
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the bucket. All objects (including all object versions and Delete
-- Markers) in the bucket must be deleted before the bucket itself can be
-- deleted.
module Network.AWS.S3.V2006_03_01.DeleteBucket
    (
    -- * Request
      DeleteBucket
    -- ** Request constructor
    , mkDeleteBucketRequest
    -- ** Request lenses
    , dbrBucket

    -- * Response
    , DeleteBucketResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteBucket' request.
mkDeleteBucketRequest :: BucketName -- ^ 'dbrBucket'
                      -> DeleteBucket
mkDeleteBucketRequest p1 = DeleteBucket
    { _dbrBucket = p1
    }
{-# INLINE mkDeleteBucketRequest #-}

newtype DeleteBucket = DeleteBucket
    { _dbrBucket :: BucketName
    } deriving (Show, Generic)

dbrBucket :: Lens' DeleteBucket (BucketName)
dbrBucket = lens _dbrBucket (\s a -> s { _dbrBucket = a })
{-# INLINE dbrBucket #-}

instance ToPath DeleteBucket where
    toPath DeleteBucket{..} = mconcat
        [ "/"
        , toBS _dbrBucket
        ]

instance ToQuery DeleteBucket

instance ToHeaders DeleteBucket

instance ToBody DeleteBucket

data DeleteBucketResponse = DeleteBucketResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteBucket where
    type Sv DeleteBucket = S3
    type Rs DeleteBucket = DeleteBucketResponse

    request = delete
    response _ = nullaryResponse DeleteBucketResponse
