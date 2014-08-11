{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.S3.V2006_03_01.DeleteBucketLifecycle
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the lifecycle configuration from the bucket.
module Network.AWS.S3.V2006_03_01.DeleteBucketLifecycle where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

data DeleteBucketLifecycle = DeleteBucketLifecycle
    { _dblrBucket :: BucketName
    } deriving (Show, Generic)

makeLenses ''DeleteBucketLifecycle

instance ToPath DeleteBucketLifecycle where
    toPath DeleteBucketLifecycle{..} = mconcat
        [ "/"
        , toBS _dblrBucket
        ]

instance ToQuery DeleteBucketLifecycle where
    toQuery DeleteBucketLifecycle{..} = mconcat
        [ "lifecycle"
        ]

instance ToHeaders DeleteBucketLifecycle

instance ToBody DeleteBucketLifecycle

data DeleteBucketLifecycleResponse = DeleteBucketLifecycleResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteBucketLifecycleResponse

instance AWSRequest DeleteBucketLifecycle where
    type Sv DeleteBucketLifecycle = S3
    type Rs DeleteBucketLifecycle = DeleteBucketLifecycleResponse

    request = delete
    response _ = nullaryResponse DeleteBucketLifecycleResponse
