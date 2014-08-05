{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.S3.V2006_03_01.DeleteBucketTagging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the tags from the bucket.
module Network.AWS.S3.V2006_03_01.DeleteBucketTagging where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

data DeleteBucketTagging = DeleteBucketTagging
    { _dbtrBucket :: BucketName
    } deriving (Show, Generic)

makeLenses ''DeleteBucketTagging

instance ToPath DeleteBucketTagging where
    toPath DeleteBucketTagging{..} = mconcat
        [ "/"
        , toBS _dbtrBucket
        ]

instance ToQuery DeleteBucketTagging where
    toQuery DeleteBucketTagging{..} = mconcat
        [ "tagging"
        ]

instance ToHeaders DeleteBucketTagging

instance ToBody DeleteBucketTagging

data DeleteBucketTaggingResponse = DeleteBucketTaggingResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteBucketTaggingResponse

instance AWSRequest DeleteBucketTagging where
    type Sv DeleteBucketTagging = S3
    type Rs DeleteBucketTagging = DeleteBucketTaggingResponse

    request = delete
    response _ _ = return (Right DeleteBucketTaggingResponse)
