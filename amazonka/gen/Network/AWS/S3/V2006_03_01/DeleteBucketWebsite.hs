{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.S3.V2006_03_01.DeleteBucketWebsite
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation removes the website configuration from the bucket.
module Network.AWS.S3.V2006_03_01.DeleteBucketWebsite where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

data DeleteBucketWebsite = DeleteBucketWebsite
    { _dbwrBucket :: BucketName
    } deriving (Show, Generic)

makeLenses ''DeleteBucketWebsite

instance ToPath DeleteBucketWebsite where
    toPath DeleteBucketWebsite{..} = mconcat
        [ "/"
        , toBS _dbwrBucket
        ]

instance ToQuery DeleteBucketWebsite where
    toQuery DeleteBucketWebsite{..} = mconcat
        [ "website"
        ]

instance ToHeaders DeleteBucketWebsite

instance ToBody DeleteBucketWebsite

data DeleteBucketWebsiteResponse = DeleteBucketWebsiteResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteBucketWebsiteResponse

instance AWSRequest DeleteBucketWebsite where
    type Sv DeleteBucketWebsite = S3
    type Rs DeleteBucketWebsite = DeleteBucketWebsiteResponse

    request = delete
    response _ = nullaryResponse DeleteBucketWebsiteResponse
