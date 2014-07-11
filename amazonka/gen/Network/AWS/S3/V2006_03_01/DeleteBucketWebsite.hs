{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error)
import           Network.AWS.Request.RestS3
import           Network.AWS.S3.V2006_03_01.Types
import           Network.HTTP.Client  (Response)
import           Prelude              hiding (head)

-- | Default DeleteBucketWebsite request.
deleteBucketWebsite :: BucketName -- ^ 'dbwrBucket'
                    -> DeleteBucketWebsite
deleteBucketWebsite p1 = DeleteBucketWebsite
    { dbwrBucket = p1
    }

data DeleteBucketWebsite = DeleteBucketWebsite
    { dbwrBucket :: BucketName
    } deriving (Show, Generic)

instance ToPath DeleteBucketWebsite where
    toPath DeleteBucketWebsite{..} = mconcat
        [ "/"
        , toBS dbwrBucket
        ]

instance ToQuery DeleteBucketWebsite

instance ToHeaders DeleteBucketWebsite

instance ToBody DeleteBucketWebsite

instance AWSRequest DeleteBucketWebsite where
    type Sv DeleteBucketWebsite = S3

    request  = delete
    response = headerResponse . const $ Right DeleteBucketWebsiteResponse

data instance Rs DeleteBucketWebsite = DeleteBucketWebsiteResponse
    deriving (Eq, Show, Generic)
