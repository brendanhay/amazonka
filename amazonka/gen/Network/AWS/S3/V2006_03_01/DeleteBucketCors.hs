{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.DeleteBucketCors
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the cors configuration information set for the bucket.
module Network.AWS.S3.V2006_03_01.DeleteBucketCors where

import           Control.Applicative
import           Data.ByteString     (ByteString)
import           Data.Default
import           Data.HashMap.Strict (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Request.RestS3
import           Network.AWS.Types   hiding (Error)
import           Network.AWS.S3.V2006_03_01.Types
import           Prelude             hiding (head)


-- | Default DeleteBucketCors request.
deleteBucketCors :: BucketName -- ^ 'dbcrBucket'
                 -> DeleteBucketCors
deleteBucketCors p1 = DeleteBucketCors
    { dbcrBucket = p1
    }

data DeleteBucketCors = DeleteBucketCors
    { dbcrBucket :: BucketName
    } deriving (Eq, Show, Generic)

instance ToPath DeleteBucketCors where
    toPath DeleteBucketCors{..} = mconcat
        [ "/"
        , toBS dbcrBucket
        ]

instance ToQuery DeleteBucketCors

instance ToHeaders DeleteBucketCors

instance ToBody DeleteBucketCors

instance AWSRequest DeleteBucketCors where
    type Sv DeleteBucketCors = S3

    request  = delete
    response = headerResposne $ const DeleteBucketCorsResponse

data instance Rs DeleteBucketCors = DeleteBucketCorsResponse
    deriving (Eq, Show, Generic)
