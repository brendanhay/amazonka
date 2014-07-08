{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.DeleteBucketPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the policy from the bucket.
module Network.AWS.S3.V2006_03_01.DeleteBucketPolicy where

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

-- | Smart constructor utilising default fields to
-- specify the minimum viable DeleteBucketPolicy request.
deleteBucketPolicy :: BucketName -- ^ 'dbprBucket'
                   -> DeleteBucketPolicy
deleteBucketPolicy p1 = DeleteBucketPolicy
    { dbprBucket = p1
    }

data DeleteBucketPolicy = DeleteBucketPolicy
    { dbprBucket :: BucketName
    } deriving (Eq, Show, Generic)

instance ToPath DeleteBucketPolicy where
    toPath DeleteBucketPolicy{..} = mconcat
        [ "/"
        , toBS dbprBucket
        ]

instance ToQuery DeleteBucketPolicy

instance ToHeaders DeleteBucketPolicy

instance ToBody DeleteBucketPolicy

instance AWSRequest DeleteBucketPolicy where
    type Sv DeleteBucketPolicy = S3

    request  = delete
fromList [("payload",Null),("name",String "DeleteBucketPolicyResponse"),("shape",Object fromList [("streaming",Bool False),("location",String "body"),("pattern",Null),("required",Bool False),("min_length",Number 0.0),("max_length",Number 0.0),("name",Null),("documentation",Null),("common",Object fromList [("streaming",Bool False),("location",String "body"),("required",Bool False),("name",Null),("documentation",Null),("location_name",Null),("xml_name",Null)]),("location_name",Null),("type",String "Text"),("xml_name",Null)]),("fields",Array (fromList []))]

data instance Rs DeleteBucketPolicy = DeleteBucketPolicyResponse
    deriving (Eq, Show, Generic)
