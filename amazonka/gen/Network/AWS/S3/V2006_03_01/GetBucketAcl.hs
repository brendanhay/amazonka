{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.GetBucketAcl
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets the access control policy for the bucket.
module Network.AWS.S3.V2006_03_01.GetBucketAcl where

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
-- specify the minimum viable GetBucketAcl request.
getBucketAcl :: BucketName -- ^ 'gbarBucket'
             -> GetBucketAcl
getBucketAcl p1 = GetBucketAcl
    { gbarBucket = p1
    }

data GetBucketAcl = GetBucketAcl
    { gbarBucket :: BucketName
    } deriving (Eq, Show, Generic)

instance ToPath GetBucketAcl where
    toPath GetBucketAcl{..} = mconcat
        [ "/"
        , toBS gbarBucket
        ]

instance ToQuery GetBucketAcl

instance ToHeaders GetBucketAcl

instance ToBody GetBucketAcl

instance AWSRequest GetBucketAcl where
    type Sv GetBucketAcl = S3

    request  = get
    response = response' undefined

data instance Rs GetBucketAcl = GetBucketAclResponse
    { gbaoGrants :: [Grant]
      -- ^ A list of grants.
    , gbaoOwner :: Maybe Owner
    } deriving (Eq, Show, Generic)
