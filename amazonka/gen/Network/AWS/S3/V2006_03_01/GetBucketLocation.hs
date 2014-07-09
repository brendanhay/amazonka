{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.GetBucketLocation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the region the bucket resides in.
module Network.AWS.S3.V2006_03_01.GetBucketLocation where

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

-- | Default GetBucketLocation request.
getBucketLocation :: BucketName -- ^ 'gblrBucket'
                  -> GetBucketLocation
getBucketLocation p1 = GetBucketLocation
    { gblrBucket = p1
    }

data GetBucketLocation = GetBucketLocation
    { gblrBucket :: BucketName
    } deriving (Eq, Show, Generic)

instance ToPath GetBucketLocation where
    toPath GetBucketLocation{..} = mconcat
        [ "/"
        , toBS gblrBucket
        ]

instance ToQuery GetBucketLocation

instance ToHeaders GetBucketLocation

instance ToBody GetBucketLocation

instance AWSRequest GetBucketLocation where
    type Sv GetBucketLocation = S3

    request  = get
    response = xmlResponse

data instance Rs GetBucketLocation = GetBucketLocationResponse
    { gbloLocationConstraint :: Maybe BucketLocationConstraint
    } deriving (Eq, Show, Generic)
