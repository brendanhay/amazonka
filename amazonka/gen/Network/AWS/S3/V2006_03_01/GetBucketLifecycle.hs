{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.GetBucketLifecycle
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the lifecycle configuration information set on the bucket.
module Network.AWS.S3.V2006_03_01.GetBucketLifecycle where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error)
import           Network.AWS.Request.RestS3
import           Network.AWS.S3.V2006_03_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data GetBucketLifecycle = GetBucketLifecycle
    { _gblsBucket :: BucketName
    } deriving (Generic)

instance ToPath GetBucketLifecycle where
    toPath GetBucketLifecycle{..} = mconcat
        [ "/"
        , toBS _gblsBucket
        ]

instance ToQuery GetBucketLifecycle

instance ToHeaders GetBucketLifecycle

instance ToBody GetBucketLifecycle

instance AWSRequest GetBucketLifecycle where
    type Sv GetBucketLifecycle = S3
    type Rs GetBucketLifecycle = GetBucketLifecycleResponse

    request = get
    response _ = xmlResponse

data GetBucketLifecycleResponse = GetBucketLifecycleResponse
    { _gblpRules :: [Rule]
    } deriving (Generic)

instance FromXML GetBucketLifecycleResponse where
    fromXMLOptions = xmlOptions
