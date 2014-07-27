{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_05_01.CreateTags
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds or overwrites one or more tags for the specified EC2 resource or
-- resources. Each resource can have a maximum of 10 tags. Each tag consists
-- of a key and optional value. Tag keys must be unique per resource. For more
-- information about tags, see Tagging Your Resources in the Amazon Elastic
-- Compute Cloud User Guide. Example This example request adds (or overwrites)
-- two tags for an AMI and an instance. One of the tags is just a key
-- (webserver), with no value (we set the value to an empty string). The other
-- tag consists of a key (stack) and value (Production).
-- https://ec2.amazonaws.com/?Action=CreateTags &amp;ResourceId.1=ami-1a2b3c4d
-- &amp;ResourceId.2=i-7f4d3a2b &amp;Tag.1.Key=webserver &amp;Tag.1.Value=
-- &amp;Tag.2.Key=stack &amp;Tag.2.Value=Production &amp;AUTHPARAMS
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/">
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE true.
module Network.AWS.EC2.V2014_05_01.CreateTags where

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
import           Network.AWS.Types    hiding (Region, Error)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_05_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data CreateTags = CreateTags
    { _ctrResources :: [Text]
      -- ^ The IDs of one or more resources to tag. For example,
      -- ami-1a2b3c4d.
    , _ctrTags :: [Tag]
      -- ^ One or more tags. The value parameter is required, but if you
      -- don't want the tag to have a value, specify the parameter with no
      -- value, and we set the value to an empty string.
    , _ctrDryRun :: Maybe Bool
      -- ^ 
    } deriving (Generic)

instance ToQuery CreateTags where
    toQuery = genericToQuery def

instance AWSRequest CreateTags where
    type Sv CreateTags = EC2
    type Rs CreateTags = CreateTagsResponse

    request = post "CreateTags"
    response _ _ = return (Right CreateTagsResponse)

data CreateTagsResponse = CreateTagsResponse
    deriving (Eq, Show, Generic)
