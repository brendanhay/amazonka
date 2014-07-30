{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.AddTagsToResource
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds metadata tags to an Amazon RDS resource. These tags can also be used
-- with cost allocation reporting to track cost associated with Amazon RDS
-- resources, or used in Condition statement in IAM policy for Amazon RDS. For
-- an overview on tagging Amazon RDS resources, see Tagging Amazon RDS
-- Resources.
module Network.AWS.RDS.V2013_09_09.AddTagsToResource where

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
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.RDS.V2013_09_09.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data AddTagsToResource = AddTagsToResource
    { _attrmResourceName :: Text
      -- ^ The Amazon RDS resource the tags will be added to. This value is
      -- an Amazon Resource Name (ARN). For information about creating an
      -- ARN, see Constructing an RDS Amazon Resource Name (ARN).
    , _attrmTags :: [Tag]
      -- ^ The tags to be assigned to the Amazon RDS resource.
    } deriving (Generic)

instance ToQuery AddTagsToResource where
    toQuery = genericToQuery def

instance AWSRequest AddTagsToResource where
    type Sv AddTagsToResource = RDS
    type Rs AddTagsToResource = AddTagsToResourceResponse

    request = post "AddTagsToResource"
    response _ _ = return (Right AddTagsToResourceResponse)

data AddTagsToResourceResponse = AddTagsToResourceResponse
    deriving (Eq, Show, Generic)
