{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

data AddTagsToResource = AddTagsToResource
    { _attrmResourceName :: Text
      -- ^ The Amazon RDS resource the tags will be added to. This value is
      -- an Amazon Resource Name (ARN). For information about creating an
      -- ARN, see Constructing an RDS Amazon Resource Name (ARN).
    , _attrmTags :: [Tag]
      -- ^ The tags to be assigned to the Amazon RDS resource.
    } deriving (Show, Generic)

makeLenses ''AddTagsToResource

instance ToQuery AddTagsToResource where
    toQuery = genericQuery def

data AddTagsToResourceResponse = AddTagsToResourceResponse
    deriving (Eq, Show, Generic)

makeLenses ''AddTagsToResourceResponse

instance AWSRequest AddTagsToResource where
    type Sv AddTagsToResource = RDS
    type Rs AddTagsToResource = AddTagsToResourceResponse

    request = post "AddTagsToResource"
    response _ = nullaryResponse AddTagsToResourceResponse
