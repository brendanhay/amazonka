{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
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
module Network.AWS.RDS.V2013_09_09.AddTagsToResource
    (
    -- * Request
      AddTagsToResource
    -- ** Request constructor
    , addTagsToResource
    -- ** Request lenses
    , attrmResourceName
    , attrmTags

    -- * Response
    , AddTagsToResourceResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'AddTagsToResource' request.
addTagsToResource :: Text -- ^ 'attrmResourceName'
                  -> [Tag] -- ^ 'attrmTags'
                  -> AddTagsToResource
addTagsToResource p1 p2 = AddTagsToResource
    { _attrmResourceName = p1
    , _attrmTags = p2
    }
{-# INLINE addTagsToResource #-}

data AddTagsToResource = AddTagsToResource
    { _attrmResourceName :: Text
      -- ^ The Amazon RDS resource the tags will be added to. This value is
      -- an Amazon Resource Name (ARN). For information about creating an
      -- ARN, see Constructing an RDS Amazon Resource Name (ARN).
    , _attrmTags :: [Tag]
      -- ^ The tags to be assigned to the Amazon RDS resource.
    } deriving (Show, Generic)

-- | The Amazon RDS resource the tags will be added to. This value is an Amazon
-- Resource Name (ARN). For information about creating an ARN, see
-- Constructing an RDS Amazon Resource Name (ARN).
attrmResourceName :: Lens' AddTagsToResource (Text)
attrmResourceName f x =
    f (_attrmResourceName x)
        <&> \y -> x { _attrmResourceName = y }
{-# INLINE attrmResourceName #-}

-- | The tags to be assigned to the Amazon RDS resource.
attrmTags :: Lens' AddTagsToResource ([Tag])
attrmTags f x =
    f (_attrmTags x)
        <&> \y -> x { _attrmTags = y }
{-# INLINE attrmTags #-}

instance ToQuery AddTagsToResource where
    toQuery = genericQuery def

data AddTagsToResourceResponse = AddTagsToResourceResponse
    deriving (Eq, Show, Generic)

instance AWSRequest AddTagsToResource where
    type Sv AddTagsToResource = RDS
    type Rs AddTagsToResource = AddTagsToResourceResponse

    request = post "AddTagsToResource"
    response _ = nullaryResponse AddTagsToResourceResponse
