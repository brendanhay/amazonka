{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.RDS.AddTagsToResource
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
module Network.AWS.RDS.AddTagsToResource
    (
    -- * Request
      AddTagsToResourceMessage
    -- ** Request constructor
    , addTagsToResourceMessage
    -- ** Request lenses
    , attrmResourceName
    , attrmTags

    -- * Response
    , AddTagsToResourceResponse
    -- ** Response constructor
    , addTagsToResourceResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data AddTagsToResourceMessage = AddTagsToResourceMessage
    { _attrmResourceName :: Text
    , _attrmTags         :: [Tag]
    } deriving (Eq, Show, Generic)

-- | 'AddTagsToResourceMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'attrmResourceName' @::@ 'Text'
--
-- * 'attrmTags' @::@ ['Tag']
--
addTagsToResourceMessage :: Text -- ^ 'attrmResourceName'
                         -> AddTagsToResourceMessage
addTagsToResourceMessage p1 = AddTagsToResourceMessage
    { _attrmResourceName = p1
    , _attrmTags         = mempty
    }

-- | The Amazon RDS resource the tags will be added to. This value is an
-- Amazon Resource Name (ARN). For information about creating an ARN, see
-- Constructing an RDS Amazon Resource Name (ARN).
attrmResourceName :: Lens' AddTagsToResourceMessage Text
attrmResourceName =
    lens _attrmResourceName (\s a -> s { _attrmResourceName = a })

-- | The tags to be assigned to the Amazon RDS resource.
attrmTags :: Lens' AddTagsToResourceMessage [Tag]
attrmTags = lens _attrmTags (\s a -> s { _attrmTags = a })

instance ToPath AddTagsToResourceMessage where
    toPath = const "/"

instance ToQuery AddTagsToResourceMessage

data AddTagsToResourceResponse = AddTagsToResourceResponse

-- | 'AddTagsToResourceResponse' constructor.
addTagsToResourceResponse :: AddTagsToResourceResponse
addTagsToResourceResponse = AddTagsToResourceResponse

instance AWSRequest AddTagsToResourceMessage where
    type Sv AddTagsToResourceMessage = RDS
    type Rs AddTagsToResourceMessage = AddTagsToResourceResponse

    request  = post "AddTagsToResource"
    response = const (nullaryResponse AddTagsToResourceResponse)
