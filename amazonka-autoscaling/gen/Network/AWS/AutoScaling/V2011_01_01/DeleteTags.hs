{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.DeleteTags
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes the specified tags or a set of tags from a set of resources.
module Network.AWS.AutoScaling.V2011_01_01.DeleteTags where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

data DeleteTags = DeleteTags
    { _dtuTags :: [Tag]
      -- ^ Each tag should be defined by its resource type, resource ID,
      -- key, value, and a propagate flag. Valid values are: Resource type
      -- = auto-scaling-group, Resource ID = AutoScalingGroupName,
      -- key=value, value=value, propagate=true or false.
    } deriving (Show, Generic)

makeLenses ''DeleteTags

instance ToQuery DeleteTags where
    toQuery = genericQuery def

data DeleteTagsResponse = DeleteTagsResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteTagsResponse

instance AWSRequest DeleteTags where
    type Sv DeleteTags = AutoScaling
    type Rs DeleteTags = DeleteTagsResponse

    request = post "DeleteTags"
    response _ = nullaryResponse DeleteTagsResponse
