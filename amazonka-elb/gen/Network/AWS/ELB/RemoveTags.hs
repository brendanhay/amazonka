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

-- Module      : Network.AWS.ELB.RemoveTags
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes one or more tags from the specified load balancer.
module Network.AWS.ELB.RemoveTags
    (
    -- * Request
      RemoveTagsInput
    -- ** Request constructor
    , removeTagsInput
    -- ** Request lenses
    , rtiLoadBalancerNames
    , rtiTags

    -- * Response
    , RemoveTagsResponse
    -- ** Response constructor
    , removeTagsResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types

data RemoveTagsInput = RemoveTagsInput
    { _rtiLoadBalancerNames :: [Text]
    , _rtiTags              :: List1 TagKeyOnly
    } deriving (Eq, Show, Generic)

-- | 'RemoveTagsInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtiLoadBalancerNames' @::@ ['Text']
--
-- * 'rtiTags' @::@ 'NonEmpty' 'TagKeyOnly'
--
removeTagsInput :: List1 TagKeyOnly -- ^ 'rtiTags'
                -> RemoveTagsInput
removeTagsInput p1 = RemoveTagsInput
    { _rtiTags              = withIso _List1 (const id) p1
    , _rtiLoadBalancerNames = mempty
    }

-- | The name of the load balancer. You can specify a maximum of one load
-- balancer name.
rtiLoadBalancerNames :: Lens' RemoveTagsInput [Text]
rtiLoadBalancerNames =
    lens _rtiLoadBalancerNames (\s a -> s { _rtiLoadBalancerNames = a })

-- | A list of tag keys to remove.
rtiTags :: Lens' RemoveTagsInput (NonEmpty TagKeyOnly)
rtiTags = lens _rtiTags (\s a -> s { _rtiTags = a })
    . _List1
instance ToQuery RemoveTagsInput

instance ToPath RemoveTagsInput where
    toPath = const "/"

data RemoveTagsResponse = RemoveTagsResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'RemoveTagsResponse' constructor.
removeTagsResponse :: RemoveTagsResponse
removeTagsResponse = RemoveTagsResponse
instance FromXML RemoveTagsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RemoveTagsResponse"

instance AWSRequest RemoveTagsInput where
    type Sv RemoveTagsInput = ELB
    type Rs RemoveTagsInput = RemoveTagsResponse

    request  = post "RemoveTags"
    response = nullaryResponse RemoveTagsResponse
