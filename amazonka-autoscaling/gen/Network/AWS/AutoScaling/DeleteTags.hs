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

-- Module      : Network.AWS.AutoScaling.DeleteTags
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes the specified tags or a set of tags from a set of resources.
module Network.AWS.AutoScaling.DeleteTags
    (
    -- * Request
      DeleteTags
    -- ** Request constructor
    , deleteTags
    -- ** Request lenses
    , dtTags

    -- * Response
    , DeleteTagsResponse
    -- ** Response constructor
    , deleteTagsResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

newtype DeleteTags = DeleteTags
    { _dtTags :: [Tag]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance IsList DeleteTags where
    type Item DeleteTags = Tag

    fromList = DeleteTags . fromList
    toList   = toList . _dtTags

-- | 'DeleteTags' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtTags' @::@ ['Tag']
--
deleteTags :: DeleteTags
deleteTags = DeleteTags
    { _dtTags = mempty
    }

-- | Each tag should be defined by its resource type, resource ID, key, value,
-- and a propagate flag. Valid values are: Resource type =
-- auto-scaling-group, Resource ID = AutoScalingGroupName, key=value,
-- value=value, propagate=true or false.
dtTags :: Lens' DeleteTags [Tag]
dtTags = lens _dtTags (\s a -> s { _dtTags = a })

instance ToQuery DeleteTags

instance ToPath DeleteTags where
    toPath = const "/"

data DeleteTagsResponse = DeleteTagsResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteTagsResponse' constructor.
deleteTagsResponse :: DeleteTagsResponse
deleteTagsResponse = DeleteTagsResponse

instance FromXML DeleteTagsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteTagsResponse"

instance AWSRequest DeleteTags where
    type Sv DeleteTags = AutoScaling
    type Rs DeleteTags = DeleteTagsResponse

    request  = post "DeleteTags"
    response = nullaryResponse DeleteTagsResponse
