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
      DeleteTagsType
    -- ** Request constructor
    , deleteTagsType
    -- ** Request lenses
    , dttTags

    -- * Response
    , DeleteTagsResponse
    -- ** Response constructor
    , deleteTagsResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

newtype DeleteTagsType = DeleteTagsType
    { _dttTags :: [Tag]
    } deriving (Eq, Show, Generic, Monoid)

-- | 'DeleteTagsType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dttTags' @::@ ['Tag']
--
deleteTagsType :: DeleteTagsType
deleteTagsType = DeleteTagsType
    { _dttTags = mempty
    }

-- | Each tag should be defined by its resource type, resource ID, key, value,
-- and a propagate flag. Valid values are: Resource type =
-- auto-scaling-group, Resource ID = AutoScalingGroupName, key=value,
-- value=value, propagate=true or false.
dttTags :: Lens' DeleteTagsType [Tag]
dttTags = lens _dttTags (\s a -> s { _dttTags = a })

instance ToPath DeleteTagsType where
    toPath = const "/"

instance ToQuery DeleteTagsType

data DeleteTagsResponse = DeleteTagsResponse

-- | 'DeleteTagsResponse' constructor.
deleteTagsResponse :: DeleteTagsResponse
deleteTagsResponse = DeleteTagsResponse

instance AWSRequest DeleteTagsType where
    type Sv DeleteTagsType = AutoScaling
    type Rs DeleteTagsType = DeleteTagsResponse

    request  = post "DeleteTags"
    response = const (nullaryResponse DeleteTagsResponse)
