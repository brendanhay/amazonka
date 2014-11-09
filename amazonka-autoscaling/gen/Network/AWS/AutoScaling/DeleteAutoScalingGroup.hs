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

-- Module      : Network.AWS.AutoScaling.DeleteAutoScalingGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified Auto Scaling group if the group has no instances and
-- no scaling activities in progress.
module Network.AWS.AutoScaling.DeleteAutoScalingGroup
    (
    -- * Request
      DeleteAutoScalingGroupType
    -- ** Request constructor
    , deleteAutoScalingGroupType
    -- ** Request lenses
    , dasgtAutoScalingGroupName
    , dasgtForceDelete

    -- * Response
    , DeleteAutoScalingGroupResponse
    -- ** Response constructor
    , deleteAutoScalingGroupResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data DeleteAutoScalingGroupType = DeleteAutoScalingGroupType
    { _dasgtAutoScalingGroupName :: Text
    , _dasgtForceDelete          :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteAutoScalingGroupType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dasgtAutoScalingGroupName' @::@ 'Text'
--
-- * 'dasgtForceDelete' @::@ 'Maybe' 'Bool'
--
deleteAutoScalingGroupType :: Text -- ^ 'dasgtAutoScalingGroupName'
                           -> DeleteAutoScalingGroupType
deleteAutoScalingGroupType p1 = DeleteAutoScalingGroupType
    { _dasgtAutoScalingGroupName = p1
    , _dasgtForceDelete          = Nothing
    }

-- | The name of the Auto Scaling group to delete.
dasgtAutoScalingGroupName :: Lens' DeleteAutoScalingGroupType Text
dasgtAutoScalingGroupName =
    lens _dasgtAutoScalingGroupName
        (\s a -> s { _dasgtAutoScalingGroupName = a })

-- | Starting with API version 2011-01-01, specifies that the Auto Scaling
-- group will be deleted along with all instances associated with the group,
-- without waiting for all instances to be terminated. This parameter also
-- deletes any lifecycle actions associated with the group.
dasgtForceDelete :: Lens' DeleteAutoScalingGroupType (Maybe Bool)
dasgtForceDelete = lens _dasgtForceDelete (\s a -> s { _dasgtForceDelete = a })

instance ToPath DeleteAutoScalingGroupType where
    toPath = const "/"

instance ToQuery DeleteAutoScalingGroupType

data DeleteAutoScalingGroupResponse = DeleteAutoScalingGroupResponse

-- | 'DeleteAutoScalingGroupResponse' constructor.
deleteAutoScalingGroupResponse :: DeleteAutoScalingGroupResponse
deleteAutoScalingGroupResponse = DeleteAutoScalingGroupResponse

instance AWSRequest DeleteAutoScalingGroupType where
    type Sv DeleteAutoScalingGroupType = AutoScaling
    type Rs DeleteAutoScalingGroupType = DeleteAutoScalingGroupResponse

    request  = post "DeleteAutoScalingGroup"
    response = const (nullaryResponse DeleteAutoScalingGroupResponse)
