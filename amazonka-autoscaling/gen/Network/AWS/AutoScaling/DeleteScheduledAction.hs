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

-- Module      : Network.AWS.AutoScaling.DeleteScheduledAction
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a scheduled action previously created using the
-- PutScheduledUpdateGroupAction.
module Network.AWS.AutoScaling.DeleteScheduledAction
    (
    -- * Request
      DeleteScheduledActionType
    -- ** Request constructor
    , deleteScheduledActionType
    -- ** Request lenses
    , dsat1AutoScalingGroupName
    , dsat1ScheduledActionName

    -- * Response
    , DeleteScheduledActionResponse
    -- ** Response constructor
    , deleteScheduledActionResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data DeleteScheduledActionType = DeleteScheduledActionType
    { _dsat1AutoScalingGroupName :: Maybe Text
    , _dsat1ScheduledActionName  :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteScheduledActionType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsat1AutoScalingGroupName' @::@ 'Maybe' 'Text'
--
-- * 'dsat1ScheduledActionName' @::@ 'Text'
--
deleteScheduledActionType :: Text -- ^ 'dsat1ScheduledActionName'
                          -> DeleteScheduledActionType
deleteScheduledActionType p1 = DeleteScheduledActionType
    { _dsat1ScheduledActionName  = p1
    , _dsat1AutoScalingGroupName = Nothing
    }

-- | The name of the Auto Scaling group.
dsat1AutoScalingGroupName :: Lens' DeleteScheduledActionType (Maybe Text)
dsat1AutoScalingGroupName =
    lens _dsat1AutoScalingGroupName
        (\s a -> s { _dsat1AutoScalingGroupName = a })

-- | The name of the action you want to delete.
dsat1ScheduledActionName :: Lens' DeleteScheduledActionType Text
dsat1ScheduledActionName =
    lens _dsat1ScheduledActionName
        (\s a -> s { _dsat1ScheduledActionName = a })

instance ToQuery DeleteScheduledActionType

instance ToPath DeleteScheduledActionType where
    toPath = const "/"

data DeleteScheduledActionResponse = DeleteScheduledActionResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteScheduledActionResponse' constructor.
deleteScheduledActionResponse :: DeleteScheduledActionResponse
deleteScheduledActionResponse = DeleteScheduledActionResponse

instance FromXML DeleteScheduledActionResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteScheduledActionResponse"

instance AWSRequest DeleteScheduledActionType where
    type Sv DeleteScheduledActionType = AutoScaling
    type Rs DeleteScheduledActionType = DeleteScheduledActionResponse

    request  = post "DeleteScheduledAction"
    response = nullaryResponse DeleteScheduledActionResponse
