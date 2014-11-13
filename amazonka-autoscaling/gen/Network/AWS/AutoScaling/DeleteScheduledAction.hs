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
      DeleteScheduledAction
    -- ** Request constructor
    , deleteScheduledAction
    -- ** Request lenses
    , dsaAutoScalingGroupName
    , dsaScheduledActionName

    -- * Response
    , DeleteScheduledActionResponse
    -- ** Response constructor
    , deleteScheduledActionResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data DeleteScheduledAction = DeleteScheduledAction
    { _dsaAutoScalingGroupName :: Maybe Text
    , _dsaScheduledActionName  :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteScheduledAction' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsaAutoScalingGroupName' @::@ 'Maybe' 'Text'
--
-- * 'dsaScheduledActionName' @::@ 'Text'
--
deleteScheduledAction :: Text -- ^ 'dsaScheduledActionName'
                      -> DeleteScheduledAction
deleteScheduledAction p1 = DeleteScheduledAction
    { _dsaScheduledActionName  = p1
    , _dsaAutoScalingGroupName = Nothing
    }

-- | The name of the Auto Scaling group.
dsaAutoScalingGroupName :: Lens' DeleteScheduledAction (Maybe Text)
dsaAutoScalingGroupName =
    lens _dsaAutoScalingGroupName (\s a -> s { _dsaAutoScalingGroupName = a })

-- | The name of the action you want to delete.
dsaScheduledActionName :: Lens' DeleteScheduledAction Text
dsaScheduledActionName =
    lens _dsaScheduledActionName (\s a -> s { _dsaScheduledActionName = a })

instance ToQuery DeleteScheduledAction

instance ToPath DeleteScheduledAction where
    toPath = const "/"

data DeleteScheduledActionResponse = DeleteScheduledActionResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteScheduledActionResponse' constructor.
deleteScheduledActionResponse :: DeleteScheduledActionResponse
deleteScheduledActionResponse = DeleteScheduledActionResponse

instance FromXML DeleteScheduledActionResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteScheduledActionResponse"

instance AWSRequest DeleteScheduledAction where
    type Sv DeleteScheduledAction = AutoScaling
    type Rs DeleteScheduledAction = DeleteScheduledActionResponse

    request  = post "DeleteScheduledAction"
    response = nullaryResponse DeleteScheduledActionResponse
