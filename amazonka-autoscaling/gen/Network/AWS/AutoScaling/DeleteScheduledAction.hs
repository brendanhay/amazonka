{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DeleteScheduledAction.html>
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
import qualified GHC.Exts

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

data DeleteScheduledActionResponse = DeleteScheduledActionResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteScheduledActionResponse' constructor.
deleteScheduledActionResponse :: DeleteScheduledActionResponse
deleteScheduledActionResponse = DeleteScheduledActionResponse

instance ToPath DeleteScheduledAction where
    toPath = const "/"

instance ToQuery DeleteScheduledAction

instance ToHeaders DeleteScheduledAction

instance AWSRequest DeleteScheduledAction where
    type Sv DeleteScheduledAction = AutoScaling
    type Rs DeleteScheduledAction = DeleteScheduledActionResponse

    request  = post "DeleteScheduledAction"
    response = nullResponse DeleteScheduledActionResponse
