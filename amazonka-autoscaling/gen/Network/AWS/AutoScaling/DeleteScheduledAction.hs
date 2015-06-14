{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.AutoScaling.DeleteScheduledAction
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes the specified scheduled action.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DeleteScheduledAction.html>
module Network.AWS.AutoScaling.DeleteScheduledAction
    (
    -- * Request
      DeleteScheduledAction
    -- ** Request constructor
    , deleteScheduledAction
    -- ** Request lenses
    , dsaScheduledActionName
    , dsaAutoScalingGroupName

    -- * Response
    , DeleteScheduledActionResponse
    -- ** Response constructor
    , deleteScheduledActionResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.AutoScaling.Types

-- | /See:/ 'deleteScheduledAction' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsaScheduledActionName'
--
-- * 'dsaAutoScalingGroupName'
data DeleteScheduledAction = DeleteScheduledAction'{_dsaScheduledActionName :: Text, _dsaAutoScalingGroupName :: Text} deriving (Eq, Read, Show)

-- | 'DeleteScheduledAction' smart constructor.
deleteScheduledAction :: Text -> Text -> DeleteScheduledAction
deleteScheduledAction pScheduledActionName pAutoScalingGroupName = DeleteScheduledAction'{_dsaScheduledActionName = pScheduledActionName, _dsaAutoScalingGroupName = pAutoScalingGroupName};

-- | The name of the action to delete.
dsaScheduledActionName :: Lens' DeleteScheduledAction Text
dsaScheduledActionName = lens _dsaScheduledActionName (\ s a -> s{_dsaScheduledActionName = a});

-- | The name of the Auto Scaling group.
dsaAutoScalingGroupName :: Lens' DeleteScheduledAction Text
dsaAutoScalingGroupName = lens _dsaAutoScalingGroupName (\ s a -> s{_dsaAutoScalingGroupName = a});

instance AWSRequest DeleteScheduledAction where
        type Sv DeleteScheduledAction = AutoScaling
        type Rs DeleteScheduledAction =
             DeleteScheduledActionResponse
        request = post
        response = receiveNull DeleteScheduledActionResponse'

instance ToHeaders DeleteScheduledAction where
        toHeaders = const mempty

instance ToPath DeleteScheduledAction where
        toPath = const "/"

instance ToQuery DeleteScheduledAction where
        toQuery DeleteScheduledAction'{..}
          = mconcat
              ["Action" =: ("DeleteScheduledAction" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "ScheduledActionName" =: _dsaScheduledActionName,
               "AutoScalingGroupName" =: _dsaAutoScalingGroupName]

-- | /See:/ 'deleteScheduledActionResponse' smart constructor.
data DeleteScheduledActionResponse = DeleteScheduledActionResponse' deriving (Eq, Read, Show)

-- | 'DeleteScheduledActionResponse' smart constructor.
deleteScheduledActionResponse :: DeleteScheduledActionResponse
deleteScheduledActionResponse = DeleteScheduledActionResponse';
