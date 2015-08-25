{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DeleteScheduledAction
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified scheduled action.
--
-- /See:/ <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DeleteScheduledAction.html AWS API Reference> for DeleteScheduledAction.
module Network.AWS.AutoScaling.DeleteScheduledAction
    (
    -- * Creating a Request
      deleteScheduledAction
    , DeleteScheduledAction
    -- * Request Lenses
    , dsaAutoScalingGroupName
    , dsaScheduledActionName

    -- * Destructuring the Response
    , deleteScheduledActionResponse
    , DeleteScheduledActionResponse
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.AutoScaling.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteScheduledAction' smart constructor.
data DeleteScheduledAction = DeleteScheduledAction'
    { _dsaAutoScalingGroupName :: !(Maybe Text)
    , _dsaScheduledActionName  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteScheduledAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsaAutoScalingGroupName'
--
-- * 'dsaScheduledActionName'
deleteScheduledAction
    :: Text -- ^ 'dsaScheduledActionName'
    -> DeleteScheduledAction
deleteScheduledAction pScheduledActionName_ =
    DeleteScheduledAction'
    { _dsaAutoScalingGroupName = Nothing
    , _dsaScheduledActionName = pScheduledActionName_
    }

-- | The name of the Auto Scaling group.
dsaAutoScalingGroupName :: Lens' DeleteScheduledAction (Maybe Text)
dsaAutoScalingGroupName = lens _dsaAutoScalingGroupName (\ s a -> s{_dsaAutoScalingGroupName = a});

-- | The name of the action to delete.
dsaScheduledActionName :: Lens' DeleteScheduledAction Text
dsaScheduledActionName = lens _dsaScheduledActionName (\ s a -> s{_dsaScheduledActionName = a});

instance AWSRequest DeleteScheduledAction where
        type Rs DeleteScheduledAction =
             DeleteScheduledActionResponse
        request = postQuery autoScaling
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
               "AutoScalingGroupName" =: _dsaAutoScalingGroupName,
               "ScheduledActionName" =: _dsaScheduledActionName]

-- | /See:/ 'deleteScheduledActionResponse' smart constructor.
data DeleteScheduledActionResponse =
    DeleteScheduledActionResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteScheduledActionResponse' with the minimum fields required to make a request.
--
deleteScheduledActionResponse
    :: DeleteScheduledActionResponse
deleteScheduledActionResponse = DeleteScheduledActionResponse'
