{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DeleteScheduledAction
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified scheduled action.
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

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteScheduledAction' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsaAutoScalingGroupName'
--
-- * 'dsaScheduledActionName'
data DeleteScheduledAction = DeleteScheduledAction'
    { _dsaAutoScalingGroupName :: !(Maybe Text)
    , _dsaScheduledActionName  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteScheduledAction' smart constructor.
deleteScheduledAction :: Text -> DeleteScheduledAction
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
               "AutoScalingGroupName" =: _dsaAutoScalingGroupName,
               "ScheduledActionName" =: _dsaScheduledActionName]

-- | /See:/ 'deleteScheduledActionResponse' smart constructor.
data DeleteScheduledActionResponse =
    DeleteScheduledActionResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteScheduledActionResponse' smart constructor.
deleteScheduledActionResponse :: DeleteScheduledActionResponse
deleteScheduledActionResponse = DeleteScheduledActionResponse'
