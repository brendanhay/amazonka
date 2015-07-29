{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DeleteAutoScalingGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Auto Scaling group.
--
-- The group must have no instances and no scaling activities in progress.
--
-- To remove all instances before calling @DeleteAutoScalingGroup@, call
-- UpdateAutoScalingGroup to set the minimum and maximum size of the Auto
-- Scaling group to zero.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DeleteAutoScalingGroup.html>
module Network.AWS.AutoScaling.DeleteAutoScalingGroup
    (
    -- * Request
      DeleteAutoScalingGroup
    -- ** Request constructor
    , deleteAutoScalingGroup
    -- ** Request lenses
    , dasgForceDelete
    , dasgAutoScalingGroupName

    -- * Response
    , DeleteAutoScalingGroupResponse
    -- ** Response constructor
    , deleteAutoScalingGroupResponse
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteAutoScalingGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dasgForceDelete'
--
-- * 'dasgAutoScalingGroupName'
data DeleteAutoScalingGroup = DeleteAutoScalingGroup'
    { _dasgForceDelete          :: !(Maybe Bool)
    , _dasgAutoScalingGroupName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteAutoScalingGroup' smart constructor.
deleteAutoScalingGroup :: Text -> DeleteAutoScalingGroup
deleteAutoScalingGroup pAutoScalingGroupName_ =
    DeleteAutoScalingGroup'
    { _dasgForceDelete = Nothing
    , _dasgAutoScalingGroupName = pAutoScalingGroupName_
    }

-- | Specifies that the group will be deleted along with all instances
-- associated with the group, without waiting for all instances to be
-- terminated. This parameter also deletes any lifecycle actions associated
-- with the group.
dasgForceDelete :: Lens' DeleteAutoScalingGroup (Maybe Bool)
dasgForceDelete = lens _dasgForceDelete (\ s a -> s{_dasgForceDelete = a});

-- | The name of the group to delete.
dasgAutoScalingGroupName :: Lens' DeleteAutoScalingGroup Text
dasgAutoScalingGroupName = lens _dasgAutoScalingGroupName (\ s a -> s{_dasgAutoScalingGroupName = a});

instance AWSRequest DeleteAutoScalingGroup where
        type Sv DeleteAutoScalingGroup = AutoScaling
        type Rs DeleteAutoScalingGroup =
             DeleteAutoScalingGroupResponse
        request = postQuery
        response
          = receiveNull DeleteAutoScalingGroupResponse'

instance ToHeaders DeleteAutoScalingGroup where
        toHeaders = const mempty

instance ToPath DeleteAutoScalingGroup where
        toPath = const mempty

instance ToQuery DeleteAutoScalingGroup where
        toQuery DeleteAutoScalingGroup'{..}
          = mconcat
              ["Action" =:
                 ("DeleteAutoScalingGroup" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "ForceDelete" =: _dasgForceDelete,
               "AutoScalingGroupName" =: _dasgAutoScalingGroupName]

-- | /See:/ 'deleteAutoScalingGroupResponse' smart constructor.
data DeleteAutoScalingGroupResponse =
    DeleteAutoScalingGroupResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteAutoScalingGroupResponse' smart constructor.
deleteAutoScalingGroupResponse :: DeleteAutoScalingGroupResponse
deleteAutoScalingGroupResponse = DeleteAutoScalingGroupResponse'
