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
-- Module      : Network.AWS.AutoScaling.DeleteAutoScalingGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Auto Scaling group.
--
--
-- If the group has instances or scaling activities in progress, you must specify the option to force the deletion in order for it to succeed.
--
-- If the group has policies, deleting the group deletes the policies, the underlying alarm actions, and any alarm that no longer has an associated action.
--
-- To remove instances from the Auto Scaling group before deleting it, call 'DetachInstances' with the list of instances and the option to decrement the desired capacity so that Auto Scaling does not launch replacement instances.
--
-- To terminate all instances before deleting the Auto Scaling group, call 'UpdateAutoScalingGroup' and set the minimum size and desired capacity of the Auto Scaling group to zero.
--
module Network.AWS.AutoScaling.DeleteAutoScalingGroup
    (
    -- * Creating a Request
      deleteAutoScalingGroup
    , DeleteAutoScalingGroup
    -- * Request Lenses
    , dasgForceDelete
    , dasgAutoScalingGroupName

    -- * Destructuring the Response
    , deleteAutoScalingGroupResponse
    , DeleteAutoScalingGroupResponse
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteAutoScalingGroup' smart constructor.
data DeleteAutoScalingGroup = DeleteAutoScalingGroup'
  { _dasgForceDelete          :: !(Maybe Bool)
  , _dasgAutoScalingGroupName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAutoScalingGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dasgForceDelete' - Specifies that the group will be deleted along with all instances associated with the group, without waiting for all instances to be terminated. This parameter also deletes any lifecycle actions associated with the group.
--
-- * 'dasgAutoScalingGroupName' - The name of the Auto Scaling group.
deleteAutoScalingGroup
    :: Text -- ^ 'dasgAutoScalingGroupName'
    -> DeleteAutoScalingGroup
deleteAutoScalingGroup pAutoScalingGroupName_ =
  DeleteAutoScalingGroup'
    { _dasgForceDelete = Nothing
    , _dasgAutoScalingGroupName = pAutoScalingGroupName_
    }


-- | Specifies that the group will be deleted along with all instances associated with the group, without waiting for all instances to be terminated. This parameter also deletes any lifecycle actions associated with the group.
dasgForceDelete :: Lens' DeleteAutoScalingGroup (Maybe Bool)
dasgForceDelete = lens _dasgForceDelete (\ s a -> s{_dasgForceDelete = a})

-- | The name of the Auto Scaling group.
dasgAutoScalingGroupName :: Lens' DeleteAutoScalingGroup Text
dasgAutoScalingGroupName = lens _dasgAutoScalingGroupName (\ s a -> s{_dasgAutoScalingGroupName = a})

instance AWSRequest DeleteAutoScalingGroup where
        type Rs DeleteAutoScalingGroup =
             DeleteAutoScalingGroupResponse
        request = postQuery autoScaling
        response
          = receiveNull DeleteAutoScalingGroupResponse'

instance Hashable DeleteAutoScalingGroup where

instance NFData DeleteAutoScalingGroup where

instance ToHeaders DeleteAutoScalingGroup where
        toHeaders = const mempty

instance ToPath DeleteAutoScalingGroup where
        toPath = const "/"

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAutoScalingGroupResponse' with the minimum fields required to make a request.
--
deleteAutoScalingGroupResponse
    :: DeleteAutoScalingGroupResponse
deleteAutoScalingGroupResponse = DeleteAutoScalingGroupResponse'


instance NFData DeleteAutoScalingGroupResponse where
