{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DeleteLifecycleHook
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified lifecycle hook.
--
-- If there are any outstanding lifecycle actions, they are completed first
-- (@ABANDON@ for launching instances, @CONTINUE@ for terminating
-- instances).
--
-- /See:/ <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DeleteLifecycleHook.html AWS API Reference> for DeleteLifecycleHook.
module Network.AWS.AutoScaling.DeleteLifecycleHook
    (
    -- * Creating a Request
      DeleteLifecycleHook
    , deleteLifecycleHook
    -- * Request Lenses
    , delLifecycleHookName
    , delAutoScalingGroupName

    -- * Destructuring the Response
    , DeleteLifecycleHookResponse
    , deleteLifecycleHookResponse
    -- * Response Lenses
    , drsStatus
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteLifecycleHook' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delLifecycleHookName'
--
-- * 'delAutoScalingGroupName'
data DeleteLifecycleHook = DeleteLifecycleHook'
    { _delLifecycleHookName    :: !Text
    , _delAutoScalingGroupName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteLifecycleHook' smart constructor.
deleteLifecycleHook :: Text -> Text -> DeleteLifecycleHook
deleteLifecycleHook pLifecycleHookName_ pAutoScalingGroupName_ =
    DeleteLifecycleHook'
    { _delLifecycleHookName = pLifecycleHookName_
    , _delAutoScalingGroupName = pAutoScalingGroupName_
    }

-- | The name of the lifecycle hook.
delLifecycleHookName :: Lens' DeleteLifecycleHook Text
delLifecycleHookName = lens _delLifecycleHookName (\ s a -> s{_delLifecycleHookName = a});

-- | The name of the Auto Scaling group for the lifecycle hook.
delAutoScalingGroupName :: Lens' DeleteLifecycleHook Text
delAutoScalingGroupName = lens _delAutoScalingGroupName (\ s a -> s{_delAutoScalingGroupName = a});

instance AWSRequest DeleteLifecycleHook where
        type Sv DeleteLifecycleHook = AutoScaling
        type Rs DeleteLifecycleHook =
             DeleteLifecycleHookResponse
        request = postQuery
        response
          = receiveXMLWrapper "DeleteLifecycleHookResult"
              (\ s h x ->
                 DeleteLifecycleHookResponse' <$> (pure (fromEnum s)))

instance ToHeaders DeleteLifecycleHook where
        toHeaders = const mempty

instance ToPath DeleteLifecycleHook where
        toPath = const "/"

instance ToQuery DeleteLifecycleHook where
        toQuery DeleteLifecycleHook'{..}
          = mconcat
              ["Action" =: ("DeleteLifecycleHook" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "LifecycleHookName" =: _delLifecycleHookName,
               "AutoScalingGroupName" =: _delAutoScalingGroupName]

-- | /See:/ 'deleteLifecycleHookResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drsStatus'
newtype DeleteLifecycleHookResponse = DeleteLifecycleHookResponse'
    { _drsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteLifecycleHookResponse' smart constructor.
deleteLifecycleHookResponse :: Int -> DeleteLifecycleHookResponse
deleteLifecycleHookResponse pStatus_ =
    DeleteLifecycleHookResponse'
    { _drsStatus = pStatus_
    }

-- | Undocumented member.
drsStatus :: Lens' DeleteLifecycleHookResponse Int
drsStatus = lens _drsStatus (\ s a -> s{_drsStatus = a});
