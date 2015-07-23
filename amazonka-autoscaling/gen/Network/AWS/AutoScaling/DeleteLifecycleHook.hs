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
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified lifecycle hook.
--
-- If there are any outstanding lifecycle actions, they are completed first
-- (@ABANDON@ for launching instances, @CONTINUE@ for terminating
-- instances).
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DeleteLifecycleHook.html>
module Network.AWS.AutoScaling.DeleteLifecycleHook
    (
    -- * Request
      DeleteLifecycleHook
    -- ** Request constructor
    , deleteLifecycleHook
    -- ** Request lenses
    , delrqLifecycleHookName
    , delrqAutoScalingGroupName

    -- * Response
    , DeleteLifecycleHookResponse
    -- ** Response constructor
    , deleteLifecycleHookResponse
    -- ** Response lenses
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
-- * 'delrqLifecycleHookName'
--
-- * 'delrqAutoScalingGroupName'
data DeleteLifecycleHook = DeleteLifecycleHook'
    { _delrqLifecycleHookName    :: !Text
    , _delrqAutoScalingGroupName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteLifecycleHook' smart constructor.
deleteLifecycleHook :: Text -> Text -> DeleteLifecycleHook
deleteLifecycleHook pLifecycleHookName_ pAutoScalingGroupName_ =
    DeleteLifecycleHook'
    { _delrqLifecycleHookName = pLifecycleHookName_
    , _delrqAutoScalingGroupName = pAutoScalingGroupName_
    }

-- | The name of the lifecycle hook.
delrqLifecycleHookName :: Lens' DeleteLifecycleHook Text
delrqLifecycleHookName = lens _delrqLifecycleHookName (\ s a -> s{_delrqLifecycleHookName = a});

-- | The name of the Auto Scaling group for the lifecycle hook.
delrqAutoScalingGroupName :: Lens' DeleteLifecycleHook Text
delrqAutoScalingGroupName = lens _delrqAutoScalingGroupName (\ s a -> s{_delrqAutoScalingGroupName = a});

instance AWSRequest DeleteLifecycleHook where
        type Sv DeleteLifecycleHook = AutoScaling
        type Rs DeleteLifecycleHook =
             DeleteLifecycleHookResponse
        request = post
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
               "LifecycleHookName" =: _delrqLifecycleHookName,
               "AutoScalingGroupName" =: _delrqAutoScalingGroupName]

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

-- | FIXME: Undocumented member.
drsStatus :: Lens' DeleteLifecycleHookResponse Int
drsStatus = lens _drsStatus (\ s a -> s{_drsStatus = a});
