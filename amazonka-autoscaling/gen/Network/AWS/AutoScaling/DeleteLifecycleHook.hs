{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.AutoScaling.DeleteLifecycleHook
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

-- | Deletes the specified lifecycle hook.
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
    , delLifecycleHookName
    , delAutoScalingGroupName

    -- * Response
    , DeleteLifecycleHookResponse
    -- ** Response constructor
    , deleteLifecycleHookResponse
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteLifecycleHook' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delLifecycleHookName'
--
-- * 'delAutoScalingGroupName'
data DeleteLifecycleHook = DeleteLifecycleHook'{_delLifecycleHookName :: Text, _delAutoScalingGroupName :: Text} deriving (Eq, Read, Show)

-- | 'DeleteLifecycleHook' smart constructor.
deleteLifecycleHook :: Text -> Text -> DeleteLifecycleHook
deleteLifecycleHook pLifecycleHookName pAutoScalingGroupName = DeleteLifecycleHook'{_delLifecycleHookName = pLifecycleHookName, _delAutoScalingGroupName = pAutoScalingGroupName};

-- | The name of the lifecycle hook.
delLifecycleHookName :: Lens' DeleteLifecycleHook Text
delLifecycleHookName = lens _delLifecycleHookName (\ s a -> s{_delLifecycleHookName = a});

-- | The name of the Auto Scaling group for the lifecycle hook.
delAutoScalingGroupName :: Lens' DeleteLifecycleHook Text
delAutoScalingGroupName = lens _delAutoScalingGroupName (\ s a -> s{_delAutoScalingGroupName = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest DeleteLifecycleHook where
        type Sv DeleteLifecycleHook = AutoScaling
        type Rs DeleteLifecycleHook =
             DeleteLifecycleHookResponse
        request = post
        response = receiveNull DeleteLifecycleHookResponse'

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
data DeleteLifecycleHookResponse = DeleteLifecycleHookResponse' deriving (Eq, Read, Show)

-- | 'DeleteLifecycleHookResponse' smart constructor.
deleteLifecycleHookResponse :: DeleteLifecycleHookResponse
deleteLifecycleHookResponse = DeleteLifecycleHookResponse';
