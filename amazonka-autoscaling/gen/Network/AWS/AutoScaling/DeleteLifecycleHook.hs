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
-- Module      : Network.AWS.AutoScaling.DeleteLifecycleHook
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified lifecycle hook.
--
--
-- If there are any outstanding lifecycle actions, they are completed first (@ABANDON@ for launching instances, @CONTINUE@ for terminating instances).
--
module Network.AWS.AutoScaling.DeleteLifecycleHook
    (
    -- * Creating a Request
      deleteLifecycleHook
    , DeleteLifecycleHook
    -- * Request Lenses
    , delLifecycleHookName
    , delAutoScalingGroupName

    -- * Destructuring the Response
    , deleteLifecycleHookResponse
    , DeleteLifecycleHookResponse
    -- * Response Lenses
    , drsResponseStatus
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteLifecycleHook' smart constructor.
data DeleteLifecycleHook = DeleteLifecycleHook'
  { _delLifecycleHookName    :: !Text
  , _delAutoScalingGroupName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLifecycleHook' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delLifecycleHookName' - The name of the lifecycle hook.
--
-- * 'delAutoScalingGroupName' - The name of the Auto Scaling group.
deleteLifecycleHook
    :: Text -- ^ 'delLifecycleHookName'
    -> Text -- ^ 'delAutoScalingGroupName'
    -> DeleteLifecycleHook
deleteLifecycleHook pLifecycleHookName_ pAutoScalingGroupName_ =
  DeleteLifecycleHook'
    { _delLifecycleHookName = pLifecycleHookName_
    , _delAutoScalingGroupName = pAutoScalingGroupName_
    }


-- | The name of the lifecycle hook.
delLifecycleHookName :: Lens' DeleteLifecycleHook Text
delLifecycleHookName = lens _delLifecycleHookName (\ s a -> s{_delLifecycleHookName = a})

-- | The name of the Auto Scaling group.
delAutoScalingGroupName :: Lens' DeleteLifecycleHook Text
delAutoScalingGroupName = lens _delAutoScalingGroupName (\ s a -> s{_delAutoScalingGroupName = a})

instance AWSRequest DeleteLifecycleHook where
        type Rs DeleteLifecycleHook =
             DeleteLifecycleHookResponse
        request = postQuery autoScaling
        response
          = receiveXMLWrapper "DeleteLifecycleHookResult"
              (\ s h x ->
                 DeleteLifecycleHookResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteLifecycleHook where

instance NFData DeleteLifecycleHook where

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
newtype DeleteLifecycleHookResponse = DeleteLifecycleHookResponse'
  { _drsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLifecycleHookResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsResponseStatus' - -- | The response status code.
deleteLifecycleHookResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DeleteLifecycleHookResponse
deleteLifecycleHookResponse pResponseStatus_ =
  DeleteLifecycleHookResponse' {_drsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
drsResponseStatus :: Lens' DeleteLifecycleHookResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DeleteLifecycleHookResponse where
