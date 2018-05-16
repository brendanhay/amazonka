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
-- Module      : Network.AWS.SSM.DeregisterManagedInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the server or virtual machine from the list of registered servers. You can reregister the instance again at any time. If you don't plan to use Run Command on the server, we suggest uninstalling the SSM Agent first.
--
--
module Network.AWS.SSM.DeregisterManagedInstance
    (
    -- * Creating a Request
      deregisterManagedInstance
    , DeregisterManagedInstance
    -- * Request Lenses
    , dmiInstanceId

    -- * Destructuring the Response
    , deregisterManagedInstanceResponse
    , DeregisterManagedInstanceResponse
    -- * Response Lenses
    , dmirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'deregisterManagedInstance' smart constructor.
newtype DeregisterManagedInstance = DeregisterManagedInstance'
  { _dmiInstanceId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeregisterManagedInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmiInstanceId' - The ID assigned to the managed instance when you registered it using the activation process.
deregisterManagedInstance
    :: Text -- ^ 'dmiInstanceId'
    -> DeregisterManagedInstance
deregisterManagedInstance pInstanceId_ =
  DeregisterManagedInstance' {_dmiInstanceId = pInstanceId_}


-- | The ID assigned to the managed instance when you registered it using the activation process.
dmiInstanceId :: Lens' DeregisterManagedInstance Text
dmiInstanceId = lens _dmiInstanceId (\ s a -> s{_dmiInstanceId = a})

instance AWSRequest DeregisterManagedInstance where
        type Rs DeregisterManagedInstance =
             DeregisterManagedInstanceResponse
        request = postJSON ssm
        response
          = receiveEmpty
              (\ s h x ->
                 DeregisterManagedInstanceResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeregisterManagedInstance where

instance NFData DeregisterManagedInstance where

instance ToHeaders DeregisterManagedInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DeregisterManagedInstance" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeregisterManagedInstance where
        toJSON DeregisterManagedInstance'{..}
          = object
              (catMaybes [Just ("InstanceId" .= _dmiInstanceId)])

instance ToPath DeregisterManagedInstance where
        toPath = const "/"

instance ToQuery DeregisterManagedInstance where
        toQuery = const mempty

-- | /See:/ 'deregisterManagedInstanceResponse' smart constructor.
newtype DeregisterManagedInstanceResponse = DeregisterManagedInstanceResponse'
  { _dmirsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeregisterManagedInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmirsResponseStatus' - -- | The response status code.
deregisterManagedInstanceResponse
    :: Int -- ^ 'dmirsResponseStatus'
    -> DeregisterManagedInstanceResponse
deregisterManagedInstanceResponse pResponseStatus_ =
  DeregisterManagedInstanceResponse' {_dmirsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dmirsResponseStatus :: Lens' DeregisterManagedInstanceResponse Int
dmirsResponseStatus = lens _dmirsResponseStatus (\ s a -> s{_dmirsResponseStatus = a})

instance NFData DeregisterManagedInstanceResponse
         where
