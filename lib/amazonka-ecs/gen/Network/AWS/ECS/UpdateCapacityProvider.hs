{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.UpdateCapacityProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the parameters for a capacity provider.
module Network.AWS.ECS.UpdateCapacityProvider
  ( -- * Creating a Request
    updateCapacityProvider,
    UpdateCapacityProvider,

    -- * Request Lenses
    ucpName,
    ucpAutoScalingGroupProvider,

    -- * Destructuring the Response
    updateCapacityProviderResponse,
    UpdateCapacityProviderResponse,

    -- * Response Lenses
    ucprsCapacityProvider,
    ucprsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateCapacityProvider' smart constructor.
data UpdateCapacityProvider = UpdateCapacityProvider'
  { _ucpName ::
      !Text,
    _ucpAutoScalingGroupProvider ::
      !AutoScalingGroupProviderUpdate
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateCapacityProvider' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucpName' - An object representing the parameters to update for the Auto Scaling group capacity provider.
--
-- * 'ucpAutoScalingGroupProvider' - The name of the capacity provider to update.
updateCapacityProvider ::
  -- | 'ucpName'
  Text ->
  -- | 'ucpAutoScalingGroupProvider'
  AutoScalingGroupProviderUpdate ->
  UpdateCapacityProvider
updateCapacityProvider pName_ pAutoScalingGroupProvider_ =
  UpdateCapacityProvider'
    { _ucpName = pName_,
      _ucpAutoScalingGroupProvider = pAutoScalingGroupProvider_
    }

-- | An object representing the parameters to update for the Auto Scaling group capacity provider.
ucpName :: Lens' UpdateCapacityProvider Text
ucpName = lens _ucpName (\s a -> s {_ucpName = a})

-- | The name of the capacity provider to update.
ucpAutoScalingGroupProvider :: Lens' UpdateCapacityProvider AutoScalingGroupProviderUpdate
ucpAutoScalingGroupProvider = lens _ucpAutoScalingGroupProvider (\s a -> s {_ucpAutoScalingGroupProvider = a})

instance AWSRequest UpdateCapacityProvider where
  type Rs UpdateCapacityProvider = UpdateCapacityProviderResponse
  request = postJSON ecs
  response =
    receiveJSON
      ( \s h x ->
          UpdateCapacityProviderResponse'
            <$> (x .?> "capacityProvider") <*> (pure (fromEnum s))
      )

instance Hashable UpdateCapacityProvider

instance NFData UpdateCapacityProvider

instance ToHeaders UpdateCapacityProvider where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AmazonEC2ContainerServiceV20141113.UpdateCapacityProvider" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateCapacityProvider where
  toJSON UpdateCapacityProvider' {..} =
    object
      ( catMaybes
          [ Just ("name" .= _ucpName),
            Just ("autoScalingGroupProvider" .= _ucpAutoScalingGroupProvider)
          ]
      )

instance ToPath UpdateCapacityProvider where
  toPath = const "/"

instance ToQuery UpdateCapacityProvider where
  toQuery = const mempty

-- | /See:/ 'updateCapacityProviderResponse' smart constructor.
data UpdateCapacityProviderResponse = UpdateCapacityProviderResponse'
  { _ucprsCapacityProvider ::
      !(Maybe CapacityProvider),
    _ucprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateCapacityProviderResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucprsCapacityProvider' - Undocumented member.
--
-- * 'ucprsResponseStatus' - -- | The response status code.
updateCapacityProviderResponse ::
  -- | 'ucprsResponseStatus'
  Int ->
  UpdateCapacityProviderResponse
updateCapacityProviderResponse pResponseStatus_ =
  UpdateCapacityProviderResponse'
    { _ucprsCapacityProvider = Nothing,
      _ucprsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
ucprsCapacityProvider :: Lens' UpdateCapacityProviderResponse (Maybe CapacityProvider)
ucprsCapacityProvider = lens _ucprsCapacityProvider (\s a -> s {_ucprsCapacityProvider = a})

-- | -- | The response status code.
ucprsResponseStatus :: Lens' UpdateCapacityProviderResponse Int
ucprsResponseStatus = lens _ucprsResponseStatus (\s a -> s {_ucprsResponseStatus = a})

instance NFData UpdateCapacityProviderResponse
