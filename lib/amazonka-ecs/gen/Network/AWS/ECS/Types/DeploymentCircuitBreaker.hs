{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.DeploymentCircuitBreaker
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.DeploymentCircuitBreaker where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The __deployment circuit breaker__ determines whether a service deployment will fail if the service can't reach a steady state. If enabled, a service deployment will transition to a failed state and stop launching new tasks. You can also enable Amazon ECS to roll back your service to the last completed deployment after a failure. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-type-ecs.html Rolling update> in the /Amazon Elastic Container Service Developer Guide/ .
--
--
--
-- /See:/ 'deploymentCircuitBreaker' smart constructor.
data DeploymentCircuitBreaker = DeploymentCircuitBreaker'
  { _dcbEnable ::
      !Bool,
    _dcbRollback :: !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeploymentCircuitBreaker' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcbEnable' - Whether to enable the deployment circuit breaker logic for the service.
--
-- * 'dcbRollback' - Whether to enable Amazon ECS to roll back the service if a service deployment fails. If rollback is enabled, when a service deployment fails, the service is rolled back to the last deployment that completed successfully.
deploymentCircuitBreaker ::
  -- | 'dcbEnable'
  Bool ->
  -- | 'dcbRollback'
  Bool ->
  DeploymentCircuitBreaker
deploymentCircuitBreaker pEnable_ pRollback_ =
  DeploymentCircuitBreaker'
    { _dcbEnable = pEnable_,
      _dcbRollback = pRollback_
    }

-- | Whether to enable the deployment circuit breaker logic for the service.
dcbEnable :: Lens' DeploymentCircuitBreaker Bool
dcbEnable = lens _dcbEnable (\s a -> s {_dcbEnable = a})

-- | Whether to enable Amazon ECS to roll back the service if a service deployment fails. If rollback is enabled, when a service deployment fails, the service is rolled back to the last deployment that completed successfully.
dcbRollback :: Lens' DeploymentCircuitBreaker Bool
dcbRollback = lens _dcbRollback (\s a -> s {_dcbRollback = a})

instance FromJSON DeploymentCircuitBreaker where
  parseJSON =
    withObject
      "DeploymentCircuitBreaker"
      ( \x ->
          DeploymentCircuitBreaker'
            <$> (x .: "enable") <*> (x .: "rollback")
      )

instance Hashable DeploymentCircuitBreaker

instance NFData DeploymentCircuitBreaker

instance ToJSON DeploymentCircuitBreaker where
  toJSON DeploymentCircuitBreaker' {..} =
    object
      ( catMaybes
          [Just ("enable" .= _dcbEnable), Just ("rollback" .= _dcbRollback)]
      )
