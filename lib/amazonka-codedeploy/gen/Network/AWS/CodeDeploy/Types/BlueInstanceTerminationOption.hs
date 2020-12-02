{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.BlueInstanceTerminationOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.BlueInstanceTerminationOption where

import Network.AWS.CodeDeploy.Types.InstanceAction
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about whether instances in the original environment are terminated when a blue/green deployment is successful. @BlueInstanceTerminationOption@ does not apply to Lambda deployments.
--
--
--
-- /See:/ 'blueInstanceTerminationOption' smart constructor.
data BlueInstanceTerminationOption = BlueInstanceTerminationOption'
  { _bitoAction ::
      !(Maybe InstanceAction),
    _bitoTerminationWaitTimeInMinutes ::
      !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BlueInstanceTerminationOption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bitoAction' - The action to take on instances in the original environment after a successful blue/green deployment.     * @TERMINATE@ : Instances are terminated after a specified wait time.     * @KEEP_ALIVE@ : Instances are left running after they are deregistered from the load balancer and removed from the deployment group.
--
-- * 'bitoTerminationWaitTimeInMinutes' - For an Amazon EC2 deployment, the number of minutes to wait after a successful blue/green deployment before terminating instances from the original environment. For an Amazon ECS deployment, the number of minutes before deleting the original (blue) task set. During an Amazon ECS deployment, CodeDeploy shifts traffic from the original (blue) task set to a replacement (green) task set.  The maximum setting is 2880 minutes (2 days).
blueInstanceTerminationOption ::
  BlueInstanceTerminationOption
blueInstanceTerminationOption =
  BlueInstanceTerminationOption'
    { _bitoAction = Nothing,
      _bitoTerminationWaitTimeInMinutes = Nothing
    }

-- | The action to take on instances in the original environment after a successful blue/green deployment.     * @TERMINATE@ : Instances are terminated after a specified wait time.     * @KEEP_ALIVE@ : Instances are left running after they are deregistered from the load balancer and removed from the deployment group.
bitoAction :: Lens' BlueInstanceTerminationOption (Maybe InstanceAction)
bitoAction = lens _bitoAction (\s a -> s {_bitoAction = a})

-- | For an Amazon EC2 deployment, the number of minutes to wait after a successful blue/green deployment before terminating instances from the original environment. For an Amazon ECS deployment, the number of minutes before deleting the original (blue) task set. During an Amazon ECS deployment, CodeDeploy shifts traffic from the original (blue) task set to a replacement (green) task set.  The maximum setting is 2880 minutes (2 days).
bitoTerminationWaitTimeInMinutes :: Lens' BlueInstanceTerminationOption (Maybe Int)
bitoTerminationWaitTimeInMinutes = lens _bitoTerminationWaitTimeInMinutes (\s a -> s {_bitoTerminationWaitTimeInMinutes = a})

instance FromJSON BlueInstanceTerminationOption where
  parseJSON =
    withObject
      "BlueInstanceTerminationOption"
      ( \x ->
          BlueInstanceTerminationOption'
            <$> (x .:? "action") <*> (x .:? "terminationWaitTimeInMinutes")
      )

instance Hashable BlueInstanceTerminationOption

instance NFData BlueInstanceTerminationOption

instance ToJSON BlueInstanceTerminationOption where
  toJSON BlueInstanceTerminationOption' {..} =
    object
      ( catMaybes
          [ ("action" .=) <$> _bitoAction,
            ("terminationWaitTimeInMinutes" .=)
              <$> _bitoTerminationWaitTimeInMinutes
          ]
      )
