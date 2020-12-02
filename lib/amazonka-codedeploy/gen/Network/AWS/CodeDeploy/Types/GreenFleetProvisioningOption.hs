{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.GreenFleetProvisioningOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.GreenFleetProvisioningOption where

import Network.AWS.CodeDeploy.Types.GreenFleetProvisioningAction
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the instances that belong to the replacement environment in a blue/green deployment.
--
--
--
-- /See:/ 'greenFleetProvisioningOption' smart constructor.
newtype GreenFleetProvisioningOption = GreenFleetProvisioningOption'
  { _gfpoAction ::
      Maybe
        GreenFleetProvisioningAction
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GreenFleetProvisioningOption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfpoAction' - The method used to add instances to a replacement environment.     * @DISCOVER_EXISTING@ : Use instances that already exist or will be created manually.     * @COPY_AUTO_SCALING_GROUP@ : Use settings from a specified Auto Scaling group to define and create instances in a new Auto Scaling group.
greenFleetProvisioningOption ::
  GreenFleetProvisioningOption
greenFleetProvisioningOption =
  GreenFleetProvisioningOption' {_gfpoAction = Nothing}

-- | The method used to add instances to a replacement environment.     * @DISCOVER_EXISTING@ : Use instances that already exist or will be created manually.     * @COPY_AUTO_SCALING_GROUP@ : Use settings from a specified Auto Scaling group to define and create instances in a new Auto Scaling group.
gfpoAction :: Lens' GreenFleetProvisioningOption (Maybe GreenFleetProvisioningAction)
gfpoAction = lens _gfpoAction (\s a -> s {_gfpoAction = a})

instance FromJSON GreenFleetProvisioningOption where
  parseJSON =
    withObject
      "GreenFleetProvisioningOption"
      (\x -> GreenFleetProvisioningOption' <$> (x .:? "action"))

instance Hashable GreenFleetProvisioningOption

instance NFData GreenFleetProvisioningOption

instance ToJSON GreenFleetProvisioningOption where
  toJSON GreenFleetProvisioningOption' {..} =
    object (catMaybes [("action" .=) <$> _gfpoAction])
