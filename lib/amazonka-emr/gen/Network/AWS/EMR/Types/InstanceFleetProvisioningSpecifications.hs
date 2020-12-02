{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceFleetProvisioningSpecifications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceFleetProvisioningSpecifications where

import Network.AWS.EMR.Types.OnDemandProvisioningSpecification
import Network.AWS.EMR.Types.SpotProvisioningSpecification
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The launch specification for Spot Instances in the fleet, which determines the defined duration, provisioning timeout behavior, and allocation strategy.
--
--
--
-- /See:/ 'instanceFleetProvisioningSpecifications' smart constructor.
data InstanceFleetProvisioningSpecifications = InstanceFleetProvisioningSpecifications'
  { _ifpsSpotSpecification ::
      !( Maybe
           SpotProvisioningSpecification
       ),
    _ifpsOnDemandSpecification ::
      !( Maybe
           OnDemandProvisioningSpecification
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceFleetProvisioningSpecifications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifpsSpotSpecification' - The launch specification for Spot Instances in the fleet, which determines the defined duration, provisioning timeout behavior, and allocation strategy.
--
-- * 'ifpsOnDemandSpecification' - The launch specification for On-Demand Instances in the instance fleet, which determines the allocation strategy.
instanceFleetProvisioningSpecifications ::
  InstanceFleetProvisioningSpecifications
instanceFleetProvisioningSpecifications =
  InstanceFleetProvisioningSpecifications'
    { _ifpsSpotSpecification =
        Nothing,
      _ifpsOnDemandSpecification = Nothing
    }

-- | The launch specification for Spot Instances in the fleet, which determines the defined duration, provisioning timeout behavior, and allocation strategy.
ifpsSpotSpecification :: Lens' InstanceFleetProvisioningSpecifications (Maybe SpotProvisioningSpecification)
ifpsSpotSpecification = lens _ifpsSpotSpecification (\s a -> s {_ifpsSpotSpecification = a})

-- | The launch specification for On-Demand Instances in the instance fleet, which determines the allocation strategy.
ifpsOnDemandSpecification :: Lens' InstanceFleetProvisioningSpecifications (Maybe OnDemandProvisioningSpecification)
ifpsOnDemandSpecification = lens _ifpsOnDemandSpecification (\s a -> s {_ifpsOnDemandSpecification = a})

instance FromJSON InstanceFleetProvisioningSpecifications where
  parseJSON =
    withObject
      "InstanceFleetProvisioningSpecifications"
      ( \x ->
          InstanceFleetProvisioningSpecifications'
            <$> (x .:? "SpotSpecification") <*> (x .:? "OnDemandSpecification")
      )

instance Hashable InstanceFleetProvisioningSpecifications

instance NFData InstanceFleetProvisioningSpecifications

instance ToJSON InstanceFleetProvisioningSpecifications where
  toJSON InstanceFleetProvisioningSpecifications' {..} =
    object
      ( catMaybes
          [ ("SpotSpecification" .=) <$> _ifpsSpotSpecification,
            ("OnDemandSpecification" .=) <$> _ifpsOnDemandSpecification
          ]
      )
