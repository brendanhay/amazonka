{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.OnDemandProvisioningSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.OnDemandProvisioningSpecification where

import Network.AWS.EMR.Types.OnDemandProvisioningAllocationStrategy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The launch specification for On-Demand Instances in the instance fleet, which determines the allocation strategy.
--
--
--
-- /See:/ 'onDemandProvisioningSpecification' smart constructor.
newtype OnDemandProvisioningSpecification = OnDemandProvisioningSpecification'
  { _odpsAllocationStrategy ::
      OnDemandProvisioningAllocationStrategy
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OnDemandProvisioningSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'odpsAllocationStrategy' - Specifies the strategy to use in launching On-Demand Instance fleets. Currently, the only option is lowest-price (the default), which launches the lowest price first.
onDemandProvisioningSpecification ::
  -- | 'odpsAllocationStrategy'
  OnDemandProvisioningAllocationStrategy ->
  OnDemandProvisioningSpecification
onDemandProvisioningSpecification pAllocationStrategy_ =
  OnDemandProvisioningSpecification'
    { _odpsAllocationStrategy =
        pAllocationStrategy_
    }

-- | Specifies the strategy to use in launching On-Demand Instance fleets. Currently, the only option is lowest-price (the default), which launches the lowest price first.
odpsAllocationStrategy :: Lens' OnDemandProvisioningSpecification OnDemandProvisioningAllocationStrategy
odpsAllocationStrategy = lens _odpsAllocationStrategy (\s a -> s {_odpsAllocationStrategy = a})

instance FromJSON OnDemandProvisioningSpecification where
  parseJSON =
    withObject
      "OnDemandProvisioningSpecification"
      ( \x ->
          OnDemandProvisioningSpecification' <$> (x .: "AllocationStrategy")
      )

instance Hashable OnDemandProvisioningSpecification

instance NFData OnDemandProvisioningSpecification

instance ToJSON OnDemandProvisioningSpecification where
  toJSON OnDemandProvisioningSpecification' {..} =
    object
      ( catMaybes
          [Just ("AllocationStrategy" .= _odpsAllocationStrategy)]
      )
