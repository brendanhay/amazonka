{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AvailableCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AvailableCapacity where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceCapacity
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The capacity information for instances that can be launched onto the Dedicated Host.
--
--
--
-- /See:/ 'availableCapacity' smart constructor.
data AvailableCapacity = AvailableCapacity'
  { _acAvailableInstanceCapacity ::
      !(Maybe [InstanceCapacity]),
    _acAvailableVCPUs :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AvailableCapacity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acAvailableInstanceCapacity' - The number of instances that can be launched onto the Dedicated Host depending on the host's available capacity. For Dedicated Hosts that support multiple instance types, this parameter represents the number of instances for each instance size that is supported on the host.
--
-- * 'acAvailableVCPUs' - The number of vCPUs available for launching instances onto the Dedicated Host.
availableCapacity ::
  AvailableCapacity
availableCapacity =
  AvailableCapacity'
    { _acAvailableInstanceCapacity = Nothing,
      _acAvailableVCPUs = Nothing
    }

-- | The number of instances that can be launched onto the Dedicated Host depending on the host's available capacity. For Dedicated Hosts that support multiple instance types, this parameter represents the number of instances for each instance size that is supported on the host.
acAvailableInstanceCapacity :: Lens' AvailableCapacity [InstanceCapacity]
acAvailableInstanceCapacity = lens _acAvailableInstanceCapacity (\s a -> s {_acAvailableInstanceCapacity = a}) . _Default . _Coerce

-- | The number of vCPUs available for launching instances onto the Dedicated Host.
acAvailableVCPUs :: Lens' AvailableCapacity (Maybe Int)
acAvailableVCPUs = lens _acAvailableVCPUs (\s a -> s {_acAvailableVCPUs = a})

instance FromXML AvailableCapacity where
  parseXML x =
    AvailableCapacity'
      <$> ( x .@? "availableInstanceCapacity" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "availableVCpus")

instance Hashable AvailableCapacity

instance NFData AvailableCapacity
