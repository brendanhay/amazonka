{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceCapacity where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the number of instances that can be launched onto the Dedicated Host.
--
--
--
-- /See:/ 'instanceCapacity' smart constructor.
data InstanceCapacity = InstanceCapacity'
  { _icAvailableCapacity ::
      !(Maybe Int),
    _icInstanceType :: !(Maybe Text),
    _icTotalCapacity :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceCapacity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icAvailableCapacity' - The number of instances that can be launched onto the Dedicated Host based on the host's available capacity.
--
-- * 'icInstanceType' - The instance type supported by the Dedicated Host.
--
-- * 'icTotalCapacity' - The total number of instances that can be launched onto the Dedicated Host if there are no instances running on it.
instanceCapacity ::
  InstanceCapacity
instanceCapacity =
  InstanceCapacity'
    { _icAvailableCapacity = Nothing,
      _icInstanceType = Nothing,
      _icTotalCapacity = Nothing
    }

-- | The number of instances that can be launched onto the Dedicated Host based on the host's available capacity.
icAvailableCapacity :: Lens' InstanceCapacity (Maybe Int)
icAvailableCapacity = lens _icAvailableCapacity (\s a -> s {_icAvailableCapacity = a})

-- | The instance type supported by the Dedicated Host.
icInstanceType :: Lens' InstanceCapacity (Maybe Text)
icInstanceType = lens _icInstanceType (\s a -> s {_icInstanceType = a})

-- | The total number of instances that can be launched onto the Dedicated Host if there are no instances running on it.
icTotalCapacity :: Lens' InstanceCapacity (Maybe Int)
icTotalCapacity = lens _icTotalCapacity (\s a -> s {_icTotalCapacity = a})

instance FromXML InstanceCapacity where
  parseXML x =
    InstanceCapacity'
      <$> (x .@? "availableCapacity")
      <*> (x .@? "instanceType")
      <*> (x .@? "totalCapacity")

instance Hashable InstanceCapacity

instance NFData InstanceCapacity
