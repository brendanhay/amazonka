{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstanceNetworking
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstanceNetworking where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.InstancePortInfo
import Network.AWS.Lightsail.Types.MonthlyTransfer
import Network.AWS.Prelude

-- | Describes monthly data transfer rates and port information for an instance.
--
--
--
-- /See:/ 'instanceNetworking' smart constructor.
data InstanceNetworking = InstanceNetworking'
  { _inMonthlyTransfer ::
      !(Maybe MonthlyTransfer),
    _inPorts :: !(Maybe [InstancePortInfo])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceNetworking' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'inMonthlyTransfer' - The amount of data in GB allocated for monthly data transfers.
--
-- * 'inPorts' - An array of key-value pairs containing information about the ports on the instance.
instanceNetworking ::
  InstanceNetworking
instanceNetworking =
  InstanceNetworking'
    { _inMonthlyTransfer = Nothing,
      _inPorts = Nothing
    }

-- | The amount of data in GB allocated for monthly data transfers.
inMonthlyTransfer :: Lens' InstanceNetworking (Maybe MonthlyTransfer)
inMonthlyTransfer = lens _inMonthlyTransfer (\s a -> s {_inMonthlyTransfer = a})

-- | An array of key-value pairs containing information about the ports on the instance.
inPorts :: Lens' InstanceNetworking [InstancePortInfo]
inPorts = lens _inPorts (\s a -> s {_inPorts = a}) . _Default . _Coerce

instance FromJSON InstanceNetworking where
  parseJSON =
    withObject
      "InstanceNetworking"
      ( \x ->
          InstanceNetworking'
            <$> (x .:? "monthlyTransfer") <*> (x .:? "ports" .!= mempty)
      )

instance Hashable InstanceNetworking

instance NFData InstanceNetworking
