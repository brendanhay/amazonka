{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.HostProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.HostProperties where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the properties of a Dedicated Host.
--
--
--
-- /See:/ 'hostProperties' smart constructor.
data HostProperties = HostProperties'
  { _hpInstanceFamily ::
      !(Maybe Text),
    _hpInstanceType :: !(Maybe Text),
    _hpTotalVCPUs :: !(Maybe Int),
    _hpCores :: !(Maybe Int),
    _hpSockets :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HostProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hpInstanceFamily' - The instance family supported by the Dedicated Host. For example, @m5@ .
--
-- * 'hpInstanceType' - The instance type supported by the Dedicated Host. For example, @m5.large@ . If the host supports multiple instance types, no __instanceType__ is returned.
--
-- * 'hpTotalVCPUs' - The total number of vCPUs on the Dedicated Host.
--
-- * 'hpCores' - The number of cores on the Dedicated Host.
--
-- * 'hpSockets' - The number of sockets on the Dedicated Host.
hostProperties ::
  HostProperties
hostProperties =
  HostProperties'
    { _hpInstanceFamily = Nothing,
      _hpInstanceType = Nothing,
      _hpTotalVCPUs = Nothing,
      _hpCores = Nothing,
      _hpSockets = Nothing
    }

-- | The instance family supported by the Dedicated Host. For example, @m5@ .
hpInstanceFamily :: Lens' HostProperties (Maybe Text)
hpInstanceFamily = lens _hpInstanceFamily (\s a -> s {_hpInstanceFamily = a})

-- | The instance type supported by the Dedicated Host. For example, @m5.large@ . If the host supports multiple instance types, no __instanceType__ is returned.
hpInstanceType :: Lens' HostProperties (Maybe Text)
hpInstanceType = lens _hpInstanceType (\s a -> s {_hpInstanceType = a})

-- | The total number of vCPUs on the Dedicated Host.
hpTotalVCPUs :: Lens' HostProperties (Maybe Int)
hpTotalVCPUs = lens _hpTotalVCPUs (\s a -> s {_hpTotalVCPUs = a})

-- | The number of cores on the Dedicated Host.
hpCores :: Lens' HostProperties (Maybe Int)
hpCores = lens _hpCores (\s a -> s {_hpCores = a})

-- | The number of sockets on the Dedicated Host.
hpSockets :: Lens' HostProperties (Maybe Int)
hpSockets = lens _hpSockets (\s a -> s {_hpSockets = a})

instance FromXML HostProperties where
  parseXML x =
    HostProperties'
      <$> (x .@? "instanceFamily")
      <*> (x .@? "instanceType")
      <*> (x .@? "totalVCpus")
      <*> (x .@? "cores")
      <*> (x .@? "sockets")

instance Hashable HostProperties

instance NFData HostProperties
