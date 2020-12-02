{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.NetworkInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.NetworkInterface where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing the elastic network interface for tasks that use the @awsvpc@ network mode.
--
--
--
-- /See:/ 'networkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { _niIpv6Address ::
      !(Maybe Text),
    _niPrivateIPv4Address :: !(Maybe Text),
    _niAttachmentId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NetworkInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'niIpv6Address' - The private IPv6 address for the network interface.
--
-- * 'niPrivateIPv4Address' - The private IPv4 address for the network interface.
--
-- * 'niAttachmentId' - The attachment ID for the network interface.
networkInterface ::
  NetworkInterface
networkInterface =
  NetworkInterface'
    { _niIpv6Address = Nothing,
      _niPrivateIPv4Address = Nothing,
      _niAttachmentId = Nothing
    }

-- | The private IPv6 address for the network interface.
niIpv6Address :: Lens' NetworkInterface (Maybe Text)
niIpv6Address = lens _niIpv6Address (\s a -> s {_niIpv6Address = a})

-- | The private IPv4 address for the network interface.
niPrivateIPv4Address :: Lens' NetworkInterface (Maybe Text)
niPrivateIPv4Address = lens _niPrivateIPv4Address (\s a -> s {_niPrivateIPv4Address = a})

-- | The attachment ID for the network interface.
niAttachmentId :: Lens' NetworkInterface (Maybe Text)
niAttachmentId = lens _niAttachmentId (\s a -> s {_niAttachmentId = a})

instance FromJSON NetworkInterface where
  parseJSON =
    withObject
      "NetworkInterface"
      ( \x ->
          NetworkInterface'
            <$> (x .:? "ipv6Address")
            <*> (x .:? "privateIpv4Address")
            <*> (x .:? "attachmentId")
      )

instance Hashable NetworkInterface

instance NFData NetworkInterface
