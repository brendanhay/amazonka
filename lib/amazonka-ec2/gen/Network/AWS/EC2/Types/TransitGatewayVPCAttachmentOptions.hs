{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayVPCAttachmentOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayVPCAttachmentOptions where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ApplianceModeSupportValue
import Network.AWS.EC2.Types.DNSSupportValue
import Network.AWS.EC2.Types.IPv6SupportValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the VPC attachment options.
--
--
--
-- /See:/ 'transitGatewayVPCAttachmentOptions' smart constructor.
data TransitGatewayVPCAttachmentOptions = TransitGatewayVPCAttachmentOptions'
  { _tgvaoIPv6Support ::
      !( Maybe
           IPv6SupportValue
       ),
    _tgvaoApplianceModeSupport ::
      !( Maybe
           ApplianceModeSupportValue
       ),
    _tgvaoDNSSupport ::
      !( Maybe
           DNSSupportValue
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransitGatewayVPCAttachmentOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgvaoIPv6Support' - Indicates whether IPv6 support is disabled.
--
-- * 'tgvaoApplianceModeSupport' - Indicates whether appliance mode support is enabled.
--
-- * 'tgvaoDNSSupport' - Indicates whether DNS support is enabled.
transitGatewayVPCAttachmentOptions ::
  TransitGatewayVPCAttachmentOptions
transitGatewayVPCAttachmentOptions =
  TransitGatewayVPCAttachmentOptions'
    { _tgvaoIPv6Support = Nothing,
      _tgvaoApplianceModeSupport = Nothing,
      _tgvaoDNSSupport = Nothing
    }

-- | Indicates whether IPv6 support is disabled.
tgvaoIPv6Support :: Lens' TransitGatewayVPCAttachmentOptions (Maybe IPv6SupportValue)
tgvaoIPv6Support = lens _tgvaoIPv6Support (\s a -> s {_tgvaoIPv6Support = a})

-- | Indicates whether appliance mode support is enabled.
tgvaoApplianceModeSupport :: Lens' TransitGatewayVPCAttachmentOptions (Maybe ApplianceModeSupportValue)
tgvaoApplianceModeSupport = lens _tgvaoApplianceModeSupport (\s a -> s {_tgvaoApplianceModeSupport = a})

-- | Indicates whether DNS support is enabled.
tgvaoDNSSupport :: Lens' TransitGatewayVPCAttachmentOptions (Maybe DNSSupportValue)
tgvaoDNSSupport = lens _tgvaoDNSSupport (\s a -> s {_tgvaoDNSSupport = a})

instance FromXML TransitGatewayVPCAttachmentOptions where
  parseXML x =
    TransitGatewayVPCAttachmentOptions'
      <$> (x .@? "ipv6Support")
      <*> (x .@? "applianceModeSupport")
      <*> (x .@? "dnsSupport")

instance Hashable TransitGatewayVPCAttachmentOptions

instance NFData TransitGatewayVPCAttachmentOptions
