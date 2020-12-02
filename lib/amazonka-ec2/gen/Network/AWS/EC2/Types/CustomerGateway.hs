{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CustomerGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CustomerGateway where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a customer gateway.
--
--
--
-- /See:/ 'customerGateway' smart constructor.
data CustomerGateway = CustomerGateway'
  { _cusCertificateARN ::
      !(Maybe Text),
    _cusDeviceName :: !(Maybe Text),
    _cusTags :: !(Maybe [Tag]),
    _cusBGPASN :: !Text,
    _cusCustomerGatewayId :: !Text,
    _cusIPAddress :: !Text,
    _cusState :: !Text,
    _cusType :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CustomerGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cusCertificateARN' - The Amazon Resource Name (ARN) for the customer gateway certificate.
--
-- * 'cusDeviceName' - The name of customer gateway device.
--
-- * 'cusTags' - Any tags assigned to the customer gateway.
--
-- * 'cusBGPASN' - The customer gateway's Border Gateway Protocol (BGP) Autonomous System Number (ASN).
--
-- * 'cusCustomerGatewayId' - The ID of the customer gateway.
--
-- * 'cusIPAddress' - The Internet-routable IP address of the customer gateway's outside interface.
--
-- * 'cusState' - The current state of the customer gateway (@pending | available | deleting | deleted@ ).
--
-- * 'cusType' - The type of VPN connection the customer gateway supports (@ipsec.1@ ).
customerGateway ::
  -- | 'cusBGPASN'
  Text ->
  -- | 'cusCustomerGatewayId'
  Text ->
  -- | 'cusIPAddress'
  Text ->
  -- | 'cusState'
  Text ->
  -- | 'cusType'
  Text ->
  CustomerGateway
customerGateway
  pBGPASN_
  pCustomerGatewayId_
  pIPAddress_
  pState_
  pType_ =
    CustomerGateway'
      { _cusCertificateARN = Nothing,
        _cusDeviceName = Nothing,
        _cusTags = Nothing,
        _cusBGPASN = pBGPASN_,
        _cusCustomerGatewayId = pCustomerGatewayId_,
        _cusIPAddress = pIPAddress_,
        _cusState = pState_,
        _cusType = pType_
      }

-- | The Amazon Resource Name (ARN) for the customer gateway certificate.
cusCertificateARN :: Lens' CustomerGateway (Maybe Text)
cusCertificateARN = lens _cusCertificateARN (\s a -> s {_cusCertificateARN = a})

-- | The name of customer gateway device.
cusDeviceName :: Lens' CustomerGateway (Maybe Text)
cusDeviceName = lens _cusDeviceName (\s a -> s {_cusDeviceName = a})

-- | Any tags assigned to the customer gateway.
cusTags :: Lens' CustomerGateway [Tag]
cusTags = lens _cusTags (\s a -> s {_cusTags = a}) . _Default . _Coerce

-- | The customer gateway's Border Gateway Protocol (BGP) Autonomous System Number (ASN).
cusBGPASN :: Lens' CustomerGateway Text
cusBGPASN = lens _cusBGPASN (\s a -> s {_cusBGPASN = a})

-- | The ID of the customer gateway.
cusCustomerGatewayId :: Lens' CustomerGateway Text
cusCustomerGatewayId = lens _cusCustomerGatewayId (\s a -> s {_cusCustomerGatewayId = a})

-- | The Internet-routable IP address of the customer gateway's outside interface.
cusIPAddress :: Lens' CustomerGateway Text
cusIPAddress = lens _cusIPAddress (\s a -> s {_cusIPAddress = a})

-- | The current state of the customer gateway (@pending | available | deleting | deleted@ ).
cusState :: Lens' CustomerGateway Text
cusState = lens _cusState (\s a -> s {_cusState = a})

-- | The type of VPN connection the customer gateway supports (@ipsec.1@ ).
cusType :: Lens' CustomerGateway Text
cusType = lens _cusType (\s a -> s {_cusType = a})

instance FromXML CustomerGateway where
  parseXML x =
    CustomerGateway'
      <$> (x .@? "certificateArn")
      <*> (x .@? "deviceName")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@ "bgpAsn")
      <*> (x .@ "customerGatewayId")
      <*> (x .@ "ipAddress")
      <*> (x .@ "state")
      <*> (x .@ "type")

instance Hashable CustomerGateway

instance NFData CustomerGateway
