{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.Listener
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.Listener where

import Network.AWS.ELBv2.Types.Action
import Network.AWS.ELBv2.Types.Certificate
import Network.AWS.ELBv2.Types.ProtocolEnum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a listener.
--
--
--
-- /See:/ 'listener' smart constructor.
data Listener = Listener'
  { _lSSLPolicy :: !(Maybe Text),
    _lListenerARN :: !(Maybe Text),
    _lProtocol :: !(Maybe ProtocolEnum),
    _lDefaultActions :: !(Maybe [Action]),
    _lCertificates :: !(Maybe [Certificate]),
    _lLoadBalancerARN :: !(Maybe Text),
    _lAlpnPolicy :: !(Maybe [Text]),
    _lPort :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Listener' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lSSLPolicy' - [HTTPS or TLS listener] The security policy that defines which protocols and ciphers are supported.
--
-- * 'lListenerARN' - The Amazon Resource Name (ARN) of the listener.
--
-- * 'lProtocol' - The protocol for connections from clients to the load balancer.
--
-- * 'lDefaultActions' - The default actions for the listener.
--
-- * 'lCertificates' - [HTTPS or TLS listener] The default certificate for the listener.
--
-- * 'lLoadBalancerARN' - The Amazon Resource Name (ARN) of the load balancer.
--
-- * 'lAlpnPolicy' - [TLS listener] The name of the Application-Layer Protocol Negotiation (ALPN) policy.
--
-- * 'lPort' - The port on which the load balancer is listening.
listener ::
  Listener
listener =
  Listener'
    { _lSSLPolicy = Nothing,
      _lListenerARN = Nothing,
      _lProtocol = Nothing,
      _lDefaultActions = Nothing,
      _lCertificates = Nothing,
      _lLoadBalancerARN = Nothing,
      _lAlpnPolicy = Nothing,
      _lPort = Nothing
    }

-- | [HTTPS or TLS listener] The security policy that defines which protocols and ciphers are supported.
lSSLPolicy :: Lens' Listener (Maybe Text)
lSSLPolicy = lens _lSSLPolicy (\s a -> s {_lSSLPolicy = a})

-- | The Amazon Resource Name (ARN) of the listener.
lListenerARN :: Lens' Listener (Maybe Text)
lListenerARN = lens _lListenerARN (\s a -> s {_lListenerARN = a})

-- | The protocol for connections from clients to the load balancer.
lProtocol :: Lens' Listener (Maybe ProtocolEnum)
lProtocol = lens _lProtocol (\s a -> s {_lProtocol = a})

-- | The default actions for the listener.
lDefaultActions :: Lens' Listener [Action]
lDefaultActions = lens _lDefaultActions (\s a -> s {_lDefaultActions = a}) . _Default . _Coerce

-- | [HTTPS or TLS listener] The default certificate for the listener.
lCertificates :: Lens' Listener [Certificate]
lCertificates = lens _lCertificates (\s a -> s {_lCertificates = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the load balancer.
lLoadBalancerARN :: Lens' Listener (Maybe Text)
lLoadBalancerARN = lens _lLoadBalancerARN (\s a -> s {_lLoadBalancerARN = a})

-- | [TLS listener] The name of the Application-Layer Protocol Negotiation (ALPN) policy.
lAlpnPolicy :: Lens' Listener [Text]
lAlpnPolicy = lens _lAlpnPolicy (\s a -> s {_lAlpnPolicy = a}) . _Default . _Coerce

-- | The port on which the load balancer is listening.
lPort :: Lens' Listener (Maybe Natural)
lPort = lens _lPort (\s a -> s {_lPort = a}) . mapping _Nat

instance FromXML Listener where
  parseXML x =
    Listener'
      <$> (x .@? "SslPolicy")
      <*> (x .@? "ListenerArn")
      <*> (x .@? "Protocol")
      <*> (x .@? "DefaultActions" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "Certificates" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "LoadBalancerArn")
      <*> (x .@? "AlpnPolicy" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "Port")

instance Hashable Listener

instance NFData Listener
