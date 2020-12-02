{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.Listener
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.Listener where

import Network.AWS.ELB.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a listener.
--
--
-- For information about the protocols and the ports supported by Elastic Load Balancing, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-listener-config.html Listeners for Your Classic Load Balancer> in the /Classic Load Balancers Guide/ .
--
--
-- /See:/ 'listener' smart constructor.
data Listener = Listener'
  { _lInstanceProtocol :: !(Maybe Text),
    _lSSLCertificateId :: !(Maybe Text),
    _lProtocol :: !Text,
    _lLoadBalancerPort :: !Int,
    _lInstancePort :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Listener' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lInstanceProtocol' - The protocol to use for routing traffic to instances: HTTP, HTTPS, TCP, or SSL. If the front-end protocol is TCP or SSL, the back-end protocol must be TCP or SSL. If the front-end protocol is HTTP or HTTPS, the back-end protocol must be HTTP or HTTPS. If there is another listener with the same @InstancePort@ whose @InstanceProtocol@ is secure, (HTTPS or SSL), the listener's @InstanceProtocol@ must also be secure. If there is another listener with the same @InstancePort@ whose @InstanceProtocol@ is HTTP or TCP, the listener's @InstanceProtocol@ must be HTTP or TCP.
--
-- * 'lSSLCertificateId' - The Amazon Resource Name (ARN) of the server certificate.
--
-- * 'lProtocol' - The load balancer transport protocol to use for routing: HTTP, HTTPS, TCP, or SSL.
--
-- * 'lLoadBalancerPort' - The port on which the load balancer is listening. On EC2-VPC, you can specify any port from the range 1-65535. On EC2-Classic, you can specify any port from the following list: 25, 80, 443, 465, 587, 1024-65535.
--
-- * 'lInstancePort' - The port on which the instance is listening.
listener ::
  -- | 'lProtocol'
  Text ->
  -- | 'lLoadBalancerPort'
  Int ->
  -- | 'lInstancePort'
  Natural ->
  Listener
listener pProtocol_ pLoadBalancerPort_ pInstancePort_ =
  Listener'
    { _lInstanceProtocol = Nothing,
      _lSSLCertificateId = Nothing,
      _lProtocol = pProtocol_,
      _lLoadBalancerPort = pLoadBalancerPort_,
      _lInstancePort = _Nat # pInstancePort_
    }

-- | The protocol to use for routing traffic to instances: HTTP, HTTPS, TCP, or SSL. If the front-end protocol is TCP or SSL, the back-end protocol must be TCP or SSL. If the front-end protocol is HTTP or HTTPS, the back-end protocol must be HTTP or HTTPS. If there is another listener with the same @InstancePort@ whose @InstanceProtocol@ is secure, (HTTPS or SSL), the listener's @InstanceProtocol@ must also be secure. If there is another listener with the same @InstancePort@ whose @InstanceProtocol@ is HTTP or TCP, the listener's @InstanceProtocol@ must be HTTP or TCP.
lInstanceProtocol :: Lens' Listener (Maybe Text)
lInstanceProtocol = lens _lInstanceProtocol (\s a -> s {_lInstanceProtocol = a})

-- | The Amazon Resource Name (ARN) of the server certificate.
lSSLCertificateId :: Lens' Listener (Maybe Text)
lSSLCertificateId = lens _lSSLCertificateId (\s a -> s {_lSSLCertificateId = a})

-- | The load balancer transport protocol to use for routing: HTTP, HTTPS, TCP, or SSL.
lProtocol :: Lens' Listener Text
lProtocol = lens _lProtocol (\s a -> s {_lProtocol = a})

-- | The port on which the load balancer is listening. On EC2-VPC, you can specify any port from the range 1-65535. On EC2-Classic, you can specify any port from the following list: 25, 80, 443, 465, 587, 1024-65535.
lLoadBalancerPort :: Lens' Listener Int
lLoadBalancerPort = lens _lLoadBalancerPort (\s a -> s {_lLoadBalancerPort = a})

-- | The port on which the instance is listening.
lInstancePort :: Lens' Listener Natural
lInstancePort = lens _lInstancePort (\s a -> s {_lInstancePort = a}) . _Nat

instance FromXML Listener where
  parseXML x =
    Listener'
      <$> (x .@? "InstanceProtocol")
      <*> (x .@? "SSLCertificateId")
      <*> (x .@ "Protocol")
      <*> (x .@ "LoadBalancerPort")
      <*> (x .@ "InstancePort")

instance Hashable Listener

instance NFData Listener

instance ToQuery Listener where
  toQuery Listener' {..} =
    mconcat
      [ "InstanceProtocol" =: _lInstanceProtocol,
        "SSLCertificateId" =: _lSSLCertificateId,
        "Protocol" =: _lProtocol,
        "LoadBalancerPort" =: _lLoadBalancerPort,
        "InstancePort" =: _lInstancePort
      ]
