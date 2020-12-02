{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.CreateListener
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a listener for the specified Application Load Balancer, Network Load Balancer. or Gateway Load Balancer.
--
--
-- For more information, see the following:
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/load-balancer-listeners.html Listeners for your Application Load Balancers>
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/load-balancer-listeners.html Listeners for your Network Load Balancers>
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing/latest/gateway/gateway-listeners.html Listeners for your Gateway Load Balancers>
--
--
--
-- This operation is idempotent, which means that it completes at most one time. If you attempt to create multiple listeners with the same settings, each call succeeds.
module Network.AWS.ELBv2.CreateListener
  ( -- * Creating a Request
    createListener,
    CreateListener,

    -- * Request Lenses
    clSSLPolicy,
    clProtocol,
    clCertificates,
    clAlpnPolicy,
    clTags,
    clPort,
    clLoadBalancerARN,
    clDefaultActions,

    -- * Destructuring the Response
    createListenerResponse,
    CreateListenerResponse,

    -- * Response Lenses
    clrsListeners,
    clrsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createListener' smart constructor.
data CreateListener = CreateListener'
  { _clSSLPolicy ::
      !(Maybe Text),
    _clProtocol :: !(Maybe ProtocolEnum),
    _clCertificates :: !(Maybe [Certificate]),
    _clAlpnPolicy :: !(Maybe [Text]),
    _clTags :: !(Maybe (List1 Tag)),
    _clPort :: !(Maybe Nat),
    _clLoadBalancerARN :: !Text,
    _clDefaultActions :: ![Action]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateListener' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clSSLPolicy' - [HTTPS and TLS listeners] The security policy that defines which protocols and ciphers are supported. For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/create-https-listener.html#describe-ssl-policies Security policies> in the /Application Load Balancers Guide/ and <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html#describe-ssl-policies Security policies> in the /Network Load Balancers Guide/ .
--
-- * 'clProtocol' - The protocol for connections from clients to the load balancer. For Application Load Balancers, the supported protocols are HTTP and HTTPS. For Network Load Balancers, the supported protocols are TCP, TLS, UDP, and TCP_UDP. You can’t specify the UDP or TCP_UDP protocol if dual-stack mode is enabled. You cannot specify a protocol for a Gateway Load Balancer.
--
-- * 'clCertificates' - [HTTPS and TLS listeners] The default certificate for the listener. You must provide exactly one certificate. Set @CertificateArn@ to the certificate ARN but do not set @IsDefault@ .
--
-- * 'clAlpnPolicy' - [TLS listeners] The name of the Application-Layer Protocol Negotiation (ALPN) policy. You can specify one policy name. The following are the possible values:     * @HTTP1Only@      * @HTTP2Only@      * @HTTP2Optional@      * @HTTP2Preferred@      * @None@  For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html#alpn-policies ALPN policies> in the /Network Load Balancers Guide/ .
--
-- * 'clTags' - The tags to assign to the listener.
--
-- * 'clPort' - The port on which the load balancer is listening. You cannot specify a port for a Gateway Load Balancer.
--
-- * 'clLoadBalancerARN' - The Amazon Resource Name (ARN) of the load balancer.
--
-- * 'clDefaultActions' - The actions for the default rule.
createListener ::
  -- | 'clLoadBalancerARN'
  Text ->
  CreateListener
createListener pLoadBalancerARN_ =
  CreateListener'
    { _clSSLPolicy = Nothing,
      _clProtocol = Nothing,
      _clCertificates = Nothing,
      _clAlpnPolicy = Nothing,
      _clTags = Nothing,
      _clPort = Nothing,
      _clLoadBalancerARN = pLoadBalancerARN_,
      _clDefaultActions = mempty
    }

-- | [HTTPS and TLS listeners] The security policy that defines which protocols and ciphers are supported. For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/create-https-listener.html#describe-ssl-policies Security policies> in the /Application Load Balancers Guide/ and <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html#describe-ssl-policies Security policies> in the /Network Load Balancers Guide/ .
clSSLPolicy :: Lens' CreateListener (Maybe Text)
clSSLPolicy = lens _clSSLPolicy (\s a -> s {_clSSLPolicy = a})

-- | The protocol for connections from clients to the load balancer. For Application Load Balancers, the supported protocols are HTTP and HTTPS. For Network Load Balancers, the supported protocols are TCP, TLS, UDP, and TCP_UDP. You can’t specify the UDP or TCP_UDP protocol if dual-stack mode is enabled. You cannot specify a protocol for a Gateway Load Balancer.
clProtocol :: Lens' CreateListener (Maybe ProtocolEnum)
clProtocol = lens _clProtocol (\s a -> s {_clProtocol = a})

-- | [HTTPS and TLS listeners] The default certificate for the listener. You must provide exactly one certificate. Set @CertificateArn@ to the certificate ARN but do not set @IsDefault@ .
clCertificates :: Lens' CreateListener [Certificate]
clCertificates = lens _clCertificates (\s a -> s {_clCertificates = a}) . _Default . _Coerce

-- | [TLS listeners] The name of the Application-Layer Protocol Negotiation (ALPN) policy. You can specify one policy name. The following are the possible values:     * @HTTP1Only@      * @HTTP2Only@      * @HTTP2Optional@      * @HTTP2Preferred@      * @None@  For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html#alpn-policies ALPN policies> in the /Network Load Balancers Guide/ .
clAlpnPolicy :: Lens' CreateListener [Text]
clAlpnPolicy = lens _clAlpnPolicy (\s a -> s {_clAlpnPolicy = a}) . _Default . _Coerce

-- | The tags to assign to the listener.
clTags :: Lens' CreateListener (Maybe (NonEmpty Tag))
clTags = lens _clTags (\s a -> s {_clTags = a}) . mapping _List1

-- | The port on which the load balancer is listening. You cannot specify a port for a Gateway Load Balancer.
clPort :: Lens' CreateListener (Maybe Natural)
clPort = lens _clPort (\s a -> s {_clPort = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the load balancer.
clLoadBalancerARN :: Lens' CreateListener Text
clLoadBalancerARN = lens _clLoadBalancerARN (\s a -> s {_clLoadBalancerARN = a})

-- | The actions for the default rule.
clDefaultActions :: Lens' CreateListener [Action]
clDefaultActions = lens _clDefaultActions (\s a -> s {_clDefaultActions = a}) . _Coerce

instance AWSRequest CreateListener where
  type Rs CreateListener = CreateListenerResponse
  request = postQuery eLBv2
  response =
    receiveXMLWrapper
      "CreateListenerResult"
      ( \s h x ->
          CreateListenerResponse'
            <$> (x .@? "Listeners" .!@ mempty >>= may (parseXMLList "member"))
            <*> (pure (fromEnum s))
      )

instance Hashable CreateListener

instance NFData CreateListener

instance ToHeaders CreateListener where
  toHeaders = const mempty

instance ToPath CreateListener where
  toPath = const "/"

instance ToQuery CreateListener where
  toQuery CreateListener' {..} =
    mconcat
      [ "Action" =: ("CreateListener" :: ByteString),
        "Version" =: ("2015-12-01" :: ByteString),
        "SslPolicy" =: _clSSLPolicy,
        "Protocol" =: _clProtocol,
        "Certificates"
          =: toQuery (toQueryList "member" <$> _clCertificates),
        "AlpnPolicy" =: toQuery (toQueryList "member" <$> _clAlpnPolicy),
        "Tags" =: toQuery (toQueryList "member" <$> _clTags),
        "Port" =: _clPort,
        "LoadBalancerArn" =: _clLoadBalancerARN,
        "DefaultActions" =: toQueryList "member" _clDefaultActions
      ]

-- | /See:/ 'createListenerResponse' smart constructor.
data CreateListenerResponse = CreateListenerResponse'
  { _clrsListeners ::
      !(Maybe [Listener]),
    _clrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateListenerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clrsListeners' - Information about the listener.
--
-- * 'clrsResponseStatus' - -- | The response status code.
createListenerResponse ::
  -- | 'clrsResponseStatus'
  Int ->
  CreateListenerResponse
createListenerResponse pResponseStatus_ =
  CreateListenerResponse'
    { _clrsListeners = Nothing,
      _clrsResponseStatus = pResponseStatus_
    }

-- | Information about the listener.
clrsListeners :: Lens' CreateListenerResponse [Listener]
clrsListeners = lens _clrsListeners (\s a -> s {_clrsListeners = a}) . _Default . _Coerce

-- | -- | The response status code.
clrsResponseStatus :: Lens' CreateListenerResponse Int
clrsResponseStatus = lens _clrsResponseStatus (\s a -> s {_clrsResponseStatus = a})

instance NFData CreateListenerResponse
