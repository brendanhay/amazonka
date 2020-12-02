{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.ModifyListener
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified properties of the specified listener.
--
--
-- Any properties that you do not specify retain their current values. However, changing the protocol from HTTPS to HTTP removes the security policy and SSL certificate properties. If you change the protocol from HTTP to HTTPS, you must add the security policy and server certificate.
--
module Network.AWS.ELBv2.ModifyListener
    (
    -- * Creating a Request
      modifyListener
    , ModifyListener
    -- * Request Lenses
    , mlSSLPolicy
    , mlProtocol
    , mlDefaultActions
    , mlCertificates
    , mlPort
    , mlListenerARN

    -- * Destructuring the Response
    , modifyListenerResponse
    , ModifyListenerResponse
    -- * Response Lenses
    , mlrsListeners
    , mlrsResponseStatus
    ) where

import Network.AWS.ELBv2.Types
import Network.AWS.ELBv2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyListener' smart constructor.
data ModifyListener = ModifyListener'
  { _mlSSLPolicy      :: !(Maybe Text)
  , _mlProtocol       :: !(Maybe ProtocolEnum)
  , _mlDefaultActions :: !(Maybe [Action])
  , _mlCertificates   :: !(Maybe [Certificate])
  , _mlPort           :: !(Maybe Nat)
  , _mlListenerARN    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyListener' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mlSSLPolicy' - The security policy that defines which protocols and ciphers are supported. For more information, see <http://docs.aws.amazon.com/elasticloadbalancing/latest/application/create-https-listener.html#describe-ssl-policies Security Policies> in the /Application Load Balancers Guide/ .
--
-- * 'mlProtocol' - The protocol for connections from clients to the load balancer. Application Load Balancers support HTTP and HTTPS and Network Load Balancers support TCP.
--
-- * 'mlDefaultActions' - The default action. For Application Load Balancers, the protocol of the specified target group must be HTTP or HTTPS. For Network Load Balancers, the protocol of the specified target group must be TCP.
--
-- * 'mlCertificates' - The default SSL server certificate.
--
-- * 'mlPort' - The port for connections from clients to the load balancer.
--
-- * 'mlListenerARN' - The Amazon Resource Name (ARN) of the listener.
modifyListener
    :: Text -- ^ 'mlListenerARN'
    -> ModifyListener
modifyListener pListenerARN_ =
  ModifyListener'
    { _mlSSLPolicy = Nothing
    , _mlProtocol = Nothing
    , _mlDefaultActions = Nothing
    , _mlCertificates = Nothing
    , _mlPort = Nothing
    , _mlListenerARN = pListenerARN_
    }


-- | The security policy that defines which protocols and ciphers are supported. For more information, see <http://docs.aws.amazon.com/elasticloadbalancing/latest/application/create-https-listener.html#describe-ssl-policies Security Policies> in the /Application Load Balancers Guide/ .
mlSSLPolicy :: Lens' ModifyListener (Maybe Text)
mlSSLPolicy = lens _mlSSLPolicy (\ s a -> s{_mlSSLPolicy = a})

-- | The protocol for connections from clients to the load balancer. Application Load Balancers support HTTP and HTTPS and Network Load Balancers support TCP.
mlProtocol :: Lens' ModifyListener (Maybe ProtocolEnum)
mlProtocol = lens _mlProtocol (\ s a -> s{_mlProtocol = a})

-- | The default action. For Application Load Balancers, the protocol of the specified target group must be HTTP or HTTPS. For Network Load Balancers, the protocol of the specified target group must be TCP.
mlDefaultActions :: Lens' ModifyListener [Action]
mlDefaultActions = lens _mlDefaultActions (\ s a -> s{_mlDefaultActions = a}) . _Default . _Coerce

-- | The default SSL server certificate.
mlCertificates :: Lens' ModifyListener [Certificate]
mlCertificates = lens _mlCertificates (\ s a -> s{_mlCertificates = a}) . _Default . _Coerce

-- | The port for connections from clients to the load balancer.
mlPort :: Lens' ModifyListener (Maybe Natural)
mlPort = lens _mlPort (\ s a -> s{_mlPort = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the listener.
mlListenerARN :: Lens' ModifyListener Text
mlListenerARN = lens _mlListenerARN (\ s a -> s{_mlListenerARN = a})

instance AWSRequest ModifyListener where
        type Rs ModifyListener = ModifyListenerResponse
        request = postQuery eLBv2
        response
          = receiveXMLWrapper "ModifyListenerResult"
              (\ s h x ->
                 ModifyListenerResponse' <$>
                   (x .@? "Listeners" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable ModifyListener where

instance NFData ModifyListener where

instance ToHeaders ModifyListener where
        toHeaders = const mempty

instance ToPath ModifyListener where
        toPath = const "/"

instance ToQuery ModifyListener where
        toQuery ModifyListener'{..}
          = mconcat
              ["Action" =: ("ModifyListener" :: ByteString),
               "Version" =: ("2015-12-01" :: ByteString),
               "SslPolicy" =: _mlSSLPolicy,
               "Protocol" =: _mlProtocol,
               "DefaultActions" =:
                 toQuery (toQueryList "member" <$> _mlDefaultActions),
               "Certificates" =:
                 toQuery (toQueryList "member" <$> _mlCertificates),
               "Port" =: _mlPort, "ListenerArn" =: _mlListenerARN]

-- | /See:/ 'modifyListenerResponse' smart constructor.
data ModifyListenerResponse = ModifyListenerResponse'
  { _mlrsListeners      :: !(Maybe [Listener])
  , _mlrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyListenerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mlrsListeners' - Information about the modified listeners.
--
-- * 'mlrsResponseStatus' - -- | The response status code.
modifyListenerResponse
    :: Int -- ^ 'mlrsResponseStatus'
    -> ModifyListenerResponse
modifyListenerResponse pResponseStatus_ =
  ModifyListenerResponse'
    {_mlrsListeners = Nothing, _mlrsResponseStatus = pResponseStatus_}


-- | Information about the modified listeners.
mlrsListeners :: Lens' ModifyListenerResponse [Listener]
mlrsListeners = lens _mlrsListeners (\ s a -> s{_mlrsListeners = a}) . _Default . _Coerce

-- | -- | The response status code.
mlrsResponseStatus :: Lens' ModifyListenerResponse Int
mlrsResponseStatus = lens _mlrsResponseStatus (\ s a -> s{_mlrsResponseStatus = a})

instance NFData ModifyListenerResponse where
