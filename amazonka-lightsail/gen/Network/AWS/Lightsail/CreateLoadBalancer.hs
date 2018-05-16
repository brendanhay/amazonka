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
-- Module      : Network.AWS.Lightsail.CreateLoadBalancer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Lightsail load balancer. To learn more about deciding whether to load balance your application, see <https://lightsail.aws.amazon.com/ls/docs/how-to/article/configure-lightsail-instances-for-load-balancing Configure your Lightsail instances for load balancing> . You can create up to 5 load balancers per AWS Region in your account.
--
--
-- When you create a load balancer, you can specify a unique name and port settings. To change additional load balancer settings, use the @UpdateLoadBalancerAttribute@ operation.
--
module Network.AWS.Lightsail.CreateLoadBalancer
    (
    -- * Creating a Request
      createLoadBalancer
    , CreateLoadBalancer
    -- * Request Lenses
    , clbHealthCheckPath
    , clbCertificateName
    , clbCertificateDomainName
    , clbCertificateAlternativeNames
    , clbLoadBalancerName
    , clbInstancePort

    -- * Destructuring the Response
    , createLoadBalancerResponse
    , CreateLoadBalancerResponse
    -- * Response Lenses
    , clbrsOperations
    , clbrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createLoadBalancer' smart constructor.
data CreateLoadBalancer = CreateLoadBalancer'
  { _clbHealthCheckPath             :: !(Maybe Text)
  , _clbCertificateName             :: !(Maybe Text)
  , _clbCertificateDomainName       :: !(Maybe Text)
  , _clbCertificateAlternativeNames :: !(Maybe [Text])
  , _clbLoadBalancerName            :: !Text
  , _clbInstancePort                :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLoadBalancer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clbHealthCheckPath' - The path you provided to perform the load balancer health check. If you didn't specify a health check path, Lightsail uses the root path of your website (e.g., @"/"@ ). You may want to specify a custom health check path other than the root of your application if your home page loads slowly or has a lot of media or scripting on it.
--
-- * 'clbCertificateName' - The name of the SSL/TLS certificate. If you specify @certificateName@ , then @certificateDomainName@ is required (and vice-versa).
--
-- * 'clbCertificateDomainName' - The domain name with which your certificate is associated (e.g., @example.com@ ). If you specify @certificateDomainName@ , then @certificateName@ is required (and vice-versa).
--
-- * 'clbCertificateAlternativeNames' - The optional alternative domains and subdomains to use with your SSL/TLS certificate (e.g., @www.example.com@ , @example.com@ , @m.example.com@ , @blog.example.com@ ).
--
-- * 'clbLoadBalancerName' - The name of your load balancer.
--
-- * 'clbInstancePort' - The instance port where you're creating your load balancer.
createLoadBalancer
    :: Text -- ^ 'clbLoadBalancerName'
    -> Natural -- ^ 'clbInstancePort'
    -> CreateLoadBalancer
createLoadBalancer pLoadBalancerName_ pInstancePort_ =
  CreateLoadBalancer'
    { _clbHealthCheckPath = Nothing
    , _clbCertificateName = Nothing
    , _clbCertificateDomainName = Nothing
    , _clbCertificateAlternativeNames = Nothing
    , _clbLoadBalancerName = pLoadBalancerName_
    , _clbInstancePort = _Nat # pInstancePort_
    }


-- | The path you provided to perform the load balancer health check. If you didn't specify a health check path, Lightsail uses the root path of your website (e.g., @"/"@ ). You may want to specify a custom health check path other than the root of your application if your home page loads slowly or has a lot of media or scripting on it.
clbHealthCheckPath :: Lens' CreateLoadBalancer (Maybe Text)
clbHealthCheckPath = lens _clbHealthCheckPath (\ s a -> s{_clbHealthCheckPath = a})

-- | The name of the SSL/TLS certificate. If you specify @certificateName@ , then @certificateDomainName@ is required (and vice-versa).
clbCertificateName :: Lens' CreateLoadBalancer (Maybe Text)
clbCertificateName = lens _clbCertificateName (\ s a -> s{_clbCertificateName = a})

-- | The domain name with which your certificate is associated (e.g., @example.com@ ). If you specify @certificateDomainName@ , then @certificateName@ is required (and vice-versa).
clbCertificateDomainName :: Lens' CreateLoadBalancer (Maybe Text)
clbCertificateDomainName = lens _clbCertificateDomainName (\ s a -> s{_clbCertificateDomainName = a})

-- | The optional alternative domains and subdomains to use with your SSL/TLS certificate (e.g., @www.example.com@ , @example.com@ , @m.example.com@ , @blog.example.com@ ).
clbCertificateAlternativeNames :: Lens' CreateLoadBalancer [Text]
clbCertificateAlternativeNames = lens _clbCertificateAlternativeNames (\ s a -> s{_clbCertificateAlternativeNames = a}) . _Default . _Coerce

-- | The name of your load balancer.
clbLoadBalancerName :: Lens' CreateLoadBalancer Text
clbLoadBalancerName = lens _clbLoadBalancerName (\ s a -> s{_clbLoadBalancerName = a})

-- | The instance port where you're creating your load balancer.
clbInstancePort :: Lens' CreateLoadBalancer Natural
clbInstancePort = lens _clbInstancePort (\ s a -> s{_clbInstancePort = a}) . _Nat

instance AWSRequest CreateLoadBalancer where
        type Rs CreateLoadBalancer =
             CreateLoadBalancerResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 CreateLoadBalancerResponse' <$>
                   (x .?> "operations" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable CreateLoadBalancer where

instance NFData CreateLoadBalancer where

instance ToHeaders CreateLoadBalancer where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.CreateLoadBalancer" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateLoadBalancer where
        toJSON CreateLoadBalancer'{..}
          = object
              (catMaybes
                 [("healthCheckPath" .=) <$> _clbHealthCheckPath,
                  ("certificateName" .=) <$> _clbCertificateName,
                  ("certificateDomainName" .=) <$>
                    _clbCertificateDomainName,
                  ("certificateAlternativeNames" .=) <$>
                    _clbCertificateAlternativeNames,
                  Just ("loadBalancerName" .= _clbLoadBalancerName),
                  Just ("instancePort" .= _clbInstancePort)])

instance ToPath CreateLoadBalancer where
        toPath = const "/"

instance ToQuery CreateLoadBalancer where
        toQuery = const mempty

-- | /See:/ 'createLoadBalancerResponse' smart constructor.
data CreateLoadBalancerResponse = CreateLoadBalancerResponse'
  { _clbrsOperations     :: !(Maybe [Operation])
  , _clbrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLoadBalancerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clbrsOperations' - An object containing information about the API operations.
--
-- * 'clbrsResponseStatus' - -- | The response status code.
createLoadBalancerResponse
    :: Int -- ^ 'clbrsResponseStatus'
    -> CreateLoadBalancerResponse
createLoadBalancerResponse pResponseStatus_ =
  CreateLoadBalancerResponse'
    {_clbrsOperations = Nothing, _clbrsResponseStatus = pResponseStatus_}


-- | An object containing information about the API operations.
clbrsOperations :: Lens' CreateLoadBalancerResponse [Operation]
clbrsOperations = lens _clbrsOperations (\ s a -> s{_clbrsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
clbrsResponseStatus :: Lens' CreateLoadBalancerResponse Int
clbrsResponseStatus = lens _clbrsResponseStatus (\ s a -> s{_clbrsResponseStatus = a})

instance NFData CreateLoadBalancerResponse where
