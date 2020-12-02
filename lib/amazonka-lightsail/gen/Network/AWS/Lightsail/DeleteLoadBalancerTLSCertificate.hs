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
-- Module      : Network.AWS.Lightsail.DeleteLoadBalancerTLSCertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an SSL/TLS certificate associated with a Lightsail load balancer.
--
--
module Network.AWS.Lightsail.DeleteLoadBalancerTLSCertificate
    (
    -- * Creating a Request
      deleteLoadBalancerTLSCertificate
    , DeleteLoadBalancerTLSCertificate
    -- * Request Lenses
    , dlbtcForce
    , dlbtcLoadBalancerName
    , dlbtcCertificateName

    -- * Destructuring the Response
    , deleteLoadBalancerTLSCertificateResponse
    , DeleteLoadBalancerTLSCertificateResponse
    -- * Response Lenses
    , dlbtcrsOperations
    , dlbtcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteLoadBalancerTLSCertificate' smart constructor.
data DeleteLoadBalancerTLSCertificate = DeleteLoadBalancerTLSCertificate'
  { _dlbtcForce            :: !(Maybe Bool)
  , _dlbtcLoadBalancerName :: !Text
  , _dlbtcCertificateName  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLoadBalancerTLSCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlbtcForce' - When @true@ , forces the deletion of an SSL/TLS certificate. There can be two certificates associated with a Lightsail load balancer: the primary and the backup. The force parameter is required when the primary SSL/TLS certificate is in use by an instance attached to the load balancer.
--
-- * 'dlbtcLoadBalancerName' - The load balancer name.
--
-- * 'dlbtcCertificateName' - The SSL/TLS certificate name.
deleteLoadBalancerTLSCertificate
    :: Text -- ^ 'dlbtcLoadBalancerName'
    -> Text -- ^ 'dlbtcCertificateName'
    -> DeleteLoadBalancerTLSCertificate
deleteLoadBalancerTLSCertificate pLoadBalancerName_ pCertificateName_ =
  DeleteLoadBalancerTLSCertificate'
    { _dlbtcForce = Nothing
    , _dlbtcLoadBalancerName = pLoadBalancerName_
    , _dlbtcCertificateName = pCertificateName_
    }


-- | When @true@ , forces the deletion of an SSL/TLS certificate. There can be two certificates associated with a Lightsail load balancer: the primary and the backup. The force parameter is required when the primary SSL/TLS certificate is in use by an instance attached to the load balancer.
dlbtcForce :: Lens' DeleteLoadBalancerTLSCertificate (Maybe Bool)
dlbtcForce = lens _dlbtcForce (\ s a -> s{_dlbtcForce = a})

-- | The load balancer name.
dlbtcLoadBalancerName :: Lens' DeleteLoadBalancerTLSCertificate Text
dlbtcLoadBalancerName = lens _dlbtcLoadBalancerName (\ s a -> s{_dlbtcLoadBalancerName = a})

-- | The SSL/TLS certificate name.
dlbtcCertificateName :: Lens' DeleteLoadBalancerTLSCertificate Text
dlbtcCertificateName = lens _dlbtcCertificateName (\ s a -> s{_dlbtcCertificateName = a})

instance AWSRequest DeleteLoadBalancerTLSCertificate
         where
        type Rs DeleteLoadBalancerTLSCertificate =
             DeleteLoadBalancerTLSCertificateResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 DeleteLoadBalancerTLSCertificateResponse' <$>
                   (x .?> "operations" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DeleteLoadBalancerTLSCertificate
         where

instance NFData DeleteLoadBalancerTLSCertificate
         where

instance ToHeaders DeleteLoadBalancerTLSCertificate
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.DeleteLoadBalancerTlsCertificate"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteLoadBalancerTLSCertificate
         where
        toJSON DeleteLoadBalancerTLSCertificate'{..}
          = object
              (catMaybes
                 [("force" .=) <$> _dlbtcForce,
                  Just ("loadBalancerName" .= _dlbtcLoadBalancerName),
                  Just ("certificateName" .= _dlbtcCertificateName)])

instance ToPath DeleteLoadBalancerTLSCertificate
         where
        toPath = const "/"

instance ToQuery DeleteLoadBalancerTLSCertificate
         where
        toQuery = const mempty

-- | /See:/ 'deleteLoadBalancerTLSCertificateResponse' smart constructor.
data DeleteLoadBalancerTLSCertificateResponse = DeleteLoadBalancerTLSCertificateResponse'
  { _dlbtcrsOperations     :: !(Maybe [Operation])
  , _dlbtcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLoadBalancerTLSCertificateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlbtcrsOperations' - An object describing the API operations.
--
-- * 'dlbtcrsResponseStatus' - -- | The response status code.
deleteLoadBalancerTLSCertificateResponse
    :: Int -- ^ 'dlbtcrsResponseStatus'
    -> DeleteLoadBalancerTLSCertificateResponse
deleteLoadBalancerTLSCertificateResponse pResponseStatus_ =
  DeleteLoadBalancerTLSCertificateResponse'
    {_dlbtcrsOperations = Nothing, _dlbtcrsResponseStatus = pResponseStatus_}


-- | An object describing the API operations.
dlbtcrsOperations :: Lens' DeleteLoadBalancerTLSCertificateResponse [Operation]
dlbtcrsOperations = lens _dlbtcrsOperations (\ s a -> s{_dlbtcrsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
dlbtcrsResponseStatus :: Lens' DeleteLoadBalancerTLSCertificateResponse Int
dlbtcrsResponseStatus = lens _dlbtcrsResponseStatus (\ s a -> s{_dlbtcrsResponseStatus = a})

instance NFData
           DeleteLoadBalancerTLSCertificateResponse
         where
