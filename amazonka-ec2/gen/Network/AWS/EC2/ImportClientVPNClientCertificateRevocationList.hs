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
-- Module      : Network.AWS.EC2.ImportClientVPNClientCertificateRevocationList
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads a client certificate revocation list to the specified Client VPN endpoint. Uploading a client certificate revocation list overwrites the existing client certificate revocation list.
--
--
-- Uploading a client certificate revocation list resets existing client connections.
--
module Network.AWS.EC2.ImportClientVPNClientCertificateRevocationList
    (
    -- * Creating a Request
      importClientVPNClientCertificateRevocationList
    , ImportClientVPNClientCertificateRevocationList
    -- * Request Lenses
    , icvccrlDryRun
    , icvccrlClientVPNEndpointId
    , icvccrlCertificateRevocationList

    -- * Destructuring the Response
    , importClientVPNClientCertificateRevocationListResponse
    , ImportClientVPNClientCertificateRevocationListResponse
    -- * Response Lenses
    , icvccrlrsReturn
    , icvccrlrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'importClientVPNClientCertificateRevocationList' smart constructor.
data ImportClientVPNClientCertificateRevocationList = ImportClientVPNClientCertificateRevocationList'
  { _icvccrlDryRun                    :: !(Maybe Bool)
  , _icvccrlClientVPNEndpointId       :: !Text
  , _icvccrlCertificateRevocationList :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportClientVPNClientCertificateRevocationList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icvccrlDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'icvccrlClientVPNEndpointId' - The ID of the Client VPN endpoint to which the client certificate revocation list applies.
--
-- * 'icvccrlCertificateRevocationList' - The client certificate revocation list file. For more information, see <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/cvpn-working-certificates.html#cvpn-working-certificates-generate Generate a Client Certificate Revocation List> in the /AWS Client VPN Administrator Guide/ .
importClientVPNClientCertificateRevocationList
    :: Text -- ^ 'icvccrlClientVPNEndpointId'
    -> Text -- ^ 'icvccrlCertificateRevocationList'
    -> ImportClientVPNClientCertificateRevocationList
importClientVPNClientCertificateRevocationList pClientVPNEndpointId_ pCertificateRevocationList_ =
  ImportClientVPNClientCertificateRevocationList'
    { _icvccrlDryRun = Nothing
    , _icvccrlClientVPNEndpointId = pClientVPNEndpointId_
    , _icvccrlCertificateRevocationList = pCertificateRevocationList_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
icvccrlDryRun :: Lens' ImportClientVPNClientCertificateRevocationList (Maybe Bool)
icvccrlDryRun = lens _icvccrlDryRun (\ s a -> s{_icvccrlDryRun = a})

-- | The ID of the Client VPN endpoint to which the client certificate revocation list applies.
icvccrlClientVPNEndpointId :: Lens' ImportClientVPNClientCertificateRevocationList Text
icvccrlClientVPNEndpointId = lens _icvccrlClientVPNEndpointId (\ s a -> s{_icvccrlClientVPNEndpointId = a})

-- | The client certificate revocation list file. For more information, see <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/cvpn-working-certificates.html#cvpn-working-certificates-generate Generate a Client Certificate Revocation List> in the /AWS Client VPN Administrator Guide/ .
icvccrlCertificateRevocationList :: Lens' ImportClientVPNClientCertificateRevocationList Text
icvccrlCertificateRevocationList = lens _icvccrlCertificateRevocationList (\ s a -> s{_icvccrlCertificateRevocationList = a})

instance AWSRequest
           ImportClientVPNClientCertificateRevocationList
         where
        type Rs
               ImportClientVPNClientCertificateRevocationList
             =
             ImportClientVPNClientCertificateRevocationListResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 ImportClientVPNClientCertificateRevocationListResponse'
                   <$> (x .@? "return") <*> (pure (fromEnum s)))

instance Hashable
           ImportClientVPNClientCertificateRevocationList
         where

instance NFData
           ImportClientVPNClientCertificateRevocationList
         where

instance ToHeaders
           ImportClientVPNClientCertificateRevocationList
         where
        toHeaders = const mempty

instance ToPath
           ImportClientVPNClientCertificateRevocationList
         where
        toPath = const "/"

instance ToQuery
           ImportClientVPNClientCertificateRevocationList
         where
        toQuery
          ImportClientVPNClientCertificateRevocationList'{..}
          = mconcat
              ["Action" =:
                 ("ImportClientVpnClientCertificateRevocationList" ::
                    ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _icvccrlDryRun,
               "ClientVpnEndpointId" =: _icvccrlClientVPNEndpointId,
               "CertificateRevocationList" =:
                 _icvccrlCertificateRevocationList]

-- | /See:/ 'importClientVPNClientCertificateRevocationListResponse' smart constructor.
data ImportClientVPNClientCertificateRevocationListResponse = ImportClientVPNClientCertificateRevocationListResponse'
  { _icvccrlrsReturn         :: !(Maybe Bool)
  , _icvccrlrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportClientVPNClientCertificateRevocationListResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icvccrlrsReturn' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- * 'icvccrlrsResponseStatus' - -- | The response status code.
importClientVPNClientCertificateRevocationListResponse
    :: Int -- ^ 'icvccrlrsResponseStatus'
    -> ImportClientVPNClientCertificateRevocationListResponse
importClientVPNClientCertificateRevocationListResponse pResponseStatus_ =
  ImportClientVPNClientCertificateRevocationListResponse'
    {_icvccrlrsReturn = Nothing, _icvccrlrsResponseStatus = pResponseStatus_}


-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
icvccrlrsReturn :: Lens' ImportClientVPNClientCertificateRevocationListResponse (Maybe Bool)
icvccrlrsReturn = lens _icvccrlrsReturn (\ s a -> s{_icvccrlrsReturn = a})

-- | -- | The response status code.
icvccrlrsResponseStatus :: Lens' ImportClientVPNClientCertificateRevocationListResponse Int
icvccrlrsResponseStatus = lens _icvccrlrsResponseStatus (\ s a -> s{_icvccrlrsResponseStatus = a})

instance NFData
           ImportClientVPNClientCertificateRevocationListResponse
         where
