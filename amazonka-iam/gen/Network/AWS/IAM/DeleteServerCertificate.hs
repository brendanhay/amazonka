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
-- Module      : Network.AWS.IAM.DeleteServerCertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified server certificate.
--
--
-- For more information about working with server certificates, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html Working with Server Certificates> in the /IAM User Guide/ . This topic also includes a list of AWS services that can use the server certificates that you manage with IAM.
--
-- /Important:/ If you are using a server certificate with Elastic Load Balancing, deleting the certificate could have implications for your application. If Elastic Load Balancing doesn't detect the deletion of bound certificates, it may continue to use the certificates. This could cause Elastic Load Balancing to stop accepting traffic. We recommend that you remove the reference to the certificate from Elastic Load Balancing before using this command to delete the certificate. For more information, go to <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DeleteLoadBalancerListeners.html DeleteLoadBalancerListeners> in the /Elastic Load Balancing API Reference/ .
--
module Network.AWS.IAM.DeleteServerCertificate
    (
    -- * Creating a Request
      deleteServerCertificate
    , DeleteServerCertificate
    -- * Request Lenses
    , dscServerCertificateName

    -- * Destructuring the Response
    , deleteServerCertificateResponse
    , DeleteServerCertificateResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteServerCertificate' smart constructor.
newtype DeleteServerCertificate = DeleteServerCertificate'
  { _dscServerCertificateName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteServerCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscServerCertificateName' - The name of the server certificate you want to delete. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
deleteServerCertificate
    :: Text -- ^ 'dscServerCertificateName'
    -> DeleteServerCertificate
deleteServerCertificate pServerCertificateName_ =
  DeleteServerCertificate' {_dscServerCertificateName = pServerCertificateName_}


-- | The name of the server certificate you want to delete. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
dscServerCertificateName :: Lens' DeleteServerCertificate Text
dscServerCertificateName = lens _dscServerCertificateName (\ s a -> s{_dscServerCertificateName = a})

instance AWSRequest DeleteServerCertificate where
        type Rs DeleteServerCertificate =
             DeleteServerCertificateResponse
        request = postQuery iam
        response
          = receiveNull DeleteServerCertificateResponse'

instance Hashable DeleteServerCertificate where

instance NFData DeleteServerCertificate where

instance ToHeaders DeleteServerCertificate where
        toHeaders = const mempty

instance ToPath DeleteServerCertificate where
        toPath = const "/"

instance ToQuery DeleteServerCertificate where
        toQuery DeleteServerCertificate'{..}
          = mconcat
              ["Action" =:
                 ("DeleteServerCertificate" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "ServerCertificateName" =: _dscServerCertificateName]

-- | /See:/ 'deleteServerCertificateResponse' smart constructor.
data DeleteServerCertificateResponse =
  DeleteServerCertificateResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteServerCertificateResponse' with the minimum fields required to make a request.
--
deleteServerCertificateResponse
    :: DeleteServerCertificateResponse
deleteServerCertificateResponse = DeleteServerCertificateResponse'


instance NFData DeleteServerCertificateResponse where
