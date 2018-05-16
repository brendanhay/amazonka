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
-- Module      : Network.AWS.Redshift.DeleteHSMClientCertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified HSM client certificate.
--
--
module Network.AWS.Redshift.DeleteHSMClientCertificate
    (
    -- * Creating a Request
      deleteHSMClientCertificate
    , DeleteHSMClientCertificate
    -- * Request Lenses
    , dhsmccHSMClientCertificateIdentifier

    -- * Destructuring the Response
    , deleteHSMClientCertificateResponse
    , DeleteHSMClientCertificateResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'deleteHSMClientCertificate' smart constructor.
newtype DeleteHSMClientCertificate = DeleteHSMClientCertificate'
  { _dhsmccHSMClientCertificateIdentifier :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteHSMClientCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhsmccHSMClientCertificateIdentifier' - The identifier of the HSM client certificate to be deleted.
deleteHSMClientCertificate
    :: Text -- ^ 'dhsmccHSMClientCertificateIdentifier'
    -> DeleteHSMClientCertificate
deleteHSMClientCertificate pHSMClientCertificateIdentifier_ =
  DeleteHSMClientCertificate'
    {_dhsmccHSMClientCertificateIdentifier = pHSMClientCertificateIdentifier_}


-- | The identifier of the HSM client certificate to be deleted.
dhsmccHSMClientCertificateIdentifier :: Lens' DeleteHSMClientCertificate Text
dhsmccHSMClientCertificateIdentifier = lens _dhsmccHSMClientCertificateIdentifier (\ s a -> s{_dhsmccHSMClientCertificateIdentifier = a})

instance AWSRequest DeleteHSMClientCertificate where
        type Rs DeleteHSMClientCertificate =
             DeleteHSMClientCertificateResponse
        request = postQuery redshift
        response
          = receiveNull DeleteHSMClientCertificateResponse'

instance Hashable DeleteHSMClientCertificate where

instance NFData DeleteHSMClientCertificate where

instance ToHeaders DeleteHSMClientCertificate where
        toHeaders = const mempty

instance ToPath DeleteHSMClientCertificate where
        toPath = const "/"

instance ToQuery DeleteHSMClientCertificate where
        toQuery DeleteHSMClientCertificate'{..}
          = mconcat
              ["Action" =:
                 ("DeleteHsmClientCertificate" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "HsmClientCertificateIdentifier" =:
                 _dhsmccHSMClientCertificateIdentifier]

-- | /See:/ 'deleteHSMClientCertificateResponse' smart constructor.
data DeleteHSMClientCertificateResponse =
  DeleteHSMClientCertificateResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteHSMClientCertificateResponse' with the minimum fields required to make a request.
--
deleteHSMClientCertificateResponse
    :: DeleteHSMClientCertificateResponse
deleteHSMClientCertificateResponse = DeleteHSMClientCertificateResponse'


instance NFData DeleteHSMClientCertificateResponse
         where
