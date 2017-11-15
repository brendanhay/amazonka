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
-- Module      : Network.AWS.IoT.DeleteCertificate
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified certificate.
--
--
-- A certificate cannot be deleted if it has a policy attached to it or if its status is set to ACTIVE. To delete a certificate, first use the 'DetachPrincipalPolicy' API to detach all policies. Next, use the 'UpdateCertificate' API to set the certificate to the INACTIVE status.
--
module Network.AWS.IoT.DeleteCertificate
    (
    -- * Creating a Request
      deleteCertificate
    , DeleteCertificate
    -- * Request Lenses
    , dcCertificateId

    -- * Destructuring the Response
    , deleteCertificateResponse
    , DeleteCertificateResponse
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the DeleteCertificate operation.
--
--
--
-- /See:/ 'deleteCertificate' smart constructor.
newtype DeleteCertificate = DeleteCertificate'
  { _dcCertificateId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcCertificateId' - The ID of the certificate.
deleteCertificate
    :: Text -- ^ 'dcCertificateId'
    -> DeleteCertificate
deleteCertificate pCertificateId_ =
  DeleteCertificate' {_dcCertificateId = pCertificateId_}


-- | The ID of the certificate.
dcCertificateId :: Lens' DeleteCertificate Text
dcCertificateId = lens _dcCertificateId (\ s a -> s{_dcCertificateId = a});

instance AWSRequest DeleteCertificate where
        type Rs DeleteCertificate = DeleteCertificateResponse
        request = delete ioT
        response = receiveNull DeleteCertificateResponse'

instance Hashable DeleteCertificate where

instance NFData DeleteCertificate where

instance ToHeaders DeleteCertificate where
        toHeaders = const mempty

instance ToPath DeleteCertificate where
        toPath DeleteCertificate'{..}
          = mconcat ["/certificates/", toBS _dcCertificateId]

instance ToQuery DeleteCertificate where
        toQuery = const mempty

-- | /See:/ 'deleteCertificateResponse' smart constructor.
data DeleteCertificateResponse =
  DeleteCertificateResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCertificateResponse' with the minimum fields required to make a request.
--
deleteCertificateResponse
    :: DeleteCertificateResponse
deleteCertificateResponse = DeleteCertificateResponse'


instance NFData DeleteCertificateResponse where
