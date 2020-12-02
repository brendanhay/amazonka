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
-- Module      : Network.AWS.IoT.UpdateCertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the status of the specified certificate. This operation is idempotent.
--
--
-- Moving a certificate from the ACTIVE state (including REVOKED) will not disconnect currently connected devices, but these devices will be unable to reconnect.
--
-- The ACTIVE state is required to authenticate devices connecting to AWS IoT using a certificate.
--
module Network.AWS.IoT.UpdateCertificate
    (
    -- * Creating a Request
      updateCertificate
    , UpdateCertificate
    -- * Request Lenses
    , ucCertificateId
    , ucNewStatus

    -- * Destructuring the Response
    , updateCertificateResponse
    , UpdateCertificateResponse
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the UpdateCertificate operation.
--
--
--
-- /See:/ 'updateCertificate' smart constructor.
data UpdateCertificate = UpdateCertificate'
  { _ucCertificateId :: !Text
  , _ucNewStatus     :: !CertificateStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucCertificateId' - The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
--
-- * 'ucNewStatus' - The new status. __Note:__ Setting the status to PENDING_TRANSFER will result in an exception being thrown. PENDING_TRANSFER is a status used internally by AWS IoT. It is not intended for developer use. __Note:__ The status value REGISTER_INACTIVE is deprecated and should not be used.
updateCertificate
    :: Text -- ^ 'ucCertificateId'
    -> CertificateStatus -- ^ 'ucNewStatus'
    -> UpdateCertificate
updateCertificate pCertificateId_ pNewStatus_ =
  UpdateCertificate'
    {_ucCertificateId = pCertificateId_, _ucNewStatus = pNewStatus_}


-- | The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
ucCertificateId :: Lens' UpdateCertificate Text
ucCertificateId = lens _ucCertificateId (\ s a -> s{_ucCertificateId = a})

-- | The new status. __Note:__ Setting the status to PENDING_TRANSFER will result in an exception being thrown. PENDING_TRANSFER is a status used internally by AWS IoT. It is not intended for developer use. __Note:__ The status value REGISTER_INACTIVE is deprecated and should not be used.
ucNewStatus :: Lens' UpdateCertificate CertificateStatus
ucNewStatus = lens _ucNewStatus (\ s a -> s{_ucNewStatus = a})

instance AWSRequest UpdateCertificate where
        type Rs UpdateCertificate = UpdateCertificateResponse
        request = putJSON ioT
        response = receiveNull UpdateCertificateResponse'

instance Hashable UpdateCertificate where

instance NFData UpdateCertificate where

instance ToHeaders UpdateCertificate where
        toHeaders = const mempty

instance ToJSON UpdateCertificate where
        toJSON = const (Object mempty)

instance ToPath UpdateCertificate where
        toPath UpdateCertificate'{..}
          = mconcat ["/certificates/", toBS _ucCertificateId]

instance ToQuery UpdateCertificate where
        toQuery UpdateCertificate'{..}
          = mconcat ["newStatus" =: _ucNewStatus]

-- | /See:/ 'updateCertificateResponse' smart constructor.
data UpdateCertificateResponse =
  UpdateCertificateResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateCertificateResponse' with the minimum fields required to make a request.
--
updateCertificateResponse
    :: UpdateCertificateResponse
updateCertificateResponse = UpdateCertificateResponse'


instance NFData UpdateCertificateResponse where
