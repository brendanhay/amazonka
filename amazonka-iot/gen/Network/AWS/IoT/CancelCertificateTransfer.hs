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
-- Module      : Network.AWS.IoT.CancelCertificateTransfer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a pending transfer for the specified certificate.
--
--
-- __Note__ Only the transfer source account can use this operation to cancel a transfer. (Transfer destinations can use 'RejectCertificateTransfer' instead.) After transfer, AWS IoT returns the certificate to the source account in the INACTIVE state. After the destination account has accepted the transfer, the transfer cannot be cancelled.
--
-- After a certificate transfer is cancelled, the status of the certificate changes from PENDING_TRANSFER to INACTIVE.
--
module Network.AWS.IoT.CancelCertificateTransfer
    (
    -- * Creating a Request
      cancelCertificateTransfer
    , CancelCertificateTransfer
    -- * Request Lenses
    , cctCertificateId

    -- * Destructuring the Response
    , cancelCertificateTransferResponse
    , CancelCertificateTransferResponse
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the CancelCertificateTransfer operation.
--
--
--
-- /See:/ 'cancelCertificateTransfer' smart constructor.
newtype CancelCertificateTransfer = CancelCertificateTransfer'
  { _cctCertificateId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelCertificateTransfer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cctCertificateId' - The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
cancelCertificateTransfer
    :: Text -- ^ 'cctCertificateId'
    -> CancelCertificateTransfer
cancelCertificateTransfer pCertificateId_ =
  CancelCertificateTransfer' {_cctCertificateId = pCertificateId_}


-- | The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
cctCertificateId :: Lens' CancelCertificateTransfer Text
cctCertificateId = lens _cctCertificateId (\ s a -> s{_cctCertificateId = a})

instance AWSRequest CancelCertificateTransfer where
        type Rs CancelCertificateTransfer =
             CancelCertificateTransferResponse
        request = patchJSON ioT
        response
          = receiveNull CancelCertificateTransferResponse'

instance Hashable CancelCertificateTransfer where

instance NFData CancelCertificateTransfer where

instance ToHeaders CancelCertificateTransfer where
        toHeaders = const mempty

instance ToJSON CancelCertificateTransfer where
        toJSON = const (Object mempty)

instance ToPath CancelCertificateTransfer where
        toPath CancelCertificateTransfer'{..}
          = mconcat
              ["/cancel-certificate-transfer/",
               toBS _cctCertificateId]

instance ToQuery CancelCertificateTransfer where
        toQuery = const mempty

-- | /See:/ 'cancelCertificateTransferResponse' smart constructor.
data CancelCertificateTransferResponse =
  CancelCertificateTransferResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelCertificateTransferResponse' with the minimum fields required to make a request.
--
cancelCertificateTransferResponse
    :: CancelCertificateTransferResponse
cancelCertificateTransferResponse = CancelCertificateTransferResponse'


instance NFData CancelCertificateTransferResponse
         where
