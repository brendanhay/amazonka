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
-- Module      : Network.AWS.IoT.RejectCertificateTransfer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects a pending certificate transfer. After AWS IoT rejects a certificate transfer, the certificate status changes from __PENDING_TRANSFER__ to __INACTIVE__ .
--
--
-- To check for pending certificate transfers, call 'ListCertificates' to enumerate your certificates.
--
-- This operation can only be called by the transfer destination. After it is called, the certificate will be returned to the source's account in the INACTIVE state.
--
module Network.AWS.IoT.RejectCertificateTransfer
    (
    -- * Creating a Request
      rejectCertificateTransfer
    , RejectCertificateTransfer
    -- * Request Lenses
    , rctRejectReason
    , rctCertificateId

    -- * Destructuring the Response
    , rejectCertificateTransferResponse
    , RejectCertificateTransferResponse
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the RejectCertificateTransfer operation.
--
--
--
-- /See:/ 'rejectCertificateTransfer' smart constructor.
data RejectCertificateTransfer = RejectCertificateTransfer'
  { _rctRejectReason  :: !(Maybe Text)
  , _rctCertificateId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RejectCertificateTransfer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rctRejectReason' - The reason the certificate transfer was rejected.
--
-- * 'rctCertificateId' - The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
rejectCertificateTransfer
    :: Text -- ^ 'rctCertificateId'
    -> RejectCertificateTransfer
rejectCertificateTransfer pCertificateId_ =
  RejectCertificateTransfer'
    {_rctRejectReason = Nothing, _rctCertificateId = pCertificateId_}


-- | The reason the certificate transfer was rejected.
rctRejectReason :: Lens' RejectCertificateTransfer (Maybe Text)
rctRejectReason = lens _rctRejectReason (\ s a -> s{_rctRejectReason = a})

-- | The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
rctCertificateId :: Lens' RejectCertificateTransfer Text
rctCertificateId = lens _rctCertificateId (\ s a -> s{_rctCertificateId = a})

instance AWSRequest RejectCertificateTransfer where
        type Rs RejectCertificateTransfer =
             RejectCertificateTransferResponse
        request = patchJSON ioT
        response
          = receiveNull RejectCertificateTransferResponse'

instance Hashable RejectCertificateTransfer where

instance NFData RejectCertificateTransfer where

instance ToHeaders RejectCertificateTransfer where
        toHeaders = const mempty

instance ToJSON RejectCertificateTransfer where
        toJSON RejectCertificateTransfer'{..}
          = object
              (catMaybes
                 [("rejectReason" .=) <$> _rctRejectReason])

instance ToPath RejectCertificateTransfer where
        toPath RejectCertificateTransfer'{..}
          = mconcat
              ["/reject-certificate-transfer/",
               toBS _rctCertificateId]

instance ToQuery RejectCertificateTransfer where
        toQuery = const mempty

-- | /See:/ 'rejectCertificateTransferResponse' smart constructor.
data RejectCertificateTransferResponse =
  RejectCertificateTransferResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RejectCertificateTransferResponse' with the minimum fields required to make a request.
--
rejectCertificateTransferResponse
    :: RejectCertificateTransferResponse
rejectCertificateTransferResponse = RejectCertificateTransferResponse'


instance NFData RejectCertificateTransferResponse
         where
