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
-- Module      : Network.AWS.IoT.AcceptCertificateTransfer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a pending certificate transfer. The default state of the certificate is INACTIVE.
--
--
-- To check for pending certificate transfers, call 'ListCertificates' to enumerate your certificates.
--
module Network.AWS.IoT.AcceptCertificateTransfer
    (
    -- * Creating a Request
      acceptCertificateTransfer
    , AcceptCertificateTransfer
    -- * Request Lenses
    , actSetAsActive
    , actCertificateId

    -- * Destructuring the Response
    , acceptCertificateTransferResponse
    , AcceptCertificateTransferResponse
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the AcceptCertificateTransfer operation.
--
--
--
-- /See:/ 'acceptCertificateTransfer' smart constructor.
data AcceptCertificateTransfer = AcceptCertificateTransfer'
  { _actSetAsActive   :: !(Maybe Bool)
  , _actCertificateId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AcceptCertificateTransfer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'actSetAsActive' - Specifies whether the certificate is active.
--
-- * 'actCertificateId' - The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
acceptCertificateTransfer
    :: Text -- ^ 'actCertificateId'
    -> AcceptCertificateTransfer
acceptCertificateTransfer pCertificateId_ =
  AcceptCertificateTransfer'
    {_actSetAsActive = Nothing, _actCertificateId = pCertificateId_}


-- | Specifies whether the certificate is active.
actSetAsActive :: Lens' AcceptCertificateTransfer (Maybe Bool)
actSetAsActive = lens _actSetAsActive (\ s a -> s{_actSetAsActive = a})

-- | The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
actCertificateId :: Lens' AcceptCertificateTransfer Text
actCertificateId = lens _actCertificateId (\ s a -> s{_actCertificateId = a})

instance AWSRequest AcceptCertificateTransfer where
        type Rs AcceptCertificateTransfer =
             AcceptCertificateTransferResponse
        request = patchJSON ioT
        response
          = receiveNull AcceptCertificateTransferResponse'

instance Hashable AcceptCertificateTransfer where

instance NFData AcceptCertificateTransfer where

instance ToHeaders AcceptCertificateTransfer where
        toHeaders = const mempty

instance ToJSON AcceptCertificateTransfer where
        toJSON = const (Object mempty)

instance ToPath AcceptCertificateTransfer where
        toPath AcceptCertificateTransfer'{..}
          = mconcat
              ["/accept-certificate-transfer/",
               toBS _actCertificateId]

instance ToQuery AcceptCertificateTransfer where
        toQuery AcceptCertificateTransfer'{..}
          = mconcat ["setAsActive" =: _actSetAsActive]

-- | /See:/ 'acceptCertificateTransferResponse' smart constructor.
data AcceptCertificateTransferResponse =
  AcceptCertificateTransferResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AcceptCertificateTransferResponse' with the minimum fields required to make a request.
--
acceptCertificateTransferResponse
    :: AcceptCertificateTransferResponse
acceptCertificateTransferResponse = AcceptCertificateTransferResponse'


instance NFData AcceptCertificateTransferResponse
         where
