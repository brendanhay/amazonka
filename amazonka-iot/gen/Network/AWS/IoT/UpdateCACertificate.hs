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
-- Module      : Network.AWS.IoT.UpdateCACertificate
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a registered CA certificate.
module Network.AWS.IoT.UpdateCACertificate
    (
    -- * Creating a Request
      updateCACertificate
    , UpdateCACertificate
    -- * Request Lenses
    , ucacCertificateId
    , ucacNewStatus

    -- * Destructuring the Response
    , updateCACertificateResponse
    , UpdateCACertificateResponse
    ) where

import           Network.AWS.IoT.Types
import           Network.AWS.IoT.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input to the UpdateCACertificate operation.
--
-- /See:/ 'updateCACertificate' smart constructor.
data UpdateCACertificate = UpdateCACertificate'
    { _ucacCertificateId :: !Text
    , _ucacNewStatus     :: !CACertificateStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateCACertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucacCertificateId'
--
-- * 'ucacNewStatus'
updateCACertificate
    :: Text -- ^ 'ucacCertificateId'
    -> CACertificateStatus -- ^ 'ucacNewStatus'
    -> UpdateCACertificate
updateCACertificate pCertificateId_ pNewStatus_ =
    UpdateCACertificate'
    { _ucacCertificateId = pCertificateId_
    , _ucacNewStatus = pNewStatus_
    }

-- | The CA certificate identifier.
ucacCertificateId :: Lens' UpdateCACertificate Text
ucacCertificateId = lens _ucacCertificateId (\ s a -> s{_ucacCertificateId = a});

-- | The updated status of the CA certificate.
ucacNewStatus :: Lens' UpdateCACertificate CACertificateStatus
ucacNewStatus = lens _ucacNewStatus (\ s a -> s{_ucacNewStatus = a});

instance AWSRequest UpdateCACertificate where
        type Rs UpdateCACertificate =
             UpdateCACertificateResponse
        request = putJSON ioT
        response = receiveNull UpdateCACertificateResponse'

instance Hashable UpdateCACertificate

instance NFData UpdateCACertificate

instance ToHeaders UpdateCACertificate where
        toHeaders = const mempty

instance ToJSON UpdateCACertificate where
        toJSON = const (Object mempty)

instance ToPath UpdateCACertificate where
        toPath UpdateCACertificate'{..}
          = mconcat
              ["/cacertificate/", toBS _ucacCertificateId]

instance ToQuery UpdateCACertificate where
        toQuery UpdateCACertificate'{..}
          = mconcat ["newStatus" =: _ucacNewStatus]

-- | /See:/ 'updateCACertificateResponse' smart constructor.
data UpdateCACertificateResponse =
    UpdateCACertificateResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateCACertificateResponse' with the minimum fields required to make a request.
--
updateCACertificateResponse
    :: UpdateCACertificateResponse
updateCACertificateResponse = UpdateCACertificateResponse'

instance NFData UpdateCACertificateResponse
