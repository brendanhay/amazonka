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
-- Module      : Network.AWS.DMS.ImportCertificate
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads the specified certificate.
--
--
module Network.AWS.DMS.ImportCertificate
    (
    -- * Creating a Request
      importCertificate
    , ImportCertificate
    -- * Request Lenses
    , icCertificatePem
    , icCertificateIdentifier

    -- * Destructuring the Response
    , importCertificateResponse
    , ImportCertificateResponse
    -- * Response Lenses
    , icrsCertificate
    , icrsResponseStatus
    ) where

import           Network.AWS.DMS.Types
import           Network.AWS.DMS.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'importCertificate' smart constructor.
data ImportCertificate = ImportCertificate'
    { _icCertificatePem        :: !(Maybe Text)
    , _icCertificateIdentifier :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ImportCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icCertificatePem' - The contents of the .pem X.509 certificate file.
--
-- * 'icCertificateIdentifier' - The customer-assigned name of the certificate. Valid characters are [A-z_0-9].
importCertificate
    :: Text -- ^ 'icCertificateIdentifier'
    -> ImportCertificate
importCertificate pCertificateIdentifier_ =
    ImportCertificate'
    { _icCertificatePem = Nothing
    , _icCertificateIdentifier = pCertificateIdentifier_
    }

-- | The contents of the .pem X.509 certificate file.
icCertificatePem :: Lens' ImportCertificate (Maybe Text)
icCertificatePem = lens _icCertificatePem (\ s a -> s{_icCertificatePem = a});

-- | The customer-assigned name of the certificate. Valid characters are [A-z_0-9].
icCertificateIdentifier :: Lens' ImportCertificate Text
icCertificateIdentifier = lens _icCertificateIdentifier (\ s a -> s{_icCertificateIdentifier = a});

instance AWSRequest ImportCertificate where
        type Rs ImportCertificate = ImportCertificateResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 ImportCertificateResponse' <$>
                   (x .?> "Certificate") <*> (pure (fromEnum s)))

instance Hashable ImportCertificate

instance NFData ImportCertificate

instance ToHeaders ImportCertificate where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.ImportCertificate" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ImportCertificate where
        toJSON ImportCertificate'{..}
          = object
              (catMaybes
                 [("CertificatePem" .=) <$> _icCertificatePem,
                  Just
                    ("CertificateIdentifier" .=
                       _icCertificateIdentifier)])

instance ToPath ImportCertificate where
        toPath = const "/"

instance ToQuery ImportCertificate where
        toQuery = const mempty

-- | /See:/ 'importCertificateResponse' smart constructor.
data ImportCertificateResponse = ImportCertificateResponse'
    { _icrsCertificate    :: !(Maybe Certificate)
    , _icrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ImportCertificateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icrsCertificate' - The certificate to be uploaded.
--
-- * 'icrsResponseStatus' - -- | The response status code.
importCertificateResponse
    :: Int -- ^ 'icrsResponseStatus'
    -> ImportCertificateResponse
importCertificateResponse pResponseStatus_ =
    ImportCertificateResponse'
    { _icrsCertificate = Nothing
    , _icrsResponseStatus = pResponseStatus_
    }

-- | The certificate to be uploaded.
icrsCertificate :: Lens' ImportCertificateResponse (Maybe Certificate)
icrsCertificate = lens _icrsCertificate (\ s a -> s{_icrsCertificate = a});

-- | -- | The response status code.
icrsResponseStatus :: Lens' ImportCertificateResponse Int
icrsResponseStatus = lens _icrsResponseStatus (\ s a -> s{_icrsResponseStatus = a});

instance NFData ImportCertificateResponse
