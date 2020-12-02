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
-- Module      : Network.AWS.IoT.CreateCertificateFromCSR
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an X.509 certificate using the specified certificate signing request.
--
--
-- __Note:__ The CSR must include a public key that is either an RSA key with a length of at least 2048 bits or an ECC key from NIST P-256 or NIST P-384 curves.
--
-- __Note:__ Reusing the same certificate signing request (CSR) results in a distinct certificate.
--
-- You can create multiple certificates in a batch by creating a directory, copying multiple .csr files into that directory, and then specifying that directory on the command line. The following commands show how to create a batch of certificates given a batch of CSRs.
--
-- Assuming a set of CSRs are located inside of the directory my-csr-directory:
--
-- On Linux and OS X, the command is:
--
-- >  ls my-csr-directory/ | xargs -I {} aws iot create-certificate-from-csr --certificate-signing-request file://my-csr-directory/{}
--
-- This command lists all of the CSRs in my-csr-directory and pipes each CSR file name to the aws iot create-certificate-from-csr AWS CLI command to create a certificate for the corresponding CSR.
--
-- The aws iot create-certificate-from-csr part of the command can also be run in parallel to speed up the certificate creation process:
--
-- >  ls my-csr-directory/ | xargs -P 10 -I {} aws iot create-certificate-from-csr --certificate-signing-request file://my-csr-directory/{}
--
-- On Windows PowerShell, the command to create certificates for all CSRs in my-csr-directory is:
--
-- > ls -Name my-csr-directory | %{aws iot create-certificate-from-csr --certificate-signing-request file://my-csr-directory/$_}
--
-- On a Windows command prompt, the command to create certificates for all CSRs in my-csr-directory is:
--
-- > forfiles /p my-csr-directory /c "cmd /c aws iot create-certificate-from-csr --certificate-signing-request file://@path"
--
module Network.AWS.IoT.CreateCertificateFromCSR
    (
    -- * Creating a Request
      createCertificateFromCSR
    , CreateCertificateFromCSR
    -- * Request Lenses
    , ccfcsrSetAsActive
    , ccfcsrCertificateSigningRequest

    -- * Destructuring the Response
    , createCertificateFromCSRResponse
    , CreateCertificateFromCSRResponse
    -- * Response Lenses
    , ccfcsrrsCertificatePem
    , ccfcsrrsCertificateARN
    , ccfcsrrsCertificateId
    , ccfcsrrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the CreateCertificateFromCSR operation.
--
--
--
-- /See:/ 'createCertificateFromCSR' smart constructor.
data CreateCertificateFromCSR = CreateCertificateFromCSR'
  { _ccfcsrSetAsActive               :: !(Maybe Bool)
  , _ccfcsrCertificateSigningRequest :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCertificateFromCSR' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccfcsrSetAsActive' - Specifies whether the certificate is active.
--
-- * 'ccfcsrCertificateSigningRequest' - The certificate signing request (CSR).
createCertificateFromCSR
    :: Text -- ^ 'ccfcsrCertificateSigningRequest'
    -> CreateCertificateFromCSR
createCertificateFromCSR pCertificateSigningRequest_ =
  CreateCertificateFromCSR'
    { _ccfcsrSetAsActive = Nothing
    , _ccfcsrCertificateSigningRequest = pCertificateSigningRequest_
    }


-- | Specifies whether the certificate is active.
ccfcsrSetAsActive :: Lens' CreateCertificateFromCSR (Maybe Bool)
ccfcsrSetAsActive = lens _ccfcsrSetAsActive (\ s a -> s{_ccfcsrSetAsActive = a})

-- | The certificate signing request (CSR).
ccfcsrCertificateSigningRequest :: Lens' CreateCertificateFromCSR Text
ccfcsrCertificateSigningRequest = lens _ccfcsrCertificateSigningRequest (\ s a -> s{_ccfcsrCertificateSigningRequest = a})

instance AWSRequest CreateCertificateFromCSR where
        type Rs CreateCertificateFromCSR =
             CreateCertificateFromCSRResponse
        request = postJSON ioT
        response
          = receiveJSON
              (\ s h x ->
                 CreateCertificateFromCSRResponse' <$>
                   (x .?> "certificatePem") <*> (x .?> "certificateArn")
                     <*> (x .?> "certificateId")
                     <*> (pure (fromEnum s)))

instance Hashable CreateCertificateFromCSR where

instance NFData CreateCertificateFromCSR where

instance ToHeaders CreateCertificateFromCSR where
        toHeaders = const mempty

instance ToJSON CreateCertificateFromCSR where
        toJSON CreateCertificateFromCSR'{..}
          = object
              (catMaybes
                 [Just
                    ("certificateSigningRequest" .=
                       _ccfcsrCertificateSigningRequest)])

instance ToPath CreateCertificateFromCSR where
        toPath = const "/certificates"

instance ToQuery CreateCertificateFromCSR where
        toQuery CreateCertificateFromCSR'{..}
          = mconcat ["setAsActive" =: _ccfcsrSetAsActive]

-- | The output from the CreateCertificateFromCSR operation.
--
--
--
-- /See:/ 'createCertificateFromCSRResponse' smart constructor.
data CreateCertificateFromCSRResponse = CreateCertificateFromCSRResponse'
  { _ccfcsrrsCertificatePem :: !(Maybe Text)
  , _ccfcsrrsCertificateARN :: !(Maybe Text)
  , _ccfcsrrsCertificateId  :: !(Maybe Text)
  , _ccfcsrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCertificateFromCSRResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccfcsrrsCertificatePem' - The certificate data, in PEM format.
--
-- * 'ccfcsrrsCertificateARN' - The Amazon Resource Name (ARN) of the certificate. You can use the ARN as a principal for policy operations.
--
-- * 'ccfcsrrsCertificateId' - The ID of the certificate. Certificate management operations only take a certificateId.
--
-- * 'ccfcsrrsResponseStatus' - -- | The response status code.
createCertificateFromCSRResponse
    :: Int -- ^ 'ccfcsrrsResponseStatus'
    -> CreateCertificateFromCSRResponse
createCertificateFromCSRResponse pResponseStatus_ =
  CreateCertificateFromCSRResponse'
    { _ccfcsrrsCertificatePem = Nothing
    , _ccfcsrrsCertificateARN = Nothing
    , _ccfcsrrsCertificateId = Nothing
    , _ccfcsrrsResponseStatus = pResponseStatus_
    }


-- | The certificate data, in PEM format.
ccfcsrrsCertificatePem :: Lens' CreateCertificateFromCSRResponse (Maybe Text)
ccfcsrrsCertificatePem = lens _ccfcsrrsCertificatePem (\ s a -> s{_ccfcsrrsCertificatePem = a})

-- | The Amazon Resource Name (ARN) of the certificate. You can use the ARN as a principal for policy operations.
ccfcsrrsCertificateARN :: Lens' CreateCertificateFromCSRResponse (Maybe Text)
ccfcsrrsCertificateARN = lens _ccfcsrrsCertificateARN (\ s a -> s{_ccfcsrrsCertificateARN = a})

-- | The ID of the certificate. Certificate management operations only take a certificateId.
ccfcsrrsCertificateId :: Lens' CreateCertificateFromCSRResponse (Maybe Text)
ccfcsrrsCertificateId = lens _ccfcsrrsCertificateId (\ s a -> s{_ccfcsrrsCertificateId = a})

-- | -- | The response status code.
ccfcsrrsResponseStatus :: Lens' CreateCertificateFromCSRResponse Int
ccfcsrrsResponseStatus = lens _ccfcsrrsResponseStatus (\ s a -> s{_ccfcsrrsResponseStatus = a})

instance NFData CreateCertificateFromCSRResponse
         where
