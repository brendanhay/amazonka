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
-- Module      : Network.AWS.IAM.UploadSigningCertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads an X.509 signing certificate and associates it with the specified IAM user. Some AWS services use X.509 signing certificates to validate requests that are signed with a corresponding private key. When you upload the certificate, its default status is @Active@ .
--
--
-- If the @UserName@ field is not specified, the IAM user name is determined implicitly based on the AWS access key ID used to sign the request. Because this operation works for access keys under the AWS account, you can use this operation to manage AWS account root user credentials even if the AWS account has no associated users.
--
module Network.AWS.IAM.UploadSigningCertificate
    (
    -- * Creating a Request
      uploadSigningCertificate
    , UploadSigningCertificate
    -- * Request Lenses
    , uplUserName
    , uplCertificateBody

    -- * Destructuring the Response
    , uploadSigningCertificateResponse
    , UploadSigningCertificateResponse
    -- * Response Lenses
    , uscrsResponseStatus
    , uscrsCertificate
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'uploadSigningCertificate' smart constructor.
data UploadSigningCertificate = UploadSigningCertificate'
  { _uplUserName        :: !(Maybe Text)
  , _uplCertificateBody :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UploadSigningCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uplUserName' - The name of the user the signing certificate is for. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'uplCertificateBody' - The contents of the signing certificate. The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:     * Any printable ASCII character ranging from the space character (\u0020) through the end of the ASCII character range     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through \u00FF)     * The special characters tab (\u0009), line feed (\u000A), and carriage return (\u000D)
uploadSigningCertificate
    :: Text -- ^ 'uplCertificateBody'
    -> UploadSigningCertificate
uploadSigningCertificate pCertificateBody_ =
  UploadSigningCertificate'
    {_uplUserName = Nothing, _uplCertificateBody = pCertificateBody_}


-- | The name of the user the signing certificate is for. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
uplUserName :: Lens' UploadSigningCertificate (Maybe Text)
uplUserName = lens _uplUserName (\ s a -> s{_uplUserName = a})

-- | The contents of the signing certificate. The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:     * Any printable ASCII character ranging from the space character (\u0020) through the end of the ASCII character range     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through \u00FF)     * The special characters tab (\u0009), line feed (\u000A), and carriage return (\u000D)
uplCertificateBody :: Lens' UploadSigningCertificate Text
uplCertificateBody = lens _uplCertificateBody (\ s a -> s{_uplCertificateBody = a})

instance AWSRequest UploadSigningCertificate where
        type Rs UploadSigningCertificate =
             UploadSigningCertificateResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "UploadSigningCertificateResult"
              (\ s h x ->
                 UploadSigningCertificateResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "Certificate"))

instance Hashable UploadSigningCertificate where

instance NFData UploadSigningCertificate where

instance ToHeaders UploadSigningCertificate where
        toHeaders = const mempty

instance ToPath UploadSigningCertificate where
        toPath = const "/"

instance ToQuery UploadSigningCertificate where
        toQuery UploadSigningCertificate'{..}
          = mconcat
              ["Action" =:
                 ("UploadSigningCertificate" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _uplUserName,
               "CertificateBody" =: _uplCertificateBody]

-- | Contains the response to a successful 'UploadSigningCertificate' request.
--
--
--
-- /See:/ 'uploadSigningCertificateResponse' smart constructor.
data UploadSigningCertificateResponse = UploadSigningCertificateResponse'
  { _uscrsResponseStatus :: !Int
  , _uscrsCertificate    :: !SigningCertificate
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UploadSigningCertificateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uscrsResponseStatus' - -- | The response status code.
--
-- * 'uscrsCertificate' - Information about the certificate.
uploadSigningCertificateResponse
    :: Int -- ^ 'uscrsResponseStatus'
    -> SigningCertificate -- ^ 'uscrsCertificate'
    -> UploadSigningCertificateResponse
uploadSigningCertificateResponse pResponseStatus_ pCertificate_ =
  UploadSigningCertificateResponse'
    {_uscrsResponseStatus = pResponseStatus_, _uscrsCertificate = pCertificate_}


-- | -- | The response status code.
uscrsResponseStatus :: Lens' UploadSigningCertificateResponse Int
uscrsResponseStatus = lens _uscrsResponseStatus (\ s a -> s{_uscrsResponseStatus = a})

-- | Information about the certificate.
uscrsCertificate :: Lens' UploadSigningCertificateResponse SigningCertificate
uscrsCertificate = lens _uscrsCertificate (\ s a -> s{_uscrsCertificate = a})

instance NFData UploadSigningCertificateResponse
         where
