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
-- Module      : Network.AWS.IAM.UploadServerCertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads a server certificate entity for the AWS account. The server certificate entity includes a public key certificate, a private key, and an optional certificate chain, which should all be PEM-encoded.
--
--
-- We recommend that you use <https://aws.amazon.com/certificate-manager/ AWS Certificate Manager> to provision, manage, and deploy your server certificates. With ACM you can request a certificate, deploy it to AWS resources, and let ACM handle certificate renewals for you. Certificates provided by ACM are free. For more information about using ACM, see the <http://docs.aws.amazon.com/acm/latest/userguide/ AWS Certificate Manager User Guide> .
--
-- For more information about working with server certificates, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html Working with Server Certificates> in the /IAM User Guide/ . This topic includes a list of AWS services that can use the server certificates that you manage with IAM.
--
-- For information about the number of server certificates you can upload, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-limits.html Limitations on IAM Entities and Objects> in the /IAM User Guide/ .
--
module Network.AWS.IAM.UploadServerCertificate
    (
    -- * Creating a Request
      uploadServerCertificate
    , UploadServerCertificate
    -- * Request Lenses
    , uscPath
    , uscCertificateChain
    , uscServerCertificateName
    , uscCertificateBody
    , uscPrivateKey

    -- * Destructuring the Response
    , uploadServerCertificateResponse
    , UploadServerCertificateResponse
    -- * Response Lenses
    , ursServerCertificateMetadata
    , ursResponseStatus
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'uploadServerCertificate' smart constructor.
data UploadServerCertificate = UploadServerCertificate'
  { _uscPath                  :: !(Maybe Text)
  , _uscCertificateChain      :: !(Maybe Text)
  , _uscServerCertificateName :: !Text
  , _uscCertificateBody       :: !Text
  , _uscPrivateKey            :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UploadServerCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uscPath' - The path for the server certificate. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ . This parameter is optional. If it is not included, it defaults to a slash (/). This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (\u0021) through the DEL character (\u007F), including most punctuation characters, digits, and upper and lowercased letters.
--
-- * 'uscCertificateChain' - The contents of the certificate chain. This is typically a concatenation of the PEM-encoded public key certificates of the chain. The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:     * Any printable ASCII character ranging from the space character (\u0020) through the end of the ASCII character range     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through \u00FF)     * The special characters tab (\u0009), line feed (\u000A), and carriage return (\u000D)
--
-- * 'uscServerCertificateName' - The name for the server certificate. Do not include the path in this value. The name of the certificate cannot contain any spaces. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'uscCertificateBody' - The contents of the public key certificate in PEM-encoded format. The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:     * Any printable ASCII character ranging from the space character (\u0020) through the end of the ASCII character range     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through \u00FF)     * The special characters tab (\u0009), line feed (\u000A), and carriage return (\u000D)
--
-- * 'uscPrivateKey' - The contents of the private key in PEM-encoded format. The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:     * Any printable ASCII character ranging from the space character (\u0020) through the end of the ASCII character range     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through \u00FF)     * The special characters tab (\u0009), line feed (\u000A), and carriage return (\u000D)
uploadServerCertificate
    :: Text -- ^ 'uscServerCertificateName'
    -> Text -- ^ 'uscCertificateBody'
    -> Text -- ^ 'uscPrivateKey'
    -> UploadServerCertificate
uploadServerCertificate pServerCertificateName_ pCertificateBody_ pPrivateKey_ =
  UploadServerCertificate'
    { _uscPath = Nothing
    , _uscCertificateChain = Nothing
    , _uscServerCertificateName = pServerCertificateName_
    , _uscCertificateBody = pCertificateBody_
    , _uscPrivateKey = _Sensitive # pPrivateKey_
    }


-- | The path for the server certificate. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ . This parameter is optional. If it is not included, it defaults to a slash (/). This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (\u0021) through the DEL character (\u007F), including most punctuation characters, digits, and upper and lowercased letters.
uscPath :: Lens' UploadServerCertificate (Maybe Text)
uscPath = lens _uscPath (\ s a -> s{_uscPath = a})

-- | The contents of the certificate chain. This is typically a concatenation of the PEM-encoded public key certificates of the chain. The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:     * Any printable ASCII character ranging from the space character (\u0020) through the end of the ASCII character range     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through \u00FF)     * The special characters tab (\u0009), line feed (\u000A), and carriage return (\u000D)
uscCertificateChain :: Lens' UploadServerCertificate (Maybe Text)
uscCertificateChain = lens _uscCertificateChain (\ s a -> s{_uscCertificateChain = a})

-- | The name for the server certificate. Do not include the path in this value. The name of the certificate cannot contain any spaces. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
uscServerCertificateName :: Lens' UploadServerCertificate Text
uscServerCertificateName = lens _uscServerCertificateName (\ s a -> s{_uscServerCertificateName = a})

-- | The contents of the public key certificate in PEM-encoded format. The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:     * Any printable ASCII character ranging from the space character (\u0020) through the end of the ASCII character range     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through \u00FF)     * The special characters tab (\u0009), line feed (\u000A), and carriage return (\u000D)
uscCertificateBody :: Lens' UploadServerCertificate Text
uscCertificateBody = lens _uscCertificateBody (\ s a -> s{_uscCertificateBody = a})

-- | The contents of the private key in PEM-encoded format. The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:     * Any printable ASCII character ranging from the space character (\u0020) through the end of the ASCII character range     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through \u00FF)     * The special characters tab (\u0009), line feed (\u000A), and carriage return (\u000D)
uscPrivateKey :: Lens' UploadServerCertificate Text
uscPrivateKey = lens _uscPrivateKey (\ s a -> s{_uscPrivateKey = a}) . _Sensitive

instance AWSRequest UploadServerCertificate where
        type Rs UploadServerCertificate =
             UploadServerCertificateResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "UploadServerCertificateResult"
              (\ s h x ->
                 UploadServerCertificateResponse' <$>
                   (x .@? "ServerCertificateMetadata") <*>
                     (pure (fromEnum s)))

instance Hashable UploadServerCertificate where

instance NFData UploadServerCertificate where

instance ToHeaders UploadServerCertificate where
        toHeaders = const mempty

instance ToPath UploadServerCertificate where
        toPath = const "/"

instance ToQuery UploadServerCertificate where
        toQuery UploadServerCertificate'{..}
          = mconcat
              ["Action" =:
                 ("UploadServerCertificate" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "Path" =: _uscPath,
               "CertificateChain" =: _uscCertificateChain,
               "ServerCertificateName" =: _uscServerCertificateName,
               "CertificateBody" =: _uscCertificateBody,
               "PrivateKey" =: _uscPrivateKey]

-- | Contains the response to a successful 'UploadServerCertificate' request.
--
--
--
-- /See:/ 'uploadServerCertificateResponse' smart constructor.
data UploadServerCertificateResponse = UploadServerCertificateResponse'
  { _ursServerCertificateMetadata :: !(Maybe ServerCertificateMetadata)
  , _ursResponseStatus            :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UploadServerCertificateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ursServerCertificateMetadata' - The meta information of the uploaded server certificate without its certificate body, certificate chain, and private key.
--
-- * 'ursResponseStatus' - -- | The response status code.
uploadServerCertificateResponse
    :: Int -- ^ 'ursResponseStatus'
    -> UploadServerCertificateResponse
uploadServerCertificateResponse pResponseStatus_ =
  UploadServerCertificateResponse'
    { _ursServerCertificateMetadata = Nothing
    , _ursResponseStatus = pResponseStatus_
    }


-- | The meta information of the uploaded server certificate without its certificate body, certificate chain, and private key.
ursServerCertificateMetadata :: Lens' UploadServerCertificateResponse (Maybe ServerCertificateMetadata)
ursServerCertificateMetadata = lens _ursServerCertificateMetadata (\ s a -> s{_ursServerCertificateMetadata = a})

-- | -- | The response status code.
ursResponseStatus :: Lens' UploadServerCertificateResponse Int
ursResponseStatus = lens _ursResponseStatus (\ s a -> s{_ursResponseStatus = a})

instance NFData UploadServerCertificateResponse where
