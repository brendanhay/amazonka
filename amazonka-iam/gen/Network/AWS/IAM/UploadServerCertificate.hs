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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads a server certificate entity for the AWS account. The server
-- certificate entity includes a public key certificate, a private key, and
-- an optional certificate chain, which should all be PEM-encoded.
--
-- For more information about working with server certificates, including a
-- list of AWS services that can use the server certificates that you
-- manage with IAM, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html Working with Server Certificates>
-- in the /IAM User Guide/.
--
-- For information about the number of server certificates you can upload,
-- see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-limits.html Limitations on IAM Entities and Objects>
-- in the /IAM User Guide/.
--
-- Because the body of the public key certificate, private key, and the
-- certificate chain can be large, you should use POST rather than GET when
-- calling 'UploadServerCertificate'. For information about setting up
-- signatures and authorization through the API, go to
-- <http://docs.aws.amazon.com/general/latest/gr/signing_aws_api_requests.html Signing AWS API Requests>
-- in the /AWS General Reference/. For general information about using the
-- Query API with IAM, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/programming.html Calling the API by Making HTTP Query Requests>
-- in the /IAM User Guide/.
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

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'uploadServerCertificate' smart constructor.
data UploadServerCertificate = UploadServerCertificate'
    { _uscPath                  :: !(Maybe Text)
    , _uscCertificateChain      :: !(Maybe Text)
    , _uscServerCertificateName :: !Text
    , _uscCertificateBody       :: !Text
    , _uscPrivateKey            :: !(Sensitive Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UploadServerCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uscPath'
--
-- * 'uscCertificateChain'
--
-- * 'uscServerCertificateName'
--
-- * 'uscCertificateBody'
--
-- * 'uscPrivateKey'
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

-- | The path for the server certificate. For more information about paths,
-- see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/).
--
-- If you are uploading a server certificate specifically for use with
-- Amazon CloudFront distributions, you must specify a path using the
-- '--path' option. The path must begin with '\/cloudfront' and must
-- include a trailing slash (for example, '\/cloudfront\/test\/').
uscPath :: Lens' UploadServerCertificate (Maybe Text)
uscPath = lens _uscPath (\ s a -> s{_uscPath = a});

-- | The contents of the certificate chain. This is typically a concatenation
-- of the PEM-encoded public key certificates of the chain.
uscCertificateChain :: Lens' UploadServerCertificate (Maybe Text)
uscCertificateChain = lens _uscCertificateChain (\ s a -> s{_uscCertificateChain = a});

-- | The name for the server certificate. Do not include the path in this
-- value. The name of the certificate cannot contain any spaces.
uscServerCertificateName :: Lens' UploadServerCertificate Text
uscServerCertificateName = lens _uscServerCertificateName (\ s a -> s{_uscServerCertificateName = a});

-- | The contents of the public key certificate in PEM-encoded format.
uscCertificateBody :: Lens' UploadServerCertificate Text
uscCertificateBody = lens _uscCertificateBody (\ s a -> s{_uscCertificateBody = a});

-- | The contents of the private key in PEM-encoded format.
uscPrivateKey :: Lens' UploadServerCertificate Text
uscPrivateKey = lens _uscPrivateKey (\ s a -> s{_uscPrivateKey = a}) . _Sensitive;

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

instance Hashable UploadServerCertificate

instance NFData UploadServerCertificate

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

-- | Contains the response to a successful < UploadServerCertificate>
-- request.
--
-- /See:/ 'uploadServerCertificateResponse' smart constructor.
data UploadServerCertificateResponse = UploadServerCertificateResponse'
    { _ursServerCertificateMetadata :: !(Maybe ServerCertificateMetadata)
    , _ursResponseStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UploadServerCertificateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ursServerCertificateMetadata'
--
-- * 'ursResponseStatus'
uploadServerCertificateResponse
    :: Int -- ^ 'ursResponseStatus'
    -> UploadServerCertificateResponse
uploadServerCertificateResponse pResponseStatus_ =
    UploadServerCertificateResponse'
    { _ursServerCertificateMetadata = Nothing
    , _ursResponseStatus = pResponseStatus_
    }

-- | The meta information of the uploaded server certificate without its
-- certificate body, certificate chain, and private key.
ursServerCertificateMetadata :: Lens' UploadServerCertificateResponse (Maybe ServerCertificateMetadata)
ursServerCertificateMetadata = lens _ursServerCertificateMetadata (\ s a -> s{_ursServerCertificateMetadata = a});

-- | The response status code.
ursResponseStatus :: Lens' UploadServerCertificateResponse Int
ursResponseStatus = lens _ursResponseStatus (\ s a -> s{_ursResponseStatus = a});

instance NFData UploadServerCertificateResponse
