{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UploadServerCertificate
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Uploads a server certificate entity for the AWS account. The server
-- certificate entity includes a public key certificate, a private key, and
-- an optional certificate chain, which should all be PEM-encoded.
--
-- For information about the number of server certificates you can upload,
-- see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html Limitations on IAM Entities>
-- in the /Using IAM/ guide.
--
-- Because the body of the public key certificate, private key, and the
-- certificate chain can be large, you should use POST rather than GET when
-- calling @UploadServerCertificate@. For information about setting up
-- signatures and authorization through the API, go to
-- <http://docs.aws.amazon.com/general/latest/gr/signing_aws_api_requests.html Signing AWS API Requests>
-- in the /AWS General Reference/. For general information about using the
-- Query API with IAM, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/IAM_UsingQueryAPI.html Making Query Requests>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UploadServerCertificate.html>
module Network.AWS.IAM.UploadServerCertificate
    (
    -- * Request
      UploadServerCertificate
    -- ** Request constructor
    , uploadServerCertificate
    -- ** Request lenses
    , uscPath
    , uscCertificateChain
    , uscServerCertificateName
    , uscCertificateBody
    , uscPrivateKey

    -- * Response
    , UploadServerCertificateResponse
    -- ** Response constructor
    , uploadServerCertificateResponse
    -- ** Response lenses
    , uplServerCertificateMetadata
    , uplStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'uploadServerCertificate' smart constructor.
--
-- The fields accessible through corresponding lenses are:
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
data UploadServerCertificate = UploadServerCertificate'
    { _uscPath                  :: !(Maybe Text)
    , _uscCertificateChain      :: !(Maybe Text)
    , _uscServerCertificateName :: !Text
    , _uscCertificateBody       :: !Text
    , _uscPrivateKey            :: !(Sensitive Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UploadServerCertificate' smart constructor.
uploadServerCertificate :: Text -> Text -> Text -> UploadServerCertificate
uploadServerCertificate pServerCertificateName pCertificateBody pPrivateKey =
    UploadServerCertificate'
    { _uscPath = Nothing
    , _uscCertificateChain = Nothing
    , _uscServerCertificateName = pServerCertificateName
    , _uscCertificateBody = pCertificateBody
    , _uscPrivateKey = _Sensitive # pPrivateKey
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
-- @--path@ option. The path must begin with @\/cloudfront@ and must
-- include a trailing slash (for example, @\/cloudfront\/test\/@).
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
        type Sv UploadServerCertificate = IAM
        type Rs UploadServerCertificate =
             UploadServerCertificateResponse
        request = post
        response
          = receiveXMLWrapper "UploadServerCertificateResult"
              (\ s h x ->
                 UploadServerCertificateResponse' <$>
                   (x .@? "ServerCertificateMetadata") <*>
                     (pure (fromEnum s)))

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

-- | Contains the response to a successful UploadServerCertificate request.
--
-- /See:/ 'uploadServerCertificateResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uplServerCertificateMetadata'
--
-- * 'uplStatus'
data UploadServerCertificateResponse = UploadServerCertificateResponse'
    { _uplServerCertificateMetadata :: !(Maybe ServerCertificateMetadata)
    , _uplStatus                    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UploadServerCertificateResponse' smart constructor.
uploadServerCertificateResponse :: Int -> UploadServerCertificateResponse
uploadServerCertificateResponse pStatus =
    UploadServerCertificateResponse'
    { _uplServerCertificateMetadata = Nothing
    , _uplStatus = pStatus
    }

-- | The meta information of the uploaded server certificate without its
-- certificate body, certificate chain, and private key.
uplServerCertificateMetadata :: Lens' UploadServerCertificateResponse (Maybe ServerCertificateMetadata)
uplServerCertificateMetadata = lens _uplServerCertificateMetadata (\ s a -> s{_uplServerCertificateMetadata = a});

-- | FIXME: Undocumented member.
uplStatus :: Lens' UploadServerCertificateResponse Int
uplStatus = lens _uplStatus (\ s a -> s{_uplStatus = a});
