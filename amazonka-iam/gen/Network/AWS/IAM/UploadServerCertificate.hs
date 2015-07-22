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
    , uscrqPath
    , uscrqCertificateChain
    , uscrqServerCertificateName
    , uscrqCertificateBody
    , uscrqPrivateKey

    -- * Response
    , UploadServerCertificateResponse
    -- ** Response constructor
    , uploadServerCertificateResponse
    -- ** Response lenses
    , ursServerCertificateMetadata
    , ursStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'uploadServerCertificate' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uscrqPath'
--
-- * 'uscrqCertificateChain'
--
-- * 'uscrqServerCertificateName'
--
-- * 'uscrqCertificateBody'
--
-- * 'uscrqPrivateKey'
data UploadServerCertificate = UploadServerCertificate'
    { _uscrqPath                  :: !(Maybe Text)
    , _uscrqCertificateChain      :: !(Maybe Text)
    , _uscrqServerCertificateName :: !Text
    , _uscrqCertificateBody       :: !Text
    , _uscrqPrivateKey            :: !(Sensitive Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UploadServerCertificate' smart constructor.
uploadServerCertificate :: Text -> Text -> Text -> UploadServerCertificate
uploadServerCertificate pServerCertificateName pCertificateBody pPrivateKey =
    UploadServerCertificate'
    { _uscrqPath = Nothing
    , _uscrqCertificateChain = Nothing
    , _uscrqServerCertificateName = pServerCertificateName
    , _uscrqCertificateBody = pCertificateBody
    , _uscrqPrivateKey = _Sensitive # pPrivateKey
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
uscrqPath :: Lens' UploadServerCertificate (Maybe Text)
uscrqPath = lens _uscrqPath (\ s a -> s{_uscrqPath = a});

-- | The contents of the certificate chain. This is typically a concatenation
-- of the PEM-encoded public key certificates of the chain.
uscrqCertificateChain :: Lens' UploadServerCertificate (Maybe Text)
uscrqCertificateChain = lens _uscrqCertificateChain (\ s a -> s{_uscrqCertificateChain = a});

-- | The name for the server certificate. Do not include the path in this
-- value. The name of the certificate cannot contain any spaces.
uscrqServerCertificateName :: Lens' UploadServerCertificate Text
uscrqServerCertificateName = lens _uscrqServerCertificateName (\ s a -> s{_uscrqServerCertificateName = a});

-- | The contents of the public key certificate in PEM-encoded format.
uscrqCertificateBody :: Lens' UploadServerCertificate Text
uscrqCertificateBody = lens _uscrqCertificateBody (\ s a -> s{_uscrqCertificateBody = a});

-- | The contents of the private key in PEM-encoded format.
uscrqPrivateKey :: Lens' UploadServerCertificate Text
uscrqPrivateKey = lens _uscrqPrivateKey (\ s a -> s{_uscrqPrivateKey = a}) . _Sensitive;

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
               "Path" =: _uscrqPath,
               "CertificateChain" =: _uscrqCertificateChain,
               "ServerCertificateName" =:
                 _uscrqServerCertificateName,
               "CertificateBody" =: _uscrqCertificateBody,
               "PrivateKey" =: _uscrqPrivateKey]

-- | Contains the response to a successful UploadServerCertificate request.
--
-- /See:/ 'uploadServerCertificateResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ursServerCertificateMetadata'
--
-- * 'ursStatus'
data UploadServerCertificateResponse = UploadServerCertificateResponse'
    { _ursServerCertificateMetadata :: !(Maybe ServerCertificateMetadata)
    , _ursStatus                    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UploadServerCertificateResponse' smart constructor.
uploadServerCertificateResponse :: Int -> UploadServerCertificateResponse
uploadServerCertificateResponse pStatus =
    UploadServerCertificateResponse'
    { _ursServerCertificateMetadata = Nothing
    , _ursStatus = pStatus
    }

-- | The meta information of the uploaded server certificate without its
-- certificate body, certificate chain, and private key.
ursServerCertificateMetadata :: Lens' UploadServerCertificateResponse (Maybe ServerCertificateMetadata)
ursServerCertificateMetadata = lens _ursServerCertificateMetadata (\ s a -> s{_ursServerCertificateMetadata = a});

-- | FIXME: Undocumented member.
ursStatus :: Lens' UploadServerCertificateResponse Int
ursStatus = lens _ursStatus (\ s a -> s{_ursStatus = a});
