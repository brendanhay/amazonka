{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UploadSigningCertificate
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Uploads an X.509 signing certificate and associates it with the
-- specified user. Some AWS services use X.509 signing certificates to
-- validate requests that are signed with a corresponding private key. When
-- you upload the certificate, its default status is @Active@.
--
-- If the @UserName@ field is not specified, the user name is determined
-- implicitly based on the AWS access key ID used to sign the request.
-- Because this action works for access keys under the AWS account, you can
-- use this action to manage root credentials even if the AWS account has
-- no associated users.
--
-- Because the body of a X.509 certificate can be large, you should use
-- POST rather than GET when calling @UploadSigningCertificate@. For
-- information about setting up signatures and authorization through the
-- API, go to
-- <http://docs.aws.amazon.com/general/latest/gr/signing_aws_api_requests.html Signing AWS API Requests>
-- in the /AWS General Reference/. For general information about using the
-- Query API with IAM, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/IAM_UsingQueryAPI.html Making Query Requests>
-- in the /Using IAM/guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UploadSigningCertificate.html>
module Network.AWS.IAM.UploadSigningCertificate
    (
    -- * Request
      UploadSigningCertificate
    -- ** Request constructor
    , uploadSigningCertificate
    -- ** Request lenses
    , urqUserName
    , urqCertificateBody

    -- * Response
    , UploadSigningCertificateResponse
    -- ** Response constructor
    , uploadSigningCertificateResponse
    -- ** Response lenses
    , uscrsStatus
    , uscrsCertificate
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'uploadSigningCertificate' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'urqUserName'
--
-- * 'urqCertificateBody'
data UploadSigningCertificate = UploadSigningCertificate'
    { _urqUserName        :: !(Maybe Text)
    , _urqCertificateBody :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UploadSigningCertificate' smart constructor.
uploadSigningCertificate :: Text -> UploadSigningCertificate
uploadSigningCertificate pCertificateBody =
    UploadSigningCertificate'
    { _urqUserName = Nothing
    , _urqCertificateBody = pCertificateBody
    }

-- | The name of the user the signing certificate is for.
urqUserName :: Lens' UploadSigningCertificate (Maybe Text)
urqUserName = lens _urqUserName (\ s a -> s{_urqUserName = a});

-- | The contents of the signing certificate.
urqCertificateBody :: Lens' UploadSigningCertificate Text
urqCertificateBody = lens _urqCertificateBody (\ s a -> s{_urqCertificateBody = a});

instance AWSRequest UploadSigningCertificate where
        type Sv UploadSigningCertificate = IAM
        type Rs UploadSigningCertificate =
             UploadSigningCertificateResponse
        request = post
        response
          = receiveXMLWrapper "UploadSigningCertificateResult"
              (\ s h x ->
                 UploadSigningCertificateResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "Certificate"))

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
               "UserName" =: _urqUserName,
               "CertificateBody" =: _urqCertificateBody]

-- | Contains the response to a successful UploadSigningCertificate request.
--
-- /See:/ 'uploadSigningCertificateResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uscrsStatus'
--
-- * 'uscrsCertificate'
data UploadSigningCertificateResponse = UploadSigningCertificateResponse'
    { _uscrsStatus      :: !Int
    , _uscrsCertificate :: !SigningCertificate
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UploadSigningCertificateResponse' smart constructor.
uploadSigningCertificateResponse :: Int -> SigningCertificate -> UploadSigningCertificateResponse
uploadSigningCertificateResponse pStatus pCertificate =
    UploadSigningCertificateResponse'
    { _uscrsStatus = pStatus
    , _uscrsCertificate = pCertificate
    }

-- | FIXME: Undocumented member.
uscrsStatus :: Lens' UploadSigningCertificateResponse Int
uscrsStatus = lens _uscrsStatus (\ s a -> s{_uscrsStatus = a});

-- | Information about the certificate.
uscrsCertificate :: Lens' UploadSigningCertificateResponse SigningCertificate
uscrsCertificate = lens _uscrsCertificate (\ s a -> s{_uscrsCertificate = a});
