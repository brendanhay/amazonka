{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.IAM.UploadSigningCertificate
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Uploads an X.509 signing certificate and associates it with the
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
    , uplUserName
    , uplCertificateBody

    -- * Response
    , UploadSigningCertificateResponse
    -- ** Response constructor
    , uploadSigningCertificateResponse
    -- ** Response lenses
    , uscrCertificate
    , uscrStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'uploadSigningCertificate' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uplUserName'
--
-- * 'uplCertificateBody'
data UploadSigningCertificate = UploadSigningCertificate'
    { _uplUserName        :: Maybe Text
    , _uplCertificateBody :: Text
    } deriving (Eq,Read,Show)

-- | 'UploadSigningCertificate' smart constructor.
uploadSigningCertificate :: Text -> UploadSigningCertificate
uploadSigningCertificate pCertificateBody =
    UploadSigningCertificate'
    { _uplUserName = Nothing
    , _uplCertificateBody = pCertificateBody
    }

-- | The name of the user the signing certificate is for.
uplUserName :: Lens' UploadSigningCertificate (Maybe Text)
uplUserName = lens _uplUserName (\ s a -> s{_uplUserName = a});

-- | The contents of the signing certificate.
uplCertificateBody :: Lens' UploadSigningCertificate Text
uplCertificateBody = lens _uplCertificateBody (\ s a -> s{_uplCertificateBody = a});

instance AWSRequest UploadSigningCertificate where
        type Sv UploadSigningCertificate = IAM
        type Rs UploadSigningCertificate =
             UploadSigningCertificateResponse
        request = post
        response
          = receiveXMLWrapper "UploadSigningCertificateResult"
              (\ s h x ->
                 UploadSigningCertificateResponse' <$>
                   (x .@ "Certificate") <*> (pure (fromEnum s)))

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

-- | Contains the response to a successful UploadSigningCertificate request.
--
-- /See:/ 'uploadSigningCertificateResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uscrCertificate'
--
-- * 'uscrStatus'
data UploadSigningCertificateResponse = UploadSigningCertificateResponse'
    { _uscrCertificate :: SigningCertificate
    , _uscrStatus      :: !Int
    } deriving (Eq,Read,Show)

-- | 'UploadSigningCertificateResponse' smart constructor.
uploadSigningCertificateResponse :: SigningCertificate -> Int -> UploadSigningCertificateResponse
uploadSigningCertificateResponse pCertificate pStatus =
    UploadSigningCertificateResponse'
    { _uscrCertificate = pCertificate
    , _uscrStatus = pStatus
    }

-- | Information about the certificate.
uscrCertificate :: Lens' UploadSigningCertificateResponse SigningCertificate
uscrCertificate = lens _uscrCertificate (\ s a -> s{_uscrCertificate = a});

-- | FIXME: Undocumented member.
uscrStatus :: Lens' UploadSigningCertificateResponse Int
uscrStatus = lens _uscrStatus (\ s a -> s{_uscrStatus = a});
