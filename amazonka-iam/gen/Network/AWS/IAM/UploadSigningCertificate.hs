{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.UploadSigningCertificate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Uploads an X.509 signing certificate and associates it with the specified
-- user. Some AWS services use X.509 signing certificates to validate requests
-- that are signed with a corresponding private key. When you upload the
-- certificate, its default status is 'Active'.
--
-- If the 'UserName' field is not specified, the user name is determined
-- implicitly based on the AWS access key ID used to sign the request. Because
-- this action works for access keys under the AWS account, you can use this
-- action to manage root credentials even if the AWS account has no associated
-- users.
--
-- Because the body of a X.509 certificate can be large, you should use POST
-- rather than GET when calling 'UploadSigningCertificate'. For information about
-- setting up signatures and authorization through the API, go to <http://docs.aws.amazon.com/general/latest/gr/signing_aws_api_requests.html Signing AWSAPI Requests> in the /AWS General Reference/. For general information about
-- using the Query API with IAM, go to <http://docs.aws.amazon.com/IAM/latest/UserGuide/IAM_UsingQueryAPI.html Making Query Requests> in the /Using IAM/guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UploadSigningCertificate.html>
module Network.AWS.IAM.UploadSigningCertificate
    (
    -- * Request
      UploadSigningCertificate
    -- ** Request constructor
    , uploadSigningCertificate
    -- ** Request lenses
    , usc1CertificateBody
    , usc1UserName

    -- * Response
    , UploadSigningCertificateResponse
    -- ** Response constructor
    , uploadSigningCertificateResponse
    -- ** Response lenses
    , uscrCertificate
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data UploadSigningCertificate = UploadSigningCertificate
    { _usc1CertificateBody :: Text
    , _usc1UserName        :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'UploadSigningCertificate' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'usc1CertificateBody' @::@ 'Text'
--
-- * 'usc1UserName' @::@ 'Maybe' 'Text'
--
uploadSigningCertificate :: Text -- ^ 'usc1CertificateBody'
                         -> UploadSigningCertificate
uploadSigningCertificate p1 = UploadSigningCertificate
    { _usc1CertificateBody = p1
    , _usc1UserName        = Nothing
    }

-- | The contents of the signing certificate.
usc1CertificateBody :: Lens' UploadSigningCertificate Text
usc1CertificateBody =
    lens _usc1CertificateBody (\s a -> s { _usc1CertificateBody = a })

-- | The name of the user the signing certificate is for.
usc1UserName :: Lens' UploadSigningCertificate (Maybe Text)
usc1UserName = lens _usc1UserName (\s a -> s { _usc1UserName = a })

newtype UploadSigningCertificateResponse = UploadSigningCertificateResponse
    { _uscrCertificate :: SigningCertificate
    } deriving (Eq, Read, Show)

-- | 'UploadSigningCertificateResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uscrCertificate' @::@ 'SigningCertificate'
--
uploadSigningCertificateResponse :: SigningCertificate -- ^ 'uscrCertificate'
                                 -> UploadSigningCertificateResponse
uploadSigningCertificateResponse p1 = UploadSigningCertificateResponse
    { _uscrCertificate = p1
    }

-- | Information about the certificate.
uscrCertificate :: Lens' UploadSigningCertificateResponse SigningCertificate
uscrCertificate = lens _uscrCertificate (\s a -> s { _uscrCertificate = a })

instance ToPath UploadSigningCertificate where
    toPath = const "/"

instance ToQuery UploadSigningCertificate where
    toQuery UploadSigningCertificate{..} = mconcat
        [ "CertificateBody" =? _usc1CertificateBody
        , "UserName"        =? _usc1UserName
        ]

instance ToHeaders UploadSigningCertificate

instance AWSRequest UploadSigningCertificate where
    type Sv UploadSigningCertificate = IAM
    type Rs UploadSigningCertificate = UploadSigningCertificateResponse

    request  = post "UploadSigningCertificate"
    response = xmlResponse

instance FromXML UploadSigningCertificateResponse where
    parseXML = withElement "UploadSigningCertificateResult" $ \x -> UploadSigningCertificateResponse
        <$> x .@  "Certificate"
