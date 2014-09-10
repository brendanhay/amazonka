{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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

-- | Uploads an X.509 signing certificate and associates it with the specified
-- user. Some AWS services use X.509 signing certificates to validate requests
-- that are signed with a corresponding private key. When you upload the
-- certificate, its default status is Active. If the UserName field is not
-- specified, the user name is determined implicitly based on the AWS access
-- key ID used to sign the request. Because this action works for access keys
-- under the AWS account, this API can be used to manage root credentials even
-- if the AWS account has no associated users. Because the body of a X.509
-- certificate can be large, you should use POST rather than GET when calling
-- UploadSigningCertificate. For information about setting up signatures and
-- authorization through the API, go to Signing AWS API Requests in the AWS
-- General Reference. For general information about using the Query API with
-- IAM, go to Making Query Requests in the Using IAMguide. POST / HTTP/1.1
-- Host: iam.amazonaws.com Content-Type: application/x-www-form-urlencoded
-- Action=UploadSigningCertificate &UserName=Bob &CertificateBody=-----BEGIN
-- CERTIFICATE-----
-- MIICdzCCAeCgAwIBAgIGANc+Ha2wMA0GCSqGSIb3DQEBBQUAMFMxCzAJBgNVBAYT
-- AlVTMRMwEQYDVQQKEwpBbWF6b24uY29tMQwwCgYDVQQLEwNBV1MxITAfBgNVBAMT
-- GEFXUyBMaW1pdGVkLUFzc3VyYW5jZSBDQTAeFw0wOTAyMDQxNzE5MjdaFw0xMDAy
-- MDQxNzE5MjdaMFIxCzAJBgNVBAYTAlVTMRMwEQYDVQQKEwpBbWF6b24uY29tMRcw
-- FQYDVQQLEw5BV1MtRGV2ZWxvcGVyczEVMBMGA1UEAxMMNTdxNDl0c3ZwYjRtMIGf
-- MA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQCpB/vsOwmT/O0td1RqzKjttSBaPjbr
-- dqwNe9BrOyB08fw2+Ch5oonZYXfGUrT6mkYXH5fQot9HvASrzAKHO596FdJA6DmL
-- ywdWe1Oggk7zFSXO1Xv+3vPrJtaYxYo3eRIp7w80PMkiOv6M0XK8ubcTouODeJbf
-- suDqcLnLDxwsvwIDAQABo1cwVTAOBgNVHQ8BAf8EBAMCBaAwFgYDVR0lAQH/BAww
-- CgYIKwYBBQUHAwIwDAYDVR0TAQH/BAIwADAdBgNVHQ4EFgQULGNaBphBumaKbDRK
-- CAi0mH8B3mowDQYJKoZIhvcNAQEFBQADgYEAuKxhkXaCLGcqDuweKtO/AEw9ZePH
-- wr0XqsaIK2HZboqruebXEGsojK4Ks0WzwgrEynuHJwTn760xe39rSqXWIOGrOBaX
-- wFpWHVjTFMKk+tSDG1lssLHyYWWdFFU4AnejRGORJYNaRHgVTKjHphc5jEhHm0BX
-- AEaHzTpmEXAMPLE= -----END CERTIFICATE----- &Version=2010-05-08 &AUTHPARAMS
-- Bob TA7SMP42TDN5Z26OBPJE7EXAMPLE -----BEGIN CERTIFICATE-----
-- MIICdzCCAeCgAwIBAgIGANc+Ha2wMA0GCSqGSIb3DQEBBQUAMFMxCzAJBgNVBAYT
-- AlVTMRMwEQYDVQQKEwpBbWF6b24uY29tMQwwCgYDVQQLEwNBV1MxITAfBgNVBAMT
-- GEFXUyBMaW1pdGVkLUFzc3VyYW5jZSBDQTAeFw0wOTAyMDQxNzE5MjdaFw0xMDAy
-- MDQxNzE5MjdaMFIxCzAJBgNVBAYTAlVTMRMwEQYDVQQKEwpBbWF6b24uY29tMRcw
-- FQYDVQQLEw5BV1MtRGV2ZWxvcGVyczEVMBMGA1UEAxMMNTdxNDl0c3ZwYjRtMIGf
-- MA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQCpB/vsOwmT/O0td1RqzKjttSBaPjbr
-- dqwNe9BrOyB08fw2+Ch5oonZYXfGUrT6mkYXH5fQot9HvASrzAKHO596FdJA6DmL
-- ywdWe1Oggk7zFSXO1Xv+3vPrJtaYxYo3eRIp7w80PMkiOv6M0XK8ubcTouODeJbf
-- suDqcLnLDxwsvwIDAQABo1cwVTAOBgNVHQ8BAf8EBAMCBaAwFgYDVR0lAQH/BAww
-- CgYIKwYBBQUHAwIwDAYDVR0TAQH/BAIwADAdBgNVHQ4EFgQULGNaBphBumaKbDRK
-- CAi0mH8B3mowDQYJKoZIhvcNAQEFBQADgYEAuKxhkXaCLGcqDuweKtO/AEw9ZePH
-- wr0XqsaIK2HZboqruebXEGsojK4Ks0WzwgrEynuHJwTn760xe39rSqXWIOGrOBaX
-- wFpWHVjTFMKk+tSDG1lssLHyYWWdFFU4AnejRGORJYNaRHgVTKjHphc5jEhHm0BX
-- AEaHzTpmEXAMPLE= -----END CERTIFICATE----- Active
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM
    (
    -- * Request
      UploadSigningCertificate
    -- ** Request constructor
    , mkUploadSigningCertificate
    -- ** Request lenses
    , usc3UserName
    , usc3CertificateBody

    -- * Response
    , UploadSigningCertificateResponse
    -- ** Response constructor
    , mkUploadSigningCertificateResponse
    -- ** Response lenses
    , uscrrCertificate
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data UploadSigningCertificate = UploadSigningCertificate
    { _usc3UserName :: !(Maybe Text)
    , _usc3CertificateBody :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UploadSigningCertificate' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @UserName ::@ @Maybe Text@
--
-- * @CertificateBody ::@ @Text@
--
mkUploadSigningCertificate :: Text -- ^ 'usc3CertificateBody'
                           -> UploadSigningCertificate
mkUploadSigningCertificate p2 = UploadSigningCertificate
    { _usc3UserName = Nothing
    , _usc3CertificateBody = p2
    }

-- | Name of the user the signing certificate is for.
usc3UserName :: Lens' UploadSigningCertificate (Maybe Text)
usc3UserName = lens _usc3UserName (\s a -> s { _usc3UserName = a })

-- | The contents of the signing certificate.
usc3CertificateBody :: Lens' UploadSigningCertificate Text
usc3CertificateBody =
    lens _usc3CertificateBody (\s a -> s { _usc3CertificateBody = a })

instance ToQuery UploadSigningCertificate where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the
-- UploadSigningCertificate action.
newtype UploadSigningCertificateResponse = UploadSigningCertificateResponse
    { _uscrrCertificate :: SigningCertificate
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UploadSigningCertificateResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Certificate ::@ @SigningCertificate@
--
mkUploadSigningCertificateResponse :: SigningCertificate -- ^ 'uscrrCertificate'
                                   -> UploadSigningCertificateResponse
mkUploadSigningCertificateResponse p1 = UploadSigningCertificateResponse
    { _uscrrCertificate = p1
    }

-- | Information about the certificate.
uscrrCertificate :: Lens' UploadSigningCertificateResponse SigningCertificate
uscrrCertificate =
    lens _uscrrCertificate (\s a -> s { _uscrrCertificate = a })

instance FromXML UploadSigningCertificateResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest UploadSigningCertificate where
    type Sv UploadSigningCertificate = IAM
    type Rs UploadSigningCertificate = UploadSigningCertificateResponse

    request = post "UploadSigningCertificate"
    response _ = xmlResponse
