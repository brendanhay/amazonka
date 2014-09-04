{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.UploadSigningCertificate
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
module Network.AWS.IAM.V2010_05_08.UploadSigningCertificate
    (
    -- * Request
      UploadSigningCertificate
    -- ** Request constructor
    , uploadSigningCertificate
    -- ** Request lenses
    , uscvCertificateBody
    , uscvUserName

    -- * Response
    , UploadSigningCertificateResponse
    -- ** Response lenses
    , uscwCertificate
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'UploadSigningCertificate' request.
uploadSigningCertificate :: Text -- ^ 'uscvCertificateBody'
                         -> UploadSigningCertificate
uploadSigningCertificate p1 = UploadSigningCertificate
    { _uscvCertificateBody = p1
    , _uscvUserName = Nothing
    }
{-# INLINE uploadSigningCertificate #-}

data UploadSigningCertificate = UploadSigningCertificate
    { _uscvCertificateBody :: Text
      -- ^ The contents of the signing certificate.
    , _uscvUserName :: Maybe Text
      -- ^ Name of the user the signing certificate is for.
    } deriving (Show, Generic)

-- | The contents of the signing certificate.
uscvCertificateBody :: Lens' UploadSigningCertificate (Text)
uscvCertificateBody f x =
    f (_uscvCertificateBody x)
        <&> \y -> x { _uscvCertificateBody = y }
{-# INLINE uscvCertificateBody #-}

-- | Name of the user the signing certificate is for.
uscvUserName :: Lens' UploadSigningCertificate (Maybe Text)
uscvUserName f x =
    f (_uscvUserName x)
        <&> \y -> x { _uscvUserName = y }
{-# INLINE uscvUserName #-}

instance ToQuery UploadSigningCertificate where
    toQuery = genericQuery def

data UploadSigningCertificateResponse = UploadSigningCertificateResponse
    { _uscwCertificate :: SigningCertificate
      -- ^ Information about the certificate.
    } deriving (Show, Generic)

-- | Information about the certificate.
uscwCertificate :: Lens' UploadSigningCertificateResponse (SigningCertificate)
uscwCertificate f x =
    f (_uscwCertificate x)
        <&> \y -> x { _uscwCertificate = y }
{-# INLINE uscwCertificate #-}

instance FromXML UploadSigningCertificateResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest UploadSigningCertificate where
    type Sv UploadSigningCertificate = IAM
    type Rs UploadSigningCertificate = UploadSigningCertificateResponse

    request = post "UploadSigningCertificate"
    response _ = xmlResponse
