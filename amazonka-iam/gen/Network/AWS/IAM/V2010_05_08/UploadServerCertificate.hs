{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.UploadServerCertificate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Uploads a server certificate entity for the AWS account. The server
-- certificate entity includes a public key certificate, a private key, and an
-- optional certificate chain, which should all be PEM-encoded. For
-- information about the number of server certificates you can upload, see
-- Limitations on IAM Entities in the Using IAM guide. Because the body of the
-- public key certificate, private key, and the certificate chain can be
-- large, you should use POST rather than GET when calling
-- UploadServerCertificate. For information about setting up signatures and
-- authorization through the API, go to Signing AWS API Requests in the AWS
-- General Reference. For general information about using the Query API with
-- IAM, go to Making Query Requests in the Using IAM guide.
-- https://iam.amazonaws.com/ ?Action=UploadServerCertificate
-- &ServerCertificateName=ProdServerCert &Path=/company/servercerts/
-- &CertificateBody=-----BEGIN CERTIFICATE-----
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
-- AEaHzTpmEXAMPLE= -----END CERTIFICATE----- &PrivateKey=-----BEGIN DSA
-- PRIVATE KEY-----
-- MIIBugIBTTKBgQD33xToSXPJ6hr37L3+KNi3/7DgywlBcvlFPPSHIw3ORuO/22mT
-- 8Cy5fT89WwNvZ3BPKWU6OZ38TQv3eWjNc/3U3+oqVNG2poX5nCPOtO1b96HYX2mR
-- 3FTdH6FRKbQEhpDzZ6tRrjTHjMX6sT3JRWkBd2c4bGu+HUHO1H7QvrCTeQIVTKMs
-- TCKCyrLiGhUWuUGNJUMU6y6zToGTHl84Tz7TPwDGDXuy/Dk5s4jTVr+xibROC/gS
-- Qrs4Dzz3T1ze6lvU8S1KT9UsOB5FUJNTTPCPey+Lo4mmK6b23XdTyCIT8e2fsm2j
-- jHHC1pIPiTkdLS3j6ZYjF8LY6TENFng+LDY/xwPOl7TJVoD3J/WXC2J9CEYq9o34
-- kq6WWn3CgYTuo54nXUgnoCb3xdG8COFrg+oTbIkHTSzs3w5o/GGgKK7TDF3UlJjq
-- vHNyJQ6kWBrQRR1Xp5KYQ4c/Dm5kef+62mH53HpcCELguWVcffuVQpmq3EWL9Zp9
-- jobTJQ2VHjb5IVxiO6HRSd27di3njyrzUuJCyHSDTqwLJmTThpd6OTIUTL3Tc4m2
-- 62TITdw53KWJEXAMPLE= -----END DSA PRIVATE KEY----- &Version=2010-05-08
-- &AUTHPARAMS ProdServerCert /company/servercerts/
-- arn:aws:iam::123456789012:server-certificate/company/servercerts/ProdServerCert
-- 2010-05-08T01:02:03.004Z ASCACKCEVSQ6C2EXAMPLE 2012-05-08T01:02:03.004Z
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.UploadServerCertificate
    (
    -- * Request
      UploadServerCertificate
    -- ** Request constructor
    , mkUploadServerCertificate
    -- ** Request lenses
    , usc2Path
    , usc2ServerCertificateName
    , usc2CertificateBody
    , usc2PrivateKey
    , usc2CertificateChain

    -- * Response
    , UploadServerCertificateResponse
    -- ** Response lenses
    , uscrsServerCertificateMetadata
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | 
data UploadServerCertificate = UploadServerCertificate
    { _usc2Path :: Maybe Text
    , _usc2ServerCertificateName :: Text
    , _usc2CertificateBody :: Text
    , _usc2PrivateKey :: Text
    , _usc2CertificateChain :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UploadServerCertificate' request.
mkUploadServerCertificate :: Text -- ^ 'usc2ServerCertificateName'
                          -> Text -- ^ 'usc2CertificateBody'
                          -> Text -- ^ 'usc2PrivateKey'
                          -> UploadServerCertificate
mkUploadServerCertificate p2 p3 p4 = UploadServerCertificate
    { _usc2Path = Nothing
    , _usc2ServerCertificateName = p2
    , _usc2CertificateBody = p3
    , _usc2PrivateKey = p4
    , _usc2CertificateChain = Nothing
    }

-- | The path for the server certificate. For more information about paths, see
-- Identifiers for IAM Entities in the Using IAM guide. This parameter is
-- optional. If it is not included, it defaults to a slash (/). If you are
-- uploading a server certificate specifically for use with Amazon CloudFront
-- distributions, you must specify a path using the --path option. The path
-- must begin with /cloudfront and must include a trailing slash (for example,
-- /cloudfront/test/).
usc2Path :: Lens' UploadServerCertificate (Maybe Text)
usc2Path = lens _usc2Path (\s a -> s { _usc2Path = a })

-- | The name for the server certificate. Do not include the path in this value.
usc2ServerCertificateName :: Lens' UploadServerCertificate Text
usc2ServerCertificateName =
    lens _usc2ServerCertificateName
         (\s a -> s { _usc2ServerCertificateName = a })

-- | The contents of the public key certificate in PEM-encoded format.
usc2CertificateBody :: Lens' UploadServerCertificate Text
usc2CertificateBody =
    lens _usc2CertificateBody (\s a -> s { _usc2CertificateBody = a })

-- | The contents of the private key in PEM-encoded format.
usc2PrivateKey :: Lens' UploadServerCertificate Text
usc2PrivateKey = lens _usc2PrivateKey (\s a -> s { _usc2PrivateKey = a })

-- | The contents of the certificate chain. This is typically a concatenation of
-- the PEM-encoded public key certificates of the chain.
usc2CertificateChain :: Lens' UploadServerCertificate (Maybe Text)
usc2CertificateChain =
    lens _usc2CertificateChain (\s a -> s { _usc2CertificateChain = a })

instance ToQuery UploadServerCertificate where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the
-- UploadServerCertificate action.
newtype UploadServerCertificateResponse = UploadServerCertificateResponse
    { _uscrsServerCertificateMetadata :: Maybe ServerCertificateMetadata
    } deriving (Show, Generic)

-- | The meta information of the uploaded server certificate without its
-- certificate body, certificate chain, and private key.
uscrsServerCertificateMetadata :: Lens' UploadServerCertificateResponse (Maybe ServerCertificateMetadata)
uscrsServerCertificateMetadata =
    lens _uscrsServerCertificateMetadata
         (\s a -> s { _uscrsServerCertificateMetadata = a })

instance FromXML UploadServerCertificateResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest UploadServerCertificate where
    type Sv UploadServerCertificate = IAM
    type Rs UploadServerCertificate = UploadServerCertificateResponse

    request = post "UploadServerCertificate"
    response _ = xmlResponse
