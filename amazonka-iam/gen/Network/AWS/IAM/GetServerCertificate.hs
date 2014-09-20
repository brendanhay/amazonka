{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.GetServerCertificate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves information about the specified server certificate.
-- https://iam.amazonaws.com/ ?Action=GetServerCertificate
-- &ServerCertificateName=ProdServerCert &Version=2010-05-08 &AUTHPARAMS
-- ProdServerCert /company/servercerts/
-- arn:aws:iam::123456789012:server-certificate/company/servercerts/ProdServerCert
-- 2010-05-08T01:02:03.004Z ASCACKCEVSQ6C2EXAMPLE 2012-05-08T01:02:03.004Z
-- -----BEGIN CERTIFICATE-----
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
-- AEaHzTpmEXAMPLE= -----END CERTIFICATE-----
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.GetServerCertificate
    (
    -- * Request
      GetServerCertificate
    -- ** Request constructor
    , getServerCertificate
    -- ** Request lenses
    , gscServerCertificateName

    -- * Response
    , GetServerCertificateResponse
    -- ** Response constructor
    , getServerCertificateResponse
    -- ** Response lenses
    , gscrServerCertificate
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

newtype GetServerCertificate = GetServerCertificate
    { _gscServerCertificateName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetServerCertificate' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ServerCertificateName ::@ @Text@
--
getServerCertificate :: Text -- ^ 'gscServerCertificateName'
                     -> GetServerCertificate
getServerCertificate p1 = GetServerCertificate
    { _gscServerCertificateName = p1
    }

-- | The name of the server certificate you want to retrieve information about.
gscServerCertificateName :: Lens' GetServerCertificate Text
gscServerCertificateName =
    lens _gscServerCertificateName
         (\s a -> s { _gscServerCertificateName = a })

instance ToQuery GetServerCertificate where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the GetServerCertificate
-- action.
newtype GetServerCertificateResponse = GetServerCertificateResponse
    { _gscrServerCertificate :: ServerCertificate
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetServerCertificateResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ServerCertificate ::@ @ServerCertificate@
--
getServerCertificateResponse :: ServerCertificate -- ^ 'gscrServerCertificate'
                             -> GetServerCertificateResponse
getServerCertificateResponse p1 = GetServerCertificateResponse
    { _gscrServerCertificate = p1
    }

-- | Information about the server certificate.
gscrServerCertificate :: Lens' GetServerCertificateResponse ServerCertificate
gscrServerCertificate =
    lens _gscrServerCertificate (\s a -> s { _gscrServerCertificate = a })

instance FromXML GetServerCertificateResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetServerCertificate where
    type Sv GetServerCertificate = IAM
    type Rs GetServerCertificate = GetServerCertificateResponse

    request = post "GetServerCertificate"
    response _ = xmlResponse
