{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.GetServerCertificate
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
module Network.AWS.IAM.V2010_05_08.GetServerCertificate
    (
    -- * Request
      GetServerCertificate
    -- ** Request constructor
    , getServerCertificate
    -- ** Request lenses
    , gscrServerCertificateName

    -- * Response
    , GetServerCertificateResponse
    -- ** Response lenses
    , gscsServerCertificate
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetServerCertificate' request.
getServerCertificate :: Text -- ^ 'gscrServerCertificateName'
                     -> GetServerCertificate
getServerCertificate p1 = GetServerCertificate
    { _gscrServerCertificateName = p1
    }

data GetServerCertificate = GetServerCertificate
    { _gscrServerCertificateName :: Text
      -- ^ The name of the server certificate you want to retrieve
      -- information about.
    } deriving (Show, Generic)

-- | The name of the server certificate you want to retrieve information about.
gscrServerCertificateName
    :: Functor f
    => (Text
    -> f (Text))
    -> GetServerCertificate
    -> f GetServerCertificate
gscrServerCertificateName f x =
    (\y -> x { _gscrServerCertificateName = y })
       <$> f (_gscrServerCertificateName x)
{-# INLINE gscrServerCertificateName #-}

instance ToQuery GetServerCertificate where
    toQuery = genericQuery def

data GetServerCertificateResponse = GetServerCertificateResponse
    { _gscsServerCertificate :: ServerCertificate
      -- ^ Information about the server certificate.
    } deriving (Show, Generic)

-- | Information about the server certificate.
gscsServerCertificate
    :: Functor f
    => (ServerCertificate
    -> f (ServerCertificate))
    -> GetServerCertificateResponse
    -> f GetServerCertificateResponse
gscsServerCertificate f x =
    (\y -> x { _gscsServerCertificate = y })
       <$> f (_gscsServerCertificate x)
{-# INLINE gscsServerCertificate #-}

instance FromXML GetServerCertificateResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetServerCertificate where
    type Sv GetServerCertificate = IAM
    type Rs GetServerCertificate = GetServerCertificateResponse

    request = post "GetServerCertificate"
    response _ = xmlResponse
