{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.ListSigningCertificates
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about the signing certificates associated with the
-- specified user. If there are none, the action returns an empty list.
-- Although each user is limited to a small number of signing certificates,
-- you can still paginate the results using the MaxItems and Marker
-- parameters. If the UserName field is not specified, the user name is
-- determined implicitly based on the AWS access key ID used to sign the
-- request. Because this action works for access keys under the AWS account,
-- this API can be used to manage root credentials even if the AWS account has
-- no associated users. https://iam.amazonaws.com/
-- ?Action=ListSigningCertificates &UserName=Bob &Version=2010-05-08
-- &AUTHPARAMS Bob Bob TA7SMP42TDN5Z26OBPJE7EXAMPLE -----BEGIN
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
-- AEaHzTpmEXAMPLE= -----END CERTIFICATE----- Active false
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.ListSigningCertificates
    (
    -- * Request
      ListSigningCertificates
    -- ** Request constructor
    , listSigningCertificates
    -- ** Request lenses
    , lsctUserName
    , lsctMarker
    , lsctMaxItems

    -- * Response
    , ListSigningCertificatesResponse
    -- ** Response lenses
    , lscuIsTruncated
    , lscuCertificates
    , lscuMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListSigningCertificates' request.
listSigningCertificates :: ListSigningCertificates
listSigningCertificates = ListSigningCertificates
    { _lsctUserName = Nothing
    , _lsctMarker = Nothing
    , _lsctMaxItems = Nothing
    }

data ListSigningCertificates = ListSigningCertificates
    { _lsctUserName :: Maybe Text
      -- ^ The name of the user.
    , _lsctMarker :: Maybe Text
      -- ^ Use this only when paginating results, and only in a subsequent
      -- request after you've received a response where the results are
      -- truncated. Set it to the value of the Marker element in the
      -- response you just received.
    , _lsctMaxItems :: Maybe Integer
      -- ^ Use this only when paginating results to indicate the maximum
      -- number of certificate IDs you want in the response. If there are
      -- additional certificate IDs beyond the maximum you specify, the
      -- IsTruncated response element is true. This parameter is optional.
      -- If you do not include it, it defaults to 100.
    } deriving (Show, Generic)

-- | The name of the user.
lsctUserName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListSigningCertificates
    -> f ListSigningCertificates
lsctUserName f x =
    (\y -> x { _lsctUserName = y })
       <$> f (_lsctUserName x)
{-# INLINE lsctUserName #-}

-- | Use this only when paginating results, and only in a subsequent request
-- after you've received a response where the results are truncated. Set it to
-- the value of the Marker element in the response you just received.
lsctMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListSigningCertificates
    -> f ListSigningCertificates
lsctMarker f x =
    (\y -> x { _lsctMarker = y })
       <$> f (_lsctMarker x)
{-# INLINE lsctMarker #-}

-- | Use this only when paginating results to indicate the maximum number of
-- certificate IDs you want in the response. If there are additional
-- certificate IDs beyond the maximum you specify, the IsTruncated response
-- element is true. This parameter is optional. If you do not include it, it
-- defaults to 100.
lsctMaxItems
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ListSigningCertificates
    -> f ListSigningCertificates
lsctMaxItems f x =
    (\y -> x { _lsctMaxItems = y })
       <$> f (_lsctMaxItems x)
{-# INLINE lsctMaxItems #-}

instance ToQuery ListSigningCertificates where
    toQuery = genericQuery def

data ListSigningCertificatesResponse = ListSigningCertificatesResponse
    { _lscuIsTruncated :: Bool
      -- ^ A flag that indicates whether there are more certificate IDs to
      -- list. If your results were truncated, you can make a subsequent
      -- pagination request using the Marker request parameter to retrieve
      -- more certificates in the list.
    , _lscuCertificates :: [SigningCertificate]
      -- ^ A list of the user's signing certificate information.
    , _lscuMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value to use for the Marker parameter in a subsequent pagination
      -- request.
    } deriving (Show, Generic)

-- | A flag that indicates whether there are more certificate IDs to list. If
-- your results were truncated, you can make a subsequent pagination request
-- using the Marker request parameter to retrieve more certificates in the
-- list.
lscuIsTruncated
    :: Functor f
    => (Bool
    -> f (Bool))
    -> ListSigningCertificatesResponse
    -> f ListSigningCertificatesResponse
lscuIsTruncated f x =
    (\y -> x { _lscuIsTruncated = y })
       <$> f (_lscuIsTruncated x)
{-# INLINE lscuIsTruncated #-}

-- | A list of the user's signing certificate information.
lscuCertificates
    :: Functor f
    => ([SigningCertificate]
    -> f ([SigningCertificate]))
    -> ListSigningCertificatesResponse
    -> f ListSigningCertificatesResponse
lscuCertificates f x =
    (\y -> x { _lscuCertificates = y })
       <$> f (_lscuCertificates x)
{-# INLINE lscuCertificates #-}

-- | If IsTruncated is true, this element is present and contains the value to
-- use for the Marker parameter in a subsequent pagination request.
lscuMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListSigningCertificatesResponse
    -> f ListSigningCertificatesResponse
lscuMarker f x =
    (\y -> x { _lscuMarker = y })
       <$> f (_lscuMarker x)
{-# INLINE lscuMarker #-}

instance FromXML ListSigningCertificatesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListSigningCertificates where
    type Sv ListSigningCertificates = IAM
    type Rs ListSigningCertificates = ListSigningCertificatesResponse

    request = post "ListSigningCertificates"
    response _ = xmlResponse

instance AWSPager ListSigningCertificates where
    next rq rs
        | not (_lscuIsTruncated rs) = Nothing
        | otherwise = Just $ rq
            { _lsctMarker = _lscuMarker rs
            }
