{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.ListSigningCertificates
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
module Network.AWS.IAM.ListSigningCertificates
    (
    -- * Request
      ListSigningCertificates
    -- ** Request constructor
    , mkListSigningCertificates
    -- ** Request lenses
    , lsc1UserName
    , lsc1Marker
    , lsc1MaxItems

    -- * Response
    , ListSigningCertificatesResponse
    -- ** Response constructor
    , mkListSigningCertificatesResponse
    -- ** Response lenses
    , lscrrCertificates
    , lscrrIsTruncated
    , lscrrMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data ListSigningCertificates = ListSigningCertificates
    { _lsc1UserName :: !(Maybe Text)
    , _lsc1Marker :: !(Maybe Text)
    , _lsc1MaxItems :: !(Maybe Integer)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListSigningCertificates' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @UserName ::@ @Maybe Text@
--
-- * @Marker ::@ @Maybe Text@
--
-- * @MaxItems ::@ @Maybe Integer@
--
mkListSigningCertificates :: ListSigningCertificates
mkListSigningCertificates = ListSigningCertificates
    { _lsc1UserName = Nothing
    , _lsc1Marker = Nothing
    , _lsc1MaxItems = Nothing
    }

-- | The name of the user.
lsc1UserName :: Lens' ListSigningCertificates (Maybe Text)
lsc1UserName = lens _lsc1UserName (\s a -> s { _lsc1UserName = a })

-- | Use this only when paginating results, and only in a subsequent request
-- after you've received a response where the results are truncated. Set it to
-- the value of the Marker element in the response you just received.
lsc1Marker :: Lens' ListSigningCertificates (Maybe Text)
lsc1Marker = lens _lsc1Marker (\s a -> s { _lsc1Marker = a })

-- | Use this only when paginating results to indicate the maximum number of
-- certificate IDs you want in the response. If there are additional
-- certificate IDs beyond the maximum you specify, the IsTruncated response
-- element is true. This parameter is optional. If you do not include it, it
-- defaults to 100.
lsc1MaxItems :: Lens' ListSigningCertificates (Maybe Integer)
lsc1MaxItems = lens _lsc1MaxItems (\s a -> s { _lsc1MaxItems = a })

instance ToQuery ListSigningCertificates where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the
-- ListSigningCertificates action.
data ListSigningCertificatesResponse = ListSigningCertificatesResponse
    { _lscrrCertificates :: [SigningCertificate]
    , _lscrrIsTruncated :: !Bool
    , _lscrrMarker :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListSigningCertificatesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Certificates ::@ @[SigningCertificate]@
--
-- * @IsTruncated ::@ @Bool@
--
-- * @Marker ::@ @Maybe Text@
--
mkListSigningCertificatesResponse :: [SigningCertificate] -- ^ 'lscrrCertificates'
                                  -> Bool -- ^ 'lscrrIsTruncated'
                                  -> ListSigningCertificatesResponse
mkListSigningCertificatesResponse p1 p2 = ListSigningCertificatesResponse
    { _lscrrCertificates = p1
    , _lscrrIsTruncated = p2
    , _lscrrMarker = Nothing
    }

-- | A list of the user's signing certificate information.
lscrrCertificates :: Lens' ListSigningCertificatesResponse [SigningCertificate]
lscrrCertificates =
    lens _lscrrCertificates (\s a -> s { _lscrrCertificates = a })

-- | A flag that indicates whether there are more certificate IDs to list. If
-- your results were truncated, you can make a subsequent pagination request
-- using the Marker request parameter to retrieve more certificates in the
-- list.
lscrrIsTruncated :: Lens' ListSigningCertificatesResponse Bool
lscrrIsTruncated =
    lens _lscrrIsTruncated (\s a -> s { _lscrrIsTruncated = a })

-- | If IsTruncated is true, this element is present and contains the value to
-- use for the Marker parameter in a subsequent pagination request.
lscrrMarker :: Lens' ListSigningCertificatesResponse (Maybe Text)
lscrrMarker = lens _lscrrMarker (\s a -> s { _lscrrMarker = a })

instance FromXML ListSigningCertificatesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListSigningCertificates where
    type Sv ListSigningCertificates = IAM
    type Rs ListSigningCertificates = ListSigningCertificatesResponse

    request = post "ListSigningCertificates"
    response _ = xmlResponse

instance AWSPager ListSigningCertificates where
    next rq rs
        | not (rs ^. lscrrIsTruncated) = Nothing
        | otherwise = Just $
            rq & lsc1Marker .~ rs ^. lscrrMarker
