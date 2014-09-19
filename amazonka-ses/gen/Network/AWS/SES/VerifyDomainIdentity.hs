{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.VerifyDomainIdentity
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Verifies a domain. This action is throttled at one request per second. POST
-- / HTTP/1.1 Date: Sat, 12 May 2012 05:24:02 GMT Host:
-- email.us-east-1.amazonaws.com Content-Type:
-- application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=Wr+6RCfV+QgjLki2dtIrlecMK9+RrsDaTG5uWneDAu8=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 139
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=VerifyDomainIdentity
-- &Domain=domain.com &Timestamp=2012-05-12T05%3A24%3A02.000Z
-- &Version=2010-12-01 QTKknzFg2J4ygwa+XvHAxUl1hyHoY0gVfZdfjIedHZ0=
-- 94f6368e-9bf2-11e1-8ee7-c98a0037a2b6.
module Network.AWS.SES.VerifyDomainIdentity
    (
    -- * Request
      VerifyDomainIdentity
    -- ** Request constructor
    , verifyDomainIdentity
    -- ** Request lenses
    , vdiDomain

    -- * Response
    , VerifyDomainIdentityResponse
    -- ** Response constructor
    , verifyDomainIdentityResponse
    -- ** Response lenses
    , vdirVerificationToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.SES.Types
import Network.AWS.Prelude

-- | Represents a request instructing the service to begin domain verification.
newtype VerifyDomainIdentity = VerifyDomainIdentity
    { _vdiDomain :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'VerifyDomainIdentity' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Domain ::@ @Text@
--
verifyDomainIdentity :: Text -- ^ 'vdiDomain'
                     -> VerifyDomainIdentity
verifyDomainIdentity p1 = VerifyDomainIdentity
    { _vdiDomain = p1
    }

-- | The domain to be verified.
vdiDomain :: Lens' VerifyDomainIdentity Text
vdiDomain = lens _vdiDomain (\s a -> s { _vdiDomain = a })

instance ToQuery VerifyDomainIdentity where
    toQuery = genericQuery def

-- | Represents a token used for domain ownership verification.
newtype VerifyDomainIdentityResponse = VerifyDomainIdentityResponse
    { _vdirVerificationToken :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'VerifyDomainIdentityResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VerificationToken ::@ @Text@
--
verifyDomainIdentityResponse :: Text -- ^ 'vdirVerificationToken'
                             -> VerifyDomainIdentityResponse
verifyDomainIdentityResponse p1 = VerifyDomainIdentityResponse
    { _vdirVerificationToken = p1
    }

-- | A TXT record that must be placed in the DNS settings for the domain, in
-- order to complete domain verification.
vdirVerificationToken :: Lens' VerifyDomainIdentityResponse Text
vdirVerificationToken =
    lens _vdirVerificationToken (\s a -> s { _vdirVerificationToken = a })

instance FromXML VerifyDomainIdentityResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest VerifyDomainIdentity where
    type Sv VerifyDomainIdentity = SES
    type Rs VerifyDomainIdentity = VerifyDomainIdentityResponse

    request = post "VerifyDomainIdentity"
    response _ = xmlResponse
