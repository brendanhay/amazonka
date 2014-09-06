{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.V2010_12_01.VerifyDomainIdentity
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
module Network.AWS.SES.V2010_12_01.VerifyDomainIdentity
    (
    -- * Request
      VerifyDomainIdentity
    -- ** Request constructor
    , mkVerifyDomainIdentity
    -- ** Request lenses
    , vdiDomain

    -- * Response
    , VerifyDomainIdentityResponse
    -- ** Response lenses
    , vdirsVerificationToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.SES.V2010_12_01.Types
import Network.AWS.Prelude

-- | Represents a request instructing the service to begin domain verification.
newtype VerifyDomainIdentity = VerifyDomainIdentity
    { _vdiDomain :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'VerifyDomainIdentity' request.
mkVerifyDomainIdentity :: Text -- ^ 'vdiDomain'
                       -> VerifyDomainIdentity
mkVerifyDomainIdentity p1 = VerifyDomainIdentity
    { _vdiDomain = p1
    }
{-# INLINE mkVerifyDomainIdentity #-}

-- | The domain to be verified.
vdiDomain :: Lens' VerifyDomainIdentity Text
vdiDomain = lens _vdiDomain (\s a -> s { _vdiDomain = a })
{-# INLINE vdiDomain #-}

instance ToQuery VerifyDomainIdentity where
    toQuery = genericQuery def

-- | Represents a token used for domain ownership verification.
newtype VerifyDomainIdentityResponse = VerifyDomainIdentityResponse
    { _vdirsVerificationToken :: Text
    } deriving (Show, Generic)

-- | A TXT record that must be placed in the DNS settings for the domain, in
-- order to complete domain verification.
vdirsVerificationToken :: Lens' VerifyDomainIdentityResponse Text
vdirsVerificationToken =
    lens _vdirsVerificationToken (\s a -> s { _vdirsVerificationToken = a })
{-# INLINE vdirsVerificationToken #-}

instance FromXML VerifyDomainIdentityResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest VerifyDomainIdentity where
    type Sv VerifyDomainIdentity = SES
    type Rs VerifyDomainIdentity = VerifyDomainIdentityResponse

    request = post "VerifyDomainIdentity"
    response _ = xmlResponse
