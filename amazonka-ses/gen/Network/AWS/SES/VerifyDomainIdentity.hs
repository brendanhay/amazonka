{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.SES.VerifyDomainIdentity
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Verifies a domain. This action is throttled at one request per second.
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

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SES.Types
import qualified GHC.Exts

newtype VerifyDomainIdentity = VerifyDomainIdentity
    { _vdiDomain :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'VerifyDomainIdentity' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vdiDomain' @::@ 'Text'
--
verifyDomainIdentity :: Text -- ^ 'vdiDomain'
                     -> VerifyDomainIdentity
verifyDomainIdentity p1 = VerifyDomainIdentity
    { _vdiDomain = p1
    }

-- | The domain to be verified.
vdiDomain :: Lens' VerifyDomainIdentity Text
vdiDomain = lens _vdiDomain (\s a -> s { _vdiDomain = a })

instance ToQuery VerifyDomainIdentity

instance ToPath VerifyDomainIdentity where
    toPath = const "/"

newtype VerifyDomainIdentityResponse = VerifyDomainIdentityResponse
    { _vdirVerificationToken :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'VerifyDomainIdentityResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vdirVerificationToken' @::@ 'Text'
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

instance AWSRequest VerifyDomainIdentity where
    type Sv VerifyDomainIdentity = SES
    type Rs VerifyDomainIdentity = VerifyDomainIdentityResponse

    request  = post "VerifyDomainIdentity"
    response = xmlResponse $ \h x -> VerifyDomainIdentityResponse
        <$> x %| "VerificationToken"
