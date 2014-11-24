{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.VerifyDomainDkim
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a set of DKIM tokens for a domain. DKIM /tokens/ are character
-- strings that represent your domain's identity. Using these tokens, you will
-- need to create DNS CNAME records that point to DKIM public keys hosted by
-- Amazon SES. Amazon Web Services will eventually detect that you have
-- updated your DNS records; this detection process may take up to 72 hours.
-- Upon successful detection, Amazon SES will be able to DKIM-sign email
-- originating from that domain. This action is throttled at one request per
-- second. To enable or disable Easy DKIM signing for a domain, use the
-- 'SetIdentityDkimEnabled' action. For more information about creating DNS
-- records using DKIM tokens, go to the
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim-dns-records.html
-- Amazon SES Developer Guide>.
--
-- <http://docs.aws.amazon.com/ses/latest/APIReference/API_VerifyDomainDkim.html>
module Network.AWS.SES.VerifyDomainDkim
    (
    -- * Request
      VerifyDomainDkim
    -- ** Request constructor
    , verifyDomainDkim
    -- ** Request lenses
    , vddDomain

    -- * Response
    , VerifyDomainDkimResponse
    -- ** Response constructor
    , verifyDomainDkimResponse
    -- ** Response lenses
    , vddrDkimTokens
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SES.Types
import qualified GHC.Exts

newtype VerifyDomainDkim = VerifyDomainDkim
    { _vddDomain :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'VerifyDomainDkim' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vddDomain' @::@ 'Text'
--
verifyDomainDkim :: Text -- ^ 'vddDomain'
                 -> VerifyDomainDkim
verifyDomainDkim p1 = VerifyDomainDkim
    { _vddDomain = p1
    }

-- | The name of the domain to be verified for Easy DKIM signing.
vddDomain :: Lens' VerifyDomainDkim Text
vddDomain = lens _vddDomain (\s a -> s { _vddDomain = a })

newtype VerifyDomainDkimResponse = VerifyDomainDkimResponse
    { _vddrDkimTokens :: List "DkimTokens" Text
    } deriving (Eq, Ord, Show, Monoid, Semigroup)

instance GHC.Exts.IsList VerifyDomainDkimResponse where
    type Item VerifyDomainDkimResponse = Text

    fromList = VerifyDomainDkimResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _vddrDkimTokens

-- | 'VerifyDomainDkimResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vddrDkimTokens' @::@ ['Text']
--
verifyDomainDkimResponse :: VerifyDomainDkimResponse
verifyDomainDkimResponse = VerifyDomainDkimResponse
    { _vddrDkimTokens = mempty
    }

-- | A set of character strings that represent the domain's identity. If the
-- identity is an email address, the tokens represent the domain of that
-- address. Using these tokens, you will need to create DNS CNAME records
-- that point to DKIM public keys hosted by Amazon SES. Amazon Web Services
-- will eventually detect that you have updated your DNS records; this
-- detection process may take up to 72 hours. Upon successful detection,
-- Amazon SES will be able to DKIM-sign emails originating from that domain.
-- For more information about creating DNS records using DKIM tokens, go to
-- the
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim-dns-records.html
-- Amazon SES Developer Guide>.
vddrDkimTokens :: Lens' VerifyDomainDkimResponse [Text]
vddrDkimTokens = lens _vddrDkimTokens (\s a -> s { _vddrDkimTokens = a }) . _List

instance ToPath VerifyDomainDkim where
    toPath = const "/"

instance ToQuery VerifyDomainDkim where
    toQuery VerifyDomainDkim{..} = mconcat
        [ "Domain" =? _vddDomain
        ]

instance ToHeaders VerifyDomainDkim

instance AWSRequest VerifyDomainDkim where
    type Sv VerifyDomainDkim = SES
    type Rs VerifyDomainDkim = VerifyDomainDkimResponse

    request  = post "VerifyDomainDkim"
    response = xmlResponse

instance FromXML VerifyDomainDkimResponse where
    parseXML = withElement "VerifyDomainDkimResult" $ \x -> VerifyDomainDkimResponse
        <$> x .@  "DkimTokens"
