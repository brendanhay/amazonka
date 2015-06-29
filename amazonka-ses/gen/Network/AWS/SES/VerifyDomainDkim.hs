{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SES.VerifyDomainDkim
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns a set of DKIM tokens for a domain. DKIM /tokens/ are character
-- strings that represent your domain\'s identity. Using these tokens, you
-- will need to create DNS CNAME records that point to DKIM public keys
-- hosted by Amazon SES. Amazon Web Services will eventually detect that
-- you have updated your DNS records; this detection process may take up to
-- 72 hours. Upon successful detection, Amazon SES will be able to
-- DKIM-sign email originating from that domain.
--
-- This action is throttled at one request per second.
--
-- To enable or disable Easy DKIM signing for a domain, use the
-- @SetIdentityDkimEnabled@ action.
--
-- For more information about creating DNS records using DKIM tokens, go to
-- the
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim-dns-records.html Amazon SES Developer Guide>.
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
    , vddrStatus
    , vddrDkimTokens
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types

-- | Represents a request instructing the service to begin DKIM verification
-- for a domain.
--
-- /See:/ 'verifyDomainDkim' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vddDomain'
newtype VerifyDomainDkim = VerifyDomainDkim'
    { _vddDomain :: Text
    } deriving (Eq,Read,Show)

-- | 'VerifyDomainDkim' smart constructor.
verifyDomainDkim :: Text -> VerifyDomainDkim
verifyDomainDkim pDomain =
    VerifyDomainDkim'
    { _vddDomain = pDomain
    }

-- | The name of the domain to be verified for Easy DKIM signing.
vddDomain :: Lens' VerifyDomainDkim Text
vddDomain = lens _vddDomain (\ s a -> s{_vddDomain = a});

instance AWSRequest VerifyDomainDkim where
        type Sv VerifyDomainDkim = SES
        type Rs VerifyDomainDkim = VerifyDomainDkimResponse
        request = post
        response
          = receiveXMLWrapper "VerifyDomainDkimResult"
              (\ s h x ->
                 VerifyDomainDkimResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .@? "DkimTokens" .!@ mempty >>=
                        parseXMLList "member"))

instance ToHeaders VerifyDomainDkim where
        toHeaders = const mempty

instance ToPath VerifyDomainDkim where
        toPath = const "/"

instance ToQuery VerifyDomainDkim where
        toQuery VerifyDomainDkim'{..}
          = mconcat
              ["Action" =: ("VerifyDomainDkim" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "Domain" =: _vddDomain]

-- | Represents the DNS records that must be published in the domain name\'s
-- DNS to complete DKIM setup.
--
-- /See:/ 'verifyDomainDkimResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vddrStatus'
--
-- * 'vddrDkimTokens'
data VerifyDomainDkimResponse = VerifyDomainDkimResponse'
    { _vddrStatus     :: !Int
    , _vddrDkimTokens :: ![Text]
    } deriving (Eq,Read,Show)

-- | 'VerifyDomainDkimResponse' smart constructor.
verifyDomainDkimResponse :: Int -> VerifyDomainDkimResponse
verifyDomainDkimResponse pStatus =
    VerifyDomainDkimResponse'
    { _vddrStatus = pStatus
    , _vddrDkimTokens = mempty
    }

-- | FIXME: Undocumented member.
vddrStatus :: Lens' VerifyDomainDkimResponse Int
vddrStatus = lens _vddrStatus (\ s a -> s{_vddrStatus = a});

-- | A set of character strings that represent the domain\'s identity. If the
-- identity is an email address, the tokens represent the domain of that
-- address.
--
-- Using these tokens, you will need to create DNS CNAME records that point
-- to DKIM public keys hosted by Amazon SES. Amazon Web Services will
-- eventually detect that you have updated your DNS records; this detection
-- process may take up to 72 hours. Upon successful detection, Amazon SES
-- will be able to DKIM-sign emails originating from that domain.
--
-- For more information about creating DNS records using DKIM tokens, go to
-- the
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim-dns-records.html Amazon SES Developer Guide>.
vddrDkimTokens :: Lens' VerifyDomainDkimResponse [Text]
vddrDkimTokens = lens _vddrDkimTokens (\ s a -> s{_vddrDkimTokens = a});
