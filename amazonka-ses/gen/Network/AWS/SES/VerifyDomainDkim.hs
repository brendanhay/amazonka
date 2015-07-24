{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.VerifyDomainDkim
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a set of DKIM tokens for a domain. DKIM /tokens/ are character
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
    , vddrsStatus
    , vddrsDkimTokens
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'VerifyDomainDkim' smart constructor.
verifyDomainDkim :: Text -> VerifyDomainDkim
verifyDomainDkim pDomain_ =
    VerifyDomainDkim'
    { _vddDomain = pDomain_
    }

-- | The name of the domain to be verified for Easy DKIM signing.
vddDomain :: Lens' VerifyDomainDkim Text
vddDomain = lens _vddDomain (\ s a -> s{_vddDomain = a});

instance AWSRequest VerifyDomainDkim where
        type Sv VerifyDomainDkim = SES
        type Rs VerifyDomainDkim = VerifyDomainDkimResponse
        request = post "VerifyDomainDkim"
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
-- * 'vddrsStatus'
--
-- * 'vddrsDkimTokens'
data VerifyDomainDkimResponse = VerifyDomainDkimResponse'
    { _vddrsStatus     :: !Int
    , _vddrsDkimTokens :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'VerifyDomainDkimResponse' smart constructor.
verifyDomainDkimResponse :: Int -> VerifyDomainDkimResponse
verifyDomainDkimResponse pStatus_ =
    VerifyDomainDkimResponse'
    { _vddrsStatus = pStatus_
    , _vddrsDkimTokens = mempty
    }

-- | FIXME: Undocumented member.
vddrsStatus :: Lens' VerifyDomainDkimResponse Int
vddrsStatus = lens _vddrsStatus (\ s a -> s{_vddrsStatus = a});

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
vddrsDkimTokens :: Lens' VerifyDomainDkimResponse [Text]
vddrsDkimTokens = lens _vddrsDkimTokens (\ s a -> s{_vddrsDkimTokens = a});
