{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SES.VerifyDomainIdentity
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

-- | Verifies a domain.
--
-- This action is throttled at one request per second.
--
-- <http://docs.aws.amazon.com/ses/latest/APIReference/API_VerifyDomainIdentity.html>
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
    , vdirStatus
    , vdirVerificationToken
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types

-- | Represents a request instructing the service to begin domain
-- verification.
--
-- /See:/ 'verifyDomainIdentity' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vdiDomain'
newtype VerifyDomainIdentity = VerifyDomainIdentity'
    { _vdiDomain :: Text
    } deriving (Eq,Read,Show)

-- | 'VerifyDomainIdentity' smart constructor.
verifyDomainIdentity :: Text -> VerifyDomainIdentity
verifyDomainIdentity pDomain =
    VerifyDomainIdentity'
    { _vdiDomain = pDomain
    }

-- | The domain to be verified.
vdiDomain :: Lens' VerifyDomainIdentity Text
vdiDomain = lens _vdiDomain (\ s a -> s{_vdiDomain = a});

instance AWSRequest VerifyDomainIdentity where
        type Sv VerifyDomainIdentity = SES
        type Rs VerifyDomainIdentity =
             VerifyDomainIdentityResponse
        request = post
        response
          = receiveXMLWrapper "VerifyDomainIdentityResult"
              (\ s h x ->
                 VerifyDomainIdentityResponse' <$>
                   (pure s) <*> (x .@ "VerificationToken"))

instance ToHeaders VerifyDomainIdentity where
        toHeaders = const mempty

instance ToPath VerifyDomainIdentity where
        toPath = const "/"

instance ToQuery VerifyDomainIdentity where
        toQuery VerifyDomainIdentity'{..}
          = mconcat
              ["Action" =: ("VerifyDomainIdentity" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "Domain" =: _vdiDomain]

-- | Represents a token used for domain ownership verification.
--
-- /See:/ 'verifyDomainIdentityResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vdirStatus'
--
-- * 'vdirVerificationToken'
data VerifyDomainIdentityResponse = VerifyDomainIdentityResponse'
    { _vdirStatus            :: !Status
    , _vdirVerificationToken :: !Text
    } deriving (Eq,Show)

-- | 'VerifyDomainIdentityResponse' smart constructor.
verifyDomainIdentityResponse :: Status -> Text -> VerifyDomainIdentityResponse
verifyDomainIdentityResponse pStatus pVerificationToken =
    VerifyDomainIdentityResponse'
    { _vdirStatus = pStatus
    , _vdirVerificationToken = pVerificationToken
    }

-- | FIXME: Undocumented member.
vdirStatus :: Lens' VerifyDomainIdentityResponse Status
vdirStatus = lens _vdirStatus (\ s a -> s{_vdirStatus = a});

-- | A TXT record that must be placed in the DNS settings for the domain, in
-- order to complete domain verification.
vdirVerificationToken :: Lens' VerifyDomainIdentityResponse Text
vdirVerificationToken = lens _vdirVerificationToken (\ s a -> s{_vdirVerificationToken = a});
