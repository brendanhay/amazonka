{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.VerifyDomainIdentity
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Verifies a domain.
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
    , vdirsStatus
    , vdirsVerificationToken
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'VerifyDomainIdentity' smart constructor.
verifyDomainIdentity :: Text -> VerifyDomainIdentity
verifyDomainIdentity pDomain_ =
    VerifyDomainIdentity'
    { _vdiDomain = pDomain_
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
                   (pure (fromEnum s)) <*> (x .@ "VerificationToken"))

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
-- * 'vdirsStatus'
--
-- * 'vdirsVerificationToken'
data VerifyDomainIdentityResponse = VerifyDomainIdentityResponse'
    { _vdirsStatus            :: !Int
    , _vdirsVerificationToken :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'VerifyDomainIdentityResponse' smart constructor.
verifyDomainIdentityResponse :: Int -> Text -> VerifyDomainIdentityResponse
verifyDomainIdentityResponse pStatus_ pVerificationToken_ =
    VerifyDomainIdentityResponse'
    { _vdirsStatus = pStatus_
    , _vdirsVerificationToken = pVerificationToken_
    }

-- | FIXME: Undocumented member.
vdirsStatus :: Lens' VerifyDomainIdentityResponse Int
vdirsStatus = lens _vdirsStatus (\ s a -> s{_vdirsStatus = a});

-- | A TXT record that must be placed in the DNS settings for the domain, in
-- order to complete domain verification.
vdirsVerificationToken :: Lens' VerifyDomainIdentityResponse Text
vdirsVerificationToken = lens _vdirsVerificationToken (\ s a -> s{_vdirsVerificationToken = a});
