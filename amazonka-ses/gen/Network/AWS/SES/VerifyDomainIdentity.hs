{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.VerifyDomainIdentity
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Verifies a domain.
--
-- This action is throttled at one request per second.
module Network.AWS.SES.VerifyDomainIdentity
    (
    -- * Creating a Request
      verifyDomainIdentity
    , VerifyDomainIdentity
    -- * Request Lenses
    , vdiDomain

    -- * Destructuring the Response
    , verifyDomainIdentityResponse
    , VerifyDomainIdentityResponse
    -- * Response Lenses
    , vdirsResponseStatus
    , vdirsVerificationToken
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types
import           Network.AWS.SES.Types.Product

-- | /See:/ 'verifyDomainIdentity' smart constructor.
newtype VerifyDomainIdentity = VerifyDomainIdentity'
    { _vdiDomain :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'VerifyDomainIdentity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vdiDomain'
verifyDomainIdentity
    :: Text -- ^ 'vdiDomain'
    -> VerifyDomainIdentity
verifyDomainIdentity pDomain_ =
    VerifyDomainIdentity'
    { _vdiDomain = pDomain_
    }

-- | The domain to be verified.
vdiDomain :: Lens' VerifyDomainIdentity Text
vdiDomain = lens _vdiDomain (\ s a -> s{_vdiDomain = a});

instance AWSRequest VerifyDomainIdentity where
        type Rs VerifyDomainIdentity =
             VerifyDomainIdentityResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "VerifyDomainIdentityResult"
              (\ s h x ->
                 VerifyDomainIdentityResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "VerificationToken"))

instance Hashable VerifyDomainIdentity

instance NFData VerifyDomainIdentity

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

-- | /See:/ 'verifyDomainIdentityResponse' smart constructor.
data VerifyDomainIdentityResponse = VerifyDomainIdentityResponse'
    { _vdirsResponseStatus    :: !Int
    , _vdirsVerificationToken :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'VerifyDomainIdentityResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vdirsResponseStatus'
--
-- * 'vdirsVerificationToken'
verifyDomainIdentityResponse
    :: Int -- ^ 'vdirsResponseStatus'
    -> Text -- ^ 'vdirsVerificationToken'
    -> VerifyDomainIdentityResponse
verifyDomainIdentityResponse pResponseStatus_ pVerificationToken_ =
    VerifyDomainIdentityResponse'
    { _vdirsResponseStatus = pResponseStatus_
    , _vdirsVerificationToken = pVerificationToken_
    }

-- | The response status code.
vdirsResponseStatus :: Lens' VerifyDomainIdentityResponse Int
vdirsResponseStatus = lens _vdirsResponseStatus (\ s a -> s{_vdirsResponseStatus = a});

-- | A TXT record that must be placed in the DNS settings for the domain, in
-- order to complete domain verification.
vdirsVerificationToken :: Lens' VerifyDomainIdentityResponse Text
vdirsVerificationToken = lens _vdirsVerificationToken (\ s a -> s{_vdirsVerificationToken = a});
