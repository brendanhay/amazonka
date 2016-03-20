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
-- Module      : Network.AWS.SES.GetIdentityVerificationAttributes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Given a list of identities (email addresses and\/or domains), returns
-- the verification status and (for domain identities) the verification
-- token for each identity.
--
-- This action is throttled at one request per second and can only get
-- verification attributes for up to 100 identities at a time.
module Network.AWS.SES.GetIdentityVerificationAttributes
    (
    -- * Creating a Request
      getIdentityVerificationAttributes
    , GetIdentityVerificationAttributes
    -- * Request Lenses
    , givaIdentities

    -- * Destructuring the Response
    , getIdentityVerificationAttributesResponse
    , GetIdentityVerificationAttributesResponse
    -- * Response Lenses
    , givarsResponseStatus
    , givarsVerificationAttributes
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types
import           Network.AWS.SES.Types.Product

-- | /See:/ 'getIdentityVerificationAttributes' smart constructor.
newtype GetIdentityVerificationAttributes = GetIdentityVerificationAttributes'
    { _givaIdentities :: [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetIdentityVerificationAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'givaIdentities'
getIdentityVerificationAttributes
    :: GetIdentityVerificationAttributes
getIdentityVerificationAttributes =
    GetIdentityVerificationAttributes'
    { _givaIdentities = mempty
    }

-- | A list of identities.
givaIdentities :: Lens' GetIdentityVerificationAttributes [Text]
givaIdentities = lens _givaIdentities (\ s a -> s{_givaIdentities = a}) . _Coerce;

instance AWSRequest GetIdentityVerificationAttributes
         where
        type Rs GetIdentityVerificationAttributes =
             GetIdentityVerificationAttributesResponse
        request = postQuery ses
        response
          = receiveXMLWrapper
              "GetIdentityVerificationAttributesResult"
              (\ s h x ->
                 GetIdentityVerificationAttributesResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .@? "VerificationAttributes" .!@ mempty >>=
                        parseXMLMap "entry" "key" "value"))

instance Hashable GetIdentityVerificationAttributes

instance ToHeaders GetIdentityVerificationAttributes
         where
        toHeaders = const mempty

instance ToPath GetIdentityVerificationAttributes
         where
        toPath = const "/"

instance ToQuery GetIdentityVerificationAttributes
         where
        toQuery GetIdentityVerificationAttributes'{..}
          = mconcat
              ["Action" =:
                 ("GetIdentityVerificationAttributes" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "Identities" =: toQueryList "member" _givaIdentities]

-- | /See:/ 'getIdentityVerificationAttributesResponse' smart constructor.
data GetIdentityVerificationAttributesResponse = GetIdentityVerificationAttributesResponse'
    { _givarsResponseStatus         :: !Int
    , _givarsVerificationAttributes :: !(Map Text IdentityVerificationAttributes)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetIdentityVerificationAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'givarsResponseStatus'
--
-- * 'givarsVerificationAttributes'
getIdentityVerificationAttributesResponse
    :: Int -- ^ 'givarsResponseStatus'
    -> GetIdentityVerificationAttributesResponse
getIdentityVerificationAttributesResponse pResponseStatus_ =
    GetIdentityVerificationAttributesResponse'
    { _givarsResponseStatus = pResponseStatus_
    , _givarsVerificationAttributes = mempty
    }

-- | The response status code.
givarsResponseStatus :: Lens' GetIdentityVerificationAttributesResponse Int
givarsResponseStatus = lens _givarsResponseStatus (\ s a -> s{_givarsResponseStatus = a});

-- | A map of Identities to IdentityVerificationAttributes objects.
givarsVerificationAttributes :: Lens' GetIdentityVerificationAttributesResponse (HashMap Text IdentityVerificationAttributes)
givarsVerificationAttributes = lens _givarsVerificationAttributes (\ s a -> s{_givarsVerificationAttributes = a}) . _Map;
