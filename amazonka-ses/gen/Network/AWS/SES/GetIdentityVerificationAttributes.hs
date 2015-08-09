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
--
-- /See:/ <http://docs.aws.amazon.com/ses/latest/APIReference/API_GetIdentityVerificationAttributes.html AWS API Reference> for GetIdentityVerificationAttributes.
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
    , givarsStatus
    , givarsVerificationAttributes
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types
import           Network.AWS.SES.Types.Product

-- | Represents a request instructing the service to provide the verification
-- attributes for a list of identities.
--
-- /See:/ 'getIdentityVerificationAttributes' smart constructor.
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
        type Sv GetIdentityVerificationAttributes = SES
        type Rs GetIdentityVerificationAttributes =
             GetIdentityVerificationAttributesResponse
        request = postQuery
        response
          = receiveXMLWrapper
              "GetIdentityVerificationAttributesResult"
              (\ s h x ->
                 GetIdentityVerificationAttributesResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .@? "VerificationAttributes" .!@ mempty >>=
                        parseXMLMap "entry" "key" "value"))

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

-- | Represents the verification attributes for a list of identities.
--
-- /See:/ 'getIdentityVerificationAttributesResponse' smart constructor.
data GetIdentityVerificationAttributesResponse = GetIdentityVerificationAttributesResponse'
    { _givarsStatus                 :: !Int
    , _givarsVerificationAttributes :: !(Map Text IdentityVerificationAttributes)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetIdentityVerificationAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'givarsStatus'
--
-- * 'givarsVerificationAttributes'
getIdentityVerificationAttributesResponse
    :: Int -- ^ 'givarsStatus'
    -> GetIdentityVerificationAttributesResponse
getIdentityVerificationAttributesResponse pStatus_ =
    GetIdentityVerificationAttributesResponse'
    { _givarsStatus = pStatus_
    , _givarsVerificationAttributes = mempty
    }

-- | The response status code.
givarsStatus :: Lens' GetIdentityVerificationAttributesResponse Int
givarsStatus = lens _givarsStatus (\ s a -> s{_givarsStatus = a});

-- | A map of Identities to IdentityVerificationAttributes objects.
givarsVerificationAttributes :: Lens' GetIdentityVerificationAttributesResponse (HashMap Text IdentityVerificationAttributes)
givarsVerificationAttributes = lens _givarsVerificationAttributes (\ s a -> s{_givarsVerificationAttributes = a}) . _Map;
