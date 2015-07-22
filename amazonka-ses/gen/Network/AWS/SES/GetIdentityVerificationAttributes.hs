{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.GetIdentityVerificationAttributes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Given a list of identities (email addresses and\/or domains), returns
-- the verification status and (for domain identities) the verification
-- token for each identity.
--
-- This action is throttled at one request per second and can only get
-- verification attributes for up to 100 identities at a time.
--
-- <http://docs.aws.amazon.com/ses/latest/APIReference/API_GetIdentityVerificationAttributes.html>
module Network.AWS.SES.GetIdentityVerificationAttributes
    (
    -- * Request
      GetIdentityVerificationAttributes
    -- ** Request constructor
    , getIdentityVerificationAttributes
    -- ** Request lenses
    , givarqIdentities

    -- * Response
    , GetIdentityVerificationAttributesResponse
    -- ** Response constructor
    , getIdentityVerificationAttributesResponse
    -- ** Response lenses
    , givarsStatus
    , givarsVerificationAttributes
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types

-- | Represents a request instructing the service to provide the verification
-- attributes for a list of identities.
--
-- /See:/ 'getIdentityVerificationAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'givarqIdentities'
newtype GetIdentityVerificationAttributes = GetIdentityVerificationAttributes'
    { _givarqIdentities :: [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetIdentityVerificationAttributes' smart constructor.
getIdentityVerificationAttributes :: GetIdentityVerificationAttributes
getIdentityVerificationAttributes =
    GetIdentityVerificationAttributes'
    { _givarqIdentities = mempty
    }

-- | A list of identities.
givarqIdentities :: Lens' GetIdentityVerificationAttributes [Text]
givarqIdentities = lens _givarqIdentities (\ s a -> s{_givarqIdentities = a});

instance AWSRequest GetIdentityVerificationAttributes
         where
        type Sv GetIdentityVerificationAttributes = SES
        type Rs GetIdentityVerificationAttributes =
             GetIdentityVerificationAttributesResponse
        request = post
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
               "Identities" =:
                 toQueryList "member" _givarqIdentities]

-- | Represents the verification attributes for a list of identities.
--
-- /See:/ 'getIdentityVerificationAttributesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'givarsStatus'
--
-- * 'givarsVerificationAttributes'
data GetIdentityVerificationAttributesResponse = GetIdentityVerificationAttributesResponse'
    { _givarsStatus                 :: !Int
    , _givarsVerificationAttributes :: !(Map Text IdentityVerificationAttributes)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetIdentityVerificationAttributesResponse' smart constructor.
getIdentityVerificationAttributesResponse :: Int -> GetIdentityVerificationAttributesResponse
getIdentityVerificationAttributesResponse pStatus =
    GetIdentityVerificationAttributesResponse'
    { _givarsStatus = pStatus
    , _givarsVerificationAttributes = mempty
    }

-- | FIXME: Undocumented member.
givarsStatus :: Lens' GetIdentityVerificationAttributesResponse Int
givarsStatus = lens _givarsStatus (\ s a -> s{_givarsStatus = a});

-- | A map of Identities to IdentityVerificationAttributes objects.
givarsVerificationAttributes :: Lens' GetIdentityVerificationAttributesResponse (HashMap Text IdentityVerificationAttributes)
givarsVerificationAttributes = lens _givarsVerificationAttributes (\ s a -> s{_givarsVerificationAttributes = a}) . _Map;
