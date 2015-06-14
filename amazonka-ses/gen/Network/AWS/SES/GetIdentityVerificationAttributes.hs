{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.SES.GetIdentityVerificationAttributes
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

-- | Given a list of identities (email addresses and\/or domains), returns
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
    , givaIdentities

    -- * Response
    , GetIdentityVerificationAttributesResponse
    -- ** Response constructor
    , getIdentityVerificationAttributesResponse
    -- ** Response lenses
    , givarVerificationAttributes
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.SES.Types

-- | /See:/ 'getIdentityVerificationAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'givaIdentities'
newtype GetIdentityVerificationAttributes = GetIdentityVerificationAttributes'{_givaIdentities :: [Text]} deriving (Eq, Read, Show)

-- | 'GetIdentityVerificationAttributes' smart constructor.
getIdentityVerificationAttributes :: [Text] -> GetIdentityVerificationAttributes
getIdentityVerificationAttributes pIdentities = GetIdentityVerificationAttributes'{_givaIdentities = pIdentities};

-- | A list of identities.
givaIdentities :: Lens' GetIdentityVerificationAttributes [Text]
givaIdentities = lens _givaIdentities (\ s a -> s{_givaIdentities = a});

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
               "Identities" =: "member" =: _givaIdentities]

-- | /See:/ 'getIdentityVerificationAttributesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'givarVerificationAttributes'
newtype GetIdentityVerificationAttributesResponse = GetIdentityVerificationAttributesResponse'{_givarVerificationAttributes :: HashMap Text IdentityVerificationAttributes} deriving (Eq, Read, Show)

-- | 'GetIdentityVerificationAttributesResponse' smart constructor.
getIdentityVerificationAttributesResponse :: HashMap Text IdentityVerificationAttributes -> GetIdentityVerificationAttributesResponse
getIdentityVerificationAttributesResponse pVerificationAttributes = GetIdentityVerificationAttributesResponse'{_givarVerificationAttributes = _Coerce # pVerificationAttributes};

-- | A map of Identities to IdentityVerificationAttributes objects.
givarVerificationAttributes :: Lens' GetIdentityVerificationAttributesResponse (HashMap Text IdentityVerificationAttributes)
givarVerificationAttributes = lens _givarVerificationAttributes (\ s a -> s{_givarVerificationAttributes = a}) . _Coerce;
