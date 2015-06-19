{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.SES.GetIdentityDkimAttributes
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

-- | Returns the current status of Easy DKIM signing for an entity. For
-- domain name identities, this action also returns the DKIM tokens that
-- are required for Easy DKIM signing, and whether Amazon SES has
-- successfully verified that these tokens have been published.
--
-- This action takes a list of identities as input and returns the
-- following information for each:
--
-- -   Whether Easy DKIM signing is enabled or disabled.
-- -   A set of DKIM tokens that represent the identity. If the identity is
--     an email address, the tokens represent the domain of that address.
-- -   Whether Amazon SES has successfully verified the DKIM tokens
--     published in the domain\'s DNS. This information is only returned
--     for domain name identities, not for email addresses.
--
-- This action is throttled at one request per second and can only get DKIM
-- attributes for up to 100 identities at a time.
--
-- For more information about creating DNS records using DKIM tokens, go to
-- the
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim-dns-records.html Amazon SES Developer Guide>.
--
-- <http://docs.aws.amazon.com/ses/latest/APIReference/API_GetIdentityDkimAttributes.html>
module Network.AWS.SES.GetIdentityDkimAttributes
    (
    -- * Request
      GetIdentityDkimAttributes
    -- ** Request constructor
    , getIdentityDkimAttributes
    -- ** Request lenses
    , gidaIdentities

    -- * Response
    , GetIdentityDkimAttributesResponse
    -- ** Response constructor
    , getIdentityDkimAttributesResponse
    -- ** Response lenses
    , gidarDkimAttributes
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types

-- | /See:/ 'getIdentityDkimAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gidaIdentities'
newtype GetIdentityDkimAttributes = GetIdentityDkimAttributes'{_gidaIdentities :: [Text]} deriving (Eq, Read, Show)

-- | 'GetIdentityDkimAttributes' smart constructor.
getIdentityDkimAttributes :: GetIdentityDkimAttributes
getIdentityDkimAttributes = GetIdentityDkimAttributes'{_gidaIdentities = mempty};

-- | A list of one or more verified identities - email addresses, domains, or
-- both.
gidaIdentities :: Lens' GetIdentityDkimAttributes [Text]
gidaIdentities = lens _gidaIdentities (\ s a -> s{_gidaIdentities = a});

instance AWSRequest GetIdentityDkimAttributes where
        type Sv GetIdentityDkimAttributes = SES
        type Rs GetIdentityDkimAttributes =
             GetIdentityDkimAttributesResponse
        request = post
        response
          = receiveXMLWrapper "GetIdentityDkimAttributesResult"
              (\ s h x ->
                 GetIdentityDkimAttributesResponse' <$>
                   (x .@? "DkimAttributes" .!@ mempty >>=
                      parseXMLMap "entry" "key" "value"))

instance ToHeaders GetIdentityDkimAttributes where
        toHeaders = const mempty

instance ToPath GetIdentityDkimAttributes where
        toPath = const "/"

instance ToQuery GetIdentityDkimAttributes where
        toQuery GetIdentityDkimAttributes'{..}
          = mconcat
              ["Action" =:
                 ("GetIdentityDkimAttributes" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "Identities" =: toQueryList "member" _gidaIdentities]

-- | /See:/ 'getIdentityDkimAttributesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gidarDkimAttributes'
newtype GetIdentityDkimAttributesResponse = GetIdentityDkimAttributesResponse'{_gidarDkimAttributes :: Map Text IdentityDkimAttributes} deriving (Eq, Read, Show)

-- | 'GetIdentityDkimAttributesResponse' smart constructor.
getIdentityDkimAttributesResponse :: GetIdentityDkimAttributesResponse
getIdentityDkimAttributesResponse = GetIdentityDkimAttributesResponse'{_gidarDkimAttributes = mempty};

-- | The DKIM attributes for an email address or a domain.
gidarDkimAttributes :: Lens' GetIdentityDkimAttributesResponse (HashMap Text IdentityDkimAttributes)
gidarDkimAttributes = lens _gidarDkimAttributes (\ s a -> s{_gidarDkimAttributes = a}) . _Map;
