{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.GetIdentityDkimAttributes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns the current status of Easy DKIM signing for an entity. For
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
    , gidarqIdentities

    -- * Response
    , GetIdentityDkimAttributesResponse
    -- ** Response constructor
    , getIdentityDkimAttributesResponse
    -- ** Response lenses
    , gidarsStatus
    , gidarsDkimAttributes
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types

-- | Given a list of verified identities, describes their DKIM attributes.
-- The DKIM attributes of an email address identity includes whether DKIM
-- signing is individually enabled or disabled for that address. The DKIM
-- attributes of a domain name identity includes whether DKIM signing is
-- enabled, as well as the DNS records (tokens) that must remain published
-- in the domain name\'s DNS.
--
-- /See:/ 'getIdentityDkimAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gidarqIdentities'
newtype GetIdentityDkimAttributes = GetIdentityDkimAttributes'
    { _gidarqIdentities :: [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetIdentityDkimAttributes' smart constructor.
getIdentityDkimAttributes :: GetIdentityDkimAttributes
getIdentityDkimAttributes =
    GetIdentityDkimAttributes'
    { _gidarqIdentities = mempty
    }

-- | A list of one or more verified identities - email addresses, domains, or
-- both.
gidarqIdentities :: Lens' GetIdentityDkimAttributes [Text]
gidarqIdentities = lens _gidarqIdentities (\ s a -> s{_gidarqIdentities = a});

instance AWSRequest GetIdentityDkimAttributes where
        type Sv GetIdentityDkimAttributes = SES
        type Rs GetIdentityDkimAttributes =
             GetIdentityDkimAttributesResponse
        request = post
        response
          = receiveXMLWrapper "GetIdentityDkimAttributesResult"
              (\ s h x ->
                 GetIdentityDkimAttributesResponse' <$>
                   (pure (fromEnum s)) <*>
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
               "Identities" =:
                 toQueryList "member" _gidarqIdentities]

-- | Represents a list of all the DKIM attributes for the specified identity.
--
-- /See:/ 'getIdentityDkimAttributesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gidarsStatus'
--
-- * 'gidarsDkimAttributes'
data GetIdentityDkimAttributesResponse = GetIdentityDkimAttributesResponse'
    { _gidarsStatus         :: !Int
    , _gidarsDkimAttributes :: !(Map Text IdentityDkimAttributes)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetIdentityDkimAttributesResponse' smart constructor.
getIdentityDkimAttributesResponse :: Int -> GetIdentityDkimAttributesResponse
getIdentityDkimAttributesResponse pStatus =
    GetIdentityDkimAttributesResponse'
    { _gidarsStatus = pStatus
    , _gidarsDkimAttributes = mempty
    }

-- | FIXME: Undocumented member.
gidarsStatus :: Lens' GetIdentityDkimAttributesResponse Int
gidarsStatus = lens _gidarsStatus (\ s a -> s{_gidarsStatus = a});

-- | The DKIM attributes for an email address or a domain.
gidarsDkimAttributes :: Lens' GetIdentityDkimAttributesResponse (HashMap Text IdentityDkimAttributes)
gidarsDkimAttributes = lens _gidarsDkimAttributes (\ s a -> s{_gidarsDkimAttributes = a}) . _Map;
