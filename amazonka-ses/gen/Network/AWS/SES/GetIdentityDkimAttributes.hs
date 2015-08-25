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
-- Module      : Network.AWS.SES.GetIdentityDkimAttributes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
-- /See:/ <http://docs.aws.amazon.com/ses/latest/APIReference/API_GetIdentityDkimAttributes.html AWS API Reference> for GetIdentityDkimAttributes.
module Network.AWS.SES.GetIdentityDkimAttributes
    (
    -- * Creating a Request
      getIdentityDkimAttributes
    , GetIdentityDkimAttributes
    -- * Request Lenses
    , gidaIdentities

    -- * Destructuring the Response
    , getIdentityDkimAttributesResponse
    , GetIdentityDkimAttributesResponse
    -- * Response Lenses
    , gidarsStatus
    , gidarsDkimAttributes
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types
import           Network.AWS.SES.Types.Product

-- | Given a list of verified identities, describes their DKIM attributes.
-- The DKIM attributes of an email address identity includes whether DKIM
-- signing is individually enabled or disabled for that address. The DKIM
-- attributes of a domain name identity includes whether DKIM signing is
-- enabled, as well as the DNS records (tokens) that must remain published
-- in the domain name\'s DNS.
--
-- /See:/ 'getIdentityDkimAttributes' smart constructor.
newtype GetIdentityDkimAttributes = GetIdentityDkimAttributes'
    { _gidaIdentities :: [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetIdentityDkimAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gidaIdentities'
getIdentityDkimAttributes
    :: GetIdentityDkimAttributes
getIdentityDkimAttributes =
    GetIdentityDkimAttributes'
    { _gidaIdentities = mempty
    }

-- | A list of one or more verified identities - email addresses, domains, or
-- both.
gidaIdentities :: Lens' GetIdentityDkimAttributes [Text]
gidaIdentities = lens _gidaIdentities (\ s a -> s{_gidaIdentities = a}) . _Coerce;

instance AWSRequest GetIdentityDkimAttributes where
        type Rs GetIdentityDkimAttributes =
             GetIdentityDkimAttributesResponse
        request = postQuery sES
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
               "Identities" =: toQueryList "member" _gidaIdentities]

-- | Represents a list of all the DKIM attributes for the specified identity.
--
-- /See:/ 'getIdentityDkimAttributesResponse' smart constructor.
data GetIdentityDkimAttributesResponse = GetIdentityDkimAttributesResponse'
    { _gidarsStatus         :: !Int
    , _gidarsDkimAttributes :: !(Map Text IdentityDkimAttributes)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetIdentityDkimAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gidarsStatus'
--
-- * 'gidarsDkimAttributes'
getIdentityDkimAttributesResponse
    :: Int -- ^ 'gidarsStatus'
    -> GetIdentityDkimAttributesResponse
getIdentityDkimAttributesResponse pStatus_ =
    GetIdentityDkimAttributesResponse'
    { _gidarsStatus = pStatus_
    , _gidarsDkimAttributes = mempty
    }

-- | The response status code.
gidarsStatus :: Lens' GetIdentityDkimAttributesResponse Int
gidarsStatus = lens _gidarsStatus (\ s a -> s{_gidarsStatus = a});

-- | The DKIM attributes for an email address or a domain.
gidarsDkimAttributes :: Lens' GetIdentityDkimAttributesResponse (HashMap Text IdentityDkimAttributes)
gidarsDkimAttributes = lens _gidarsDkimAttributes (\ s a -> s{_gidarsDkimAttributes = a}) . _Map;
