{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.SES.GetIdentityNotificationAttributes
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

-- | Given a list of verified identities (email addresses and\/or domains),
-- returns a structure describing identity notification attributes.
--
-- This action is throttled at one request per second and can only get
-- notification attributes for up to 100 identities at a time.
--
-- For more information about using notifications with Amazon SES, see the
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html Amazon SES Developer Guide>.
--
-- <http://docs.aws.amazon.com/ses/latest/APIReference/API_GetIdentityNotificationAttributes.html>
module Network.AWS.SES.GetIdentityNotificationAttributes
    (
    -- * Request
      GetIdentityNotificationAttributes
    -- ** Request constructor
    , getIdentityNotificationAttributes
    -- ** Request lenses
    , ginaIdentities

    -- * Response
    , GetIdentityNotificationAttributesResponse
    -- ** Response constructor
    , getIdentityNotificationAttributesResponse
    -- ** Response lenses
    , ginarNotificationAttributes
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.SES.Types

-- | /See:/ 'getIdentityNotificationAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ginaIdentities'
newtype GetIdentityNotificationAttributes = GetIdentityNotificationAttributes'{_ginaIdentities :: [Text]} deriving (Eq, Read, Show)

-- | 'GetIdentityNotificationAttributes' smart constructor.
getIdentityNotificationAttributes :: [Text] -> GetIdentityNotificationAttributes
getIdentityNotificationAttributes pIdentities = GetIdentityNotificationAttributes'{_ginaIdentities = pIdentities};

-- | A list of one or more identities.
ginaIdentities :: Lens' GetIdentityNotificationAttributes [Text]
ginaIdentities = lens _ginaIdentities (\ s a -> s{_ginaIdentities = a});

instance AWSRequest GetIdentityNotificationAttributes
         where
        type Sv GetIdentityNotificationAttributes = SES
        type Rs GetIdentityNotificationAttributes =
             GetIdentityNotificationAttributesResponse
        request = post
        response
          = receiveXMLWrapper
              "GetIdentityNotificationAttributesResult"
              (\ s h x ->
                 GetIdentityNotificationAttributesResponse' <$>
                   (x .@? "NotificationAttributes" .!@ mempty >>=
                      parseXMLMap "entry" "key" "value"))

instance ToHeaders GetIdentityNotificationAttributes
         where
        toHeaders = const mempty

instance ToPath GetIdentityNotificationAttributes
         where
        toPath = const "/"

instance ToQuery GetIdentityNotificationAttributes
         where
        toQuery GetIdentityNotificationAttributes'{..}
          = mconcat
              ["Action" =:
                 ("GetIdentityNotificationAttributes" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "Identities" =: "member" =: _ginaIdentities]

-- | /See:/ 'getIdentityNotificationAttributesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ginarNotificationAttributes'
newtype GetIdentityNotificationAttributesResponse = GetIdentityNotificationAttributesResponse'{_ginarNotificationAttributes :: HashMap Text IdentityNotificationAttributes} deriving (Eq, Read, Show)

-- | 'GetIdentityNotificationAttributesResponse' smart constructor.
getIdentityNotificationAttributesResponse :: HashMap Text IdentityNotificationAttributes -> GetIdentityNotificationAttributesResponse
getIdentityNotificationAttributesResponse pNotificationAttributes = GetIdentityNotificationAttributesResponse'{_ginarNotificationAttributes = _Coerce # pNotificationAttributes};

-- | A map of Identity to IdentityNotificationAttributes.
ginarNotificationAttributes :: Lens' GetIdentityNotificationAttributesResponse (HashMap Text IdentityNotificationAttributes)
ginarNotificationAttributes = lens _ginarNotificationAttributes (\ s a -> s{_ginarNotificationAttributes = a}) . _Coerce;
