{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

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
    , ginarStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types

-- | /See:/ 'getIdentityNotificationAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ginaIdentities'
newtype GetIdentityNotificationAttributes = GetIdentityNotificationAttributes'
    { _ginaIdentities :: [Text]
    } deriving (Eq,Read,Show)

-- | 'GetIdentityNotificationAttributes' smart constructor.
getIdentityNotificationAttributes :: GetIdentityNotificationAttributes
getIdentityNotificationAttributes =
    GetIdentityNotificationAttributes'
    { _ginaIdentities = mempty
    }

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
                      parseXMLMap "entry" "key" "value")
                     <*> (pure s))

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
               "Identities" =: toQueryList "member" _ginaIdentities]

-- | Describes whether an identity has Amazon Simple Notification Service
-- (Amazon SNS) topics set for bounce, complaint, and\/or delivery
-- notifications, and specifies whether feedback forwarding is enabled for
-- bounce and complaint notifications.
--
-- /See:/ 'getIdentityNotificationAttributesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ginarNotificationAttributes'
--
-- * 'ginarStatus'
data GetIdentityNotificationAttributesResponse = GetIdentityNotificationAttributesResponse'
    { _ginarNotificationAttributes :: !(Map Text IdentityNotificationAttributes)
    , _ginarStatus                 :: !Status
    } deriving (Eq,Read,Show)

-- | 'GetIdentityNotificationAttributesResponse' smart constructor.
getIdentityNotificationAttributesResponse :: Status -> GetIdentityNotificationAttributesResponse
getIdentityNotificationAttributesResponse pStatus =
    GetIdentityNotificationAttributesResponse'
    { _ginarNotificationAttributes = mempty
    , _ginarStatus = pStatus
    }

-- | A map of Identity to IdentityNotificationAttributes.
ginarNotificationAttributes :: Lens' GetIdentityNotificationAttributesResponse (HashMap Text IdentityNotificationAttributes)
ginarNotificationAttributes = lens _ginarNotificationAttributes (\ s a -> s{_ginarNotificationAttributes = a}) . _Map;

-- | FIXME: Undocumented member.
ginarStatus :: Lens' GetIdentityNotificationAttributesResponse Status
ginarStatus = lens _ginarStatus (\ s a -> s{_ginarStatus = a});
