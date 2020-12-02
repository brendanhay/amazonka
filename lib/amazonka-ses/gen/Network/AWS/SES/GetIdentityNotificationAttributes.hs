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
-- Module      : Network.AWS.SES.GetIdentityNotificationAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Given a list of verified identities (email addresses and/or domains), returns a structure describing identity notification attributes.
--
--
-- This operation is throttled at one request per second and can only get notification attributes for up to 100 identities at a time.
--
-- For more information about using notifications with Amazon SES, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html Amazon SES Developer Guide> .
--
module Network.AWS.SES.GetIdentityNotificationAttributes
    (
    -- * Creating a Request
      getIdentityNotificationAttributes
    , GetIdentityNotificationAttributes
    -- * Request Lenses
    , ginaIdentities

    -- * Destructuring the Response
    , getIdentityNotificationAttributesResponse
    , GetIdentityNotificationAttributesResponse
    -- * Response Lenses
    , ginarsResponseStatus
    , ginarsNotificationAttributes
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to return the notification attributes for a list of identities you verified with Amazon SES. For information about Amazon SES notifications, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html Amazon SES Developer Guide> .
--
--
--
-- /See:/ 'getIdentityNotificationAttributes' smart constructor.
newtype GetIdentityNotificationAttributes = GetIdentityNotificationAttributes'
  { _ginaIdentities :: [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetIdentityNotificationAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ginaIdentities' - A list of one or more identities. You can specify an identity by using its name or by using its Amazon Resource Name (ARN). Examples: @user@example.com@ , @example.com@ , @arn:aws:ses:us-east-1:123456789012:identity/example.com@ .
getIdentityNotificationAttributes
    :: GetIdentityNotificationAttributes
getIdentityNotificationAttributes =
  GetIdentityNotificationAttributes' {_ginaIdentities = mempty}


-- | A list of one or more identities. You can specify an identity by using its name or by using its Amazon Resource Name (ARN). Examples: @user@example.com@ , @example.com@ , @arn:aws:ses:us-east-1:123456789012:identity/example.com@ .
ginaIdentities :: Lens' GetIdentityNotificationAttributes [Text]
ginaIdentities = lens _ginaIdentities (\ s a -> s{_ginaIdentities = a}) . _Coerce

instance AWSRequest GetIdentityNotificationAttributes
         where
        type Rs GetIdentityNotificationAttributes =
             GetIdentityNotificationAttributesResponse
        request = postQuery ses
        response
          = receiveXMLWrapper
              "GetIdentityNotificationAttributesResult"
              (\ s h x ->
                 GetIdentityNotificationAttributesResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .@? "NotificationAttributes" .!@ mempty >>=
                        parseXMLMap "entry" "key" "value"))

instance Hashable GetIdentityNotificationAttributes
         where

instance NFData GetIdentityNotificationAttributes
         where

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

-- | Represents the notification attributes for a list of identities.
--
--
--
-- /See:/ 'getIdentityNotificationAttributesResponse' smart constructor.
data GetIdentityNotificationAttributesResponse = GetIdentityNotificationAttributesResponse'
  { _ginarsResponseStatus         :: !Int
  , _ginarsNotificationAttributes :: !(Map Text IdentityNotificationAttributes)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetIdentityNotificationAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ginarsResponseStatus' - -- | The response status code.
--
-- * 'ginarsNotificationAttributes' - A map of Identity to IdentityNotificationAttributes.
getIdentityNotificationAttributesResponse
    :: Int -- ^ 'ginarsResponseStatus'
    -> GetIdentityNotificationAttributesResponse
getIdentityNotificationAttributesResponse pResponseStatus_ =
  GetIdentityNotificationAttributesResponse'
    { _ginarsResponseStatus = pResponseStatus_
    , _ginarsNotificationAttributes = mempty
    }


-- | -- | The response status code.
ginarsResponseStatus :: Lens' GetIdentityNotificationAttributesResponse Int
ginarsResponseStatus = lens _ginarsResponseStatus (\ s a -> s{_ginarsResponseStatus = a})

-- | A map of Identity to IdentityNotificationAttributes.
ginarsNotificationAttributes :: Lens' GetIdentityNotificationAttributesResponse (HashMap Text IdentityNotificationAttributes)
ginarsNotificationAttributes = lens _ginarsNotificationAttributes (\ s a -> s{_ginarsNotificationAttributes = a}) . _Map

instance NFData
           GetIdentityNotificationAttributesResponse
         where
