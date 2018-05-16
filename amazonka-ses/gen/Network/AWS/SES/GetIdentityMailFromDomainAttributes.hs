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
-- Module      : Network.AWS.SES.GetIdentityMailFromDomainAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the custom MAIL FROM attributes for a list of identities (email addresses : domains).
--
--
-- This operation is throttled at one request per second and can only get custom MAIL FROM attributes for up to 100 identities at a time.
--
module Network.AWS.SES.GetIdentityMailFromDomainAttributes
    (
    -- * Creating a Request
      getIdentityMailFromDomainAttributes
    , GetIdentityMailFromDomainAttributes
    -- * Request Lenses
    , gimfdaIdentities

    -- * Destructuring the Response
    , getIdentityMailFromDomainAttributesResponse
    , GetIdentityMailFromDomainAttributesResponse
    -- * Response Lenses
    , gimfdarsResponseStatus
    , gimfdarsMailFromDomainAttributes
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to return the Amazon SES custom MAIL FROM attributes for a list of identities. For information about using a custom MAIL FROM domain, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/mail-from.html Amazon SES Developer Guide> .
--
--
--
-- /See:/ 'getIdentityMailFromDomainAttributes' smart constructor.
newtype GetIdentityMailFromDomainAttributes = GetIdentityMailFromDomainAttributes'
  { _gimfdaIdentities :: [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetIdentityMailFromDomainAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gimfdaIdentities' - A list of one or more identities.
getIdentityMailFromDomainAttributes
    :: GetIdentityMailFromDomainAttributes
getIdentityMailFromDomainAttributes =
  GetIdentityMailFromDomainAttributes' {_gimfdaIdentities = mempty}


-- | A list of one or more identities.
gimfdaIdentities :: Lens' GetIdentityMailFromDomainAttributes [Text]
gimfdaIdentities = lens _gimfdaIdentities (\ s a -> s{_gimfdaIdentities = a}) . _Coerce

instance AWSRequest
           GetIdentityMailFromDomainAttributes
         where
        type Rs GetIdentityMailFromDomainAttributes =
             GetIdentityMailFromDomainAttributesResponse
        request = postQuery ses
        response
          = receiveXMLWrapper
              "GetIdentityMailFromDomainAttributesResult"
              (\ s h x ->
                 GetIdentityMailFromDomainAttributesResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .@? "MailFromDomainAttributes" .!@ mempty >>=
                        parseXMLMap "entry" "key" "value"))

instance Hashable GetIdentityMailFromDomainAttributes
         where

instance NFData GetIdentityMailFromDomainAttributes
         where

instance ToHeaders
           GetIdentityMailFromDomainAttributes
         where
        toHeaders = const mempty

instance ToPath GetIdentityMailFromDomainAttributes
         where
        toPath = const "/"

instance ToQuery GetIdentityMailFromDomainAttributes
         where
        toQuery GetIdentityMailFromDomainAttributes'{..}
          = mconcat
              ["Action" =:
                 ("GetIdentityMailFromDomainAttributes" ::
                    ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "Identities" =:
                 toQueryList "member" _gimfdaIdentities]

-- | Represents the custom MAIL FROM attributes for a list of identities.
--
--
--
-- /See:/ 'getIdentityMailFromDomainAttributesResponse' smart constructor.
data GetIdentityMailFromDomainAttributesResponse = GetIdentityMailFromDomainAttributesResponse'
  { _gimfdarsResponseStatus :: !Int
  , _gimfdarsMailFromDomainAttributes :: !(Map Text IdentityMailFromDomainAttributes)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetIdentityMailFromDomainAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gimfdarsResponseStatus' - -- | The response status code.
--
-- * 'gimfdarsMailFromDomainAttributes' - A map of identities to custom MAIL FROM attributes.
getIdentityMailFromDomainAttributesResponse
    :: Int -- ^ 'gimfdarsResponseStatus'
    -> GetIdentityMailFromDomainAttributesResponse
getIdentityMailFromDomainAttributesResponse pResponseStatus_ =
  GetIdentityMailFromDomainAttributesResponse'
    { _gimfdarsResponseStatus = pResponseStatus_
    , _gimfdarsMailFromDomainAttributes = mempty
    }


-- | -- | The response status code.
gimfdarsResponseStatus :: Lens' GetIdentityMailFromDomainAttributesResponse Int
gimfdarsResponseStatus = lens _gimfdarsResponseStatus (\ s a -> s{_gimfdarsResponseStatus = a})

-- | A map of identities to custom MAIL FROM attributes.
gimfdarsMailFromDomainAttributes :: Lens' GetIdentityMailFromDomainAttributesResponse (HashMap Text IdentityMailFromDomainAttributes)
gimfdarsMailFromDomainAttributes = lens _gimfdarsMailFromDomainAttributes (\ s a -> s{_gimfdarsMailFromDomainAttributes = a}) . _Map

instance NFData
           GetIdentityMailFromDomainAttributesResponse
         where
