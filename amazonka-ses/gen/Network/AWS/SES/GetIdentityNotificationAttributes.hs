{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.SES.GetIdentityNotificationAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Given a list of verified identities (email addresses and/or domains),
-- returns a structure describing identity notification attributes. This
-- action is throttled at one request per second. For more information about
-- using notifications with Amazon SES, see the Amazon SES Developer Guide.
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

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SES.Types

newtype GetIdentityNotificationAttributes = GetIdentityNotificationAttributes
    { _ginaIdentities :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid, Semigroup)

instance IsList GetIdentityNotificationAttributes where
    type Item GetIdentityNotificationAttributes = Text

    fromList = GetIdentityNotificationAttributes . fromList
    toList   = toList . _ginaIdentities

-- | 'GetIdentityNotificationAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ginaIdentities' @::@ ['Text']
--
getIdentityNotificationAttributes :: GetIdentityNotificationAttributes
getIdentityNotificationAttributes = GetIdentityNotificationAttributes
    { _ginaIdentities = mempty
    }

-- | A list of one or more identities.
ginaIdentities :: Lens' GetIdentityNotificationAttributes [Text]
ginaIdentities = lens _ginaIdentities (\s a -> s { _ginaIdentities = a })

instance ToQuery GetIdentityNotificationAttributes

instance ToPath GetIdentityNotificationAttributes where
    toPath = const "/"

newtype GetIdentityNotificationAttributesResponse = GetIdentityNotificationAttributesResponse
    { _ginarNotificationAttributes :: Map Text IdentityNotificationAttributes
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance IsList GetIdentityNotificationAttributesResponse where
    type Item GetIdentityNotificationAttributesResponse = (Text, IdentityNotificationAttributes)

    fromList = GetIdentityNotificationAttributesResponse . fromList
    toList   = toList . _ginarNotificationAttributes

-- | 'GetIdentityNotificationAttributesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ginarNotificationAttributes' @::@ 'HashMap' 'Text' 'IdentityNotificationAttributes'
--
getIdentityNotificationAttributesResponse :: GetIdentityNotificationAttributesResponse
getIdentityNotificationAttributesResponse = GetIdentityNotificationAttributesResponse
    { _ginarNotificationAttributes = mempty
    }

-- | A map of Identity to IdentityNotificationAttributes.
ginarNotificationAttributes :: Lens' GetIdentityNotificationAttributesResponse (HashMap Text IdentityNotificationAttributes)
ginarNotificationAttributes =
    lens _ginarNotificationAttributes
        (\s a -> s { _ginarNotificationAttributes = a })
            . _Map

instance FromXML GetIdentityNotificationAttributesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GetIdentityNotificationAttributesResponse"

instance AWSRequest GetIdentityNotificationAttributes where
    type Sv GetIdentityNotificationAttributes = SES
    type Rs GetIdentityNotificationAttributes = GetIdentityNotificationAttributesResponse

    request  = post "GetIdentityNotificationAttributes"
    response = xmlResponse $ \h x -> GetIdentityNotificationAttributesResponse
        <$> x %| "NotificationAttributes"
