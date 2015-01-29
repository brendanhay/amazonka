{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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

-- | Given a list of verified identities (email addresses and/or domains), returns
-- a structure describing identity notification attributes.
--
-- This action is throttled at one request per second.
--
-- For more information about using notifications with Amazon SES, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html Amazon SES Developer Guide>.
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

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SES.Types
import qualified GHC.Exts

newtype GetIdentityNotificationAttributes = GetIdentityNotificationAttributes
    { _ginaIdentities :: List "member" Text
    } deriving (Eq, Ord, Read, Show, Monoid, Semigroup)

instance GHC.Exts.IsList GetIdentityNotificationAttributes where
    type Item GetIdentityNotificationAttributes = Text

    fromList = GetIdentityNotificationAttributes . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _ginaIdentities

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
ginaIdentities = lens _ginaIdentities (\s a -> s { _ginaIdentities = a }) . _List

newtype GetIdentityNotificationAttributesResponse = GetIdentityNotificationAttributesResponse
    { _ginarNotificationAttributes :: EMap "entry" "key" "value" Text IdentityNotificationAttributes
    } deriving (Eq, Read, Show, Monoid, Semigroup)

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
            . _EMap

instance ToPath GetIdentityNotificationAttributes where
    toPath = const "/"

instance ToQuery GetIdentityNotificationAttributes where
    toQuery GetIdentityNotificationAttributes{..} = mconcat
        [ "Identities" =? _ginaIdentities
        ]

instance ToHeaders GetIdentityNotificationAttributes

instance AWSRequest GetIdentityNotificationAttributes where
    type Sv GetIdentityNotificationAttributes = SES
    type Rs GetIdentityNotificationAttributes = GetIdentityNotificationAttributesResponse

    request  = post "GetIdentityNotificationAttributes"
    response = xmlResponse

instance FromXML GetIdentityNotificationAttributesResponse where
    parseXML = withElement "GetIdentityNotificationAttributesResult" $ \x -> GetIdentityNotificationAttributesResponse
        <$> x .@? "NotificationAttributes" .!@ mempty
