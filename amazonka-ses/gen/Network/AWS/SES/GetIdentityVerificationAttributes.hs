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

-- Module      : Network.AWS.SES.GetIdentityVerificationAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Given a list of identities (email addresses and/or domains), returns the
-- verification status and (for domain identities) the verification token for
-- each identity. This action is throttled at one request per second.
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

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SES.Types

newtype GetIdentityVerificationAttributes = GetIdentityVerificationAttributes
    { _givaIdentities :: [Text]
    } deriving
        ( Eq
        , Ord
        , Show
        , Generic
        , Foldable
        , Traversable
        , Monoid
        , Semigroup
        )

-- | 'GetIdentityVerificationAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'givaIdentities' @::@ ['Text']
--
getIdentityVerificationAttributes :: GetIdentityVerificationAttributes
getIdentityVerificationAttributes = GetIdentityVerificationAttributes
    { _givaIdentities = mempty
    }

-- | A list of identities.
givaIdentities :: Lens' GetIdentityVerificationAttributes [Text]
givaIdentities = lens _givaIdentities (\s a -> s { _givaIdentities = a })
instance ToQuery GetIdentityVerificationAttributes

instance ToPath GetIdentityVerificationAttributes where
    toPath = const "/"

newtype GetIdentityVerificationAttributesResponse = GetIdentityVerificationAttributesResponse
    { _givarVerificationAttributes :: Map Text IdentityVerificationAttributes
    } (Eq, Show, Generic, Foldable, Traversable, Monoid, Semigroup)

-- | 'GetIdentityVerificationAttributesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'givarVerificationAttributes' @::@ 'HashMap' 'Text' 'IdentityVerificationAttributes'
--
getIdentityVerificationAttributesResponse :: GetIdentityVerificationAttributesResponse
getIdentityVerificationAttributesResponse = GetIdentityVerificationAttributesResponse
    { _givarVerificationAttributes = mempty
    }

-- | A map of Identities to IdentityVerificationAttributes objects.
givarVerificationAttributes :: Lens' GetIdentityVerificationAttributesResponse (HashMap Text IdentityVerificationAttributes)
givarVerificationAttributes =
    lens _givarVerificationAttributes
        (\s a -> s { _givarVerificationAttributes = a })
            . _Map

instance FromXML GetIdentityVerificationAttributesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GetIdentityVerificationAttributesResponse"

instance AWSRequest GetIdentityVerificationAttributes where
    type Sv GetIdentityVerificationAttributes = SES
    type Rs GetIdentityVerificationAttributes = GetIdentityVerificationAttributesResponse

    request  = post "GetIdentityVerificationAttributes"
    response = xmlResponse $ \h x -> GetIdentityVerificationAttributesResponse
        <$> x %| "VerificationAttributes"
