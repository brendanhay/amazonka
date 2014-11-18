{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.GetIdentityDkimAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the current status of Easy DKIM signing for an entity. For domain
-- name identities, this action also returns the DKIM tokens that are required
-- for Easy DKIM signing, and whether Amazon SES has successfully verified
-- that these tokens have been published. This action takes a list of
-- identities as input and returns the following information for each: Whether
-- Easy DKIM signing is enabled or disabled. A set of DKIM tokens that
-- represent the identity. If the identity is an email address, the tokens
-- represent the domain of that address. Whether Amazon SES has successfully
-- verified the DKIM tokens published in the domain's DNS. This information is
-- only returned for domain name identities, not for email addresses. This
-- action is throttled at one request per second. For more information about
-- creating DNS records using DKIM tokens, go to the Amazon SES Developer
-- Guide.
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
import Network.AWS.Request.Query
import Network.AWS.SES.Types
import qualified GHC.Exts

newtype GetIdentityDkimAttributes = GetIdentityDkimAttributes
    { _gidaIdentities :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList GetIdentityDkimAttributes where
    type Item GetIdentityDkimAttributes = Text

    fromList = GetIdentityDkimAttributes . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _gidaIdentities

-- | 'GetIdentityDkimAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gidaIdentities' @::@ ['Text']
--
getIdentityDkimAttributes :: GetIdentityDkimAttributes
getIdentityDkimAttributes = GetIdentityDkimAttributes
    { _gidaIdentities = mempty
    }

-- | A list of one or more verified identities - email addresses, domains, or
-- both.
gidaIdentities :: Lens' GetIdentityDkimAttributes [Text]
gidaIdentities = lens _gidaIdentities (\s a -> s { _gidaIdentities = a })

newtype GetIdentityDkimAttributesResponse = GetIdentityDkimAttributesResponse
    { _gidarDkimAttributes :: Map Text IdentityDkimAttributes
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

-- | 'GetIdentityDkimAttributesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gidarDkimAttributes' @::@ 'HashMap' 'Text' 'IdentityDkimAttributes'
--
getIdentityDkimAttributesResponse :: GetIdentityDkimAttributesResponse
getIdentityDkimAttributesResponse = GetIdentityDkimAttributesResponse
    { _gidarDkimAttributes = mempty
    }

-- | The DKIM attributes for an email address or a domain.
gidarDkimAttributes :: Lens' GetIdentityDkimAttributesResponse (HashMap Text IdentityDkimAttributes)
gidarDkimAttributes =
    lens _gidarDkimAttributes (\s a -> s { _gidarDkimAttributes = a })
        . _Map

instance ToPath GetIdentityDkimAttributes where
    toPath = const "/"

instance ToQuery GetIdentityDkimAttributes

instance ToHeaders GetIdentityDkimAttributes

instance AWSRequest GetIdentityDkimAttributes where
    type Sv GetIdentityDkimAttributes = SES
    type Rs GetIdentityDkimAttributes = GetIdentityDkimAttributesResponse

    request  = post "GetIdentityDkimAttributes"
    response = xmlResponse

instance FromXML GetIdentityDkimAttributesResponse where
    parseXML x = GetIdentityDkimAttributesResponse
        <$> x .@ "DkimAttributes"
