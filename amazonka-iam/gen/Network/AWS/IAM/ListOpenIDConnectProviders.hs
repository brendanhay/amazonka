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

-- Module      : Network.AWS.IAM.ListOpenIDConnectProviders
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Lists information about the OpenID Connect providers in the AWS account.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListOpenIDConnectProviders.html>
module Network.AWS.IAM.ListOpenIDConnectProviders
    (
    -- * Request
      ListOpenIDConnectProviders
    -- ** Request constructor
    , listOpenIDConnectProviders

    -- * Response
    , ListOpenIDConnectProvidersResponse
    -- ** Response constructor
    , listOpenIDConnectProvidersResponse
    -- ** Response lenses
    , loidcprOpenIDConnectProviderList
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data ListOpenIDConnectProviders = ListOpenIDConnectProviders
    deriving (Eq, Ord, Show, Generic)

-- | 'ListOpenIDConnectProviders' constructor.
listOpenIDConnectProviders :: ListOpenIDConnectProviders
listOpenIDConnectProviders = ListOpenIDConnectProviders

newtype ListOpenIDConnectProvidersResponse = ListOpenIDConnectProvidersResponse
    { _loidcprOpenIDConnectProviderList :: List "member" OpenIDConnectProviderListEntry
    } deriving (Eq, Show, Monoid, Semigroup)

instance GHC.Exts.IsList ListOpenIDConnectProvidersResponse where
    type Item ListOpenIDConnectProvidersResponse = OpenIDConnectProviderListEntry

    fromList = ListOpenIDConnectProvidersResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _loidcprOpenIDConnectProviderList

-- | 'ListOpenIDConnectProvidersResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'loidcprOpenIDConnectProviderList' @::@ ['OpenIDConnectProviderListEntry']
--
listOpenIDConnectProvidersResponse :: ListOpenIDConnectProvidersResponse
listOpenIDConnectProvidersResponse = ListOpenIDConnectProvidersResponse
    { _loidcprOpenIDConnectProviderList = mempty
    }

-- | The list of IAM OpenID Connect providers in the AWS account.
loidcprOpenIDConnectProviderList :: Lens' ListOpenIDConnectProvidersResponse [OpenIDConnectProviderListEntry]
loidcprOpenIDConnectProviderList =
    lens _loidcprOpenIDConnectProviderList
        (\s a -> s { _loidcprOpenIDConnectProviderList = a })
            . _List

instance ToPath ListOpenIDConnectProviders where
    toPath = const "/"

instance ToQuery ListOpenIDConnectProviders where
    toQuery = const mempty

instance ToHeaders ListOpenIDConnectProviders

instance AWSRequest ListOpenIDConnectProviders where
    type Sv ListOpenIDConnectProviders = IAM
    type Rs ListOpenIDConnectProviders = ListOpenIDConnectProvidersResponse

    request  = post "ListOpenIDConnectProviders"
    response = xmlResponse

instance FromXML ListOpenIDConnectProvidersResponse where
    parseXML = withElement "ListOpenIDConnectProvidersResult" $ \x -> ListOpenIDConnectProvidersResponse
        <$> x .@? "OpenIDConnectProviderList" .!@ mempty
