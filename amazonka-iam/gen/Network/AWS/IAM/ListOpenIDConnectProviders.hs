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

-- Module      : Network.AWS.IAM.ListOpenIDConnectProviders
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists information about the OpenID Connect providers in the AWS account.
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

data ListOpenIDConnectProviders = ListOpenIDConnectProviders

-- | 'ListOpenIDConnectProviders' constructor.
listOpenIDConnectProviders :: ListOpenIDConnectProviders
listOpenIDConnectProviders = ListOpenIDConnectProviders

instance ToPath ListOpenIDConnectProviders where
    toPath = const "/"

instance ToQuery ListOpenIDConnectProviders

newtype ListOpenIDConnectProvidersResponse = ListOpenIDConnectProvidersResponse
    { _loidcprOpenIDConnectProviderList :: [OpenIDConnectProviderListEntry]
    } deriving (Eq, Show, Generic, Monoid)

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

instance AWSRequest ListOpenIDConnectProviders where
    type Sv ListOpenIDConnectProviders = IAM
    type Rs ListOpenIDConnectProviders = ListOpenIDConnectProvidersResponse

    request  = post "ListOpenIDConnectProviders"
    response = xmlResponse $ \h x -> ListOpenIDConnectProvidersResponse
        <$> x %| "OpenIDConnectProviderList"
