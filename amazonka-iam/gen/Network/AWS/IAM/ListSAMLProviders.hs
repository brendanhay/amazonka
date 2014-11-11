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

-- Module      : Network.AWS.IAM.ListSAMLProviders
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the SAML providers in the account.
module Network.AWS.IAM.ListSAMLProviders
    (
    -- * Request
      ListSAMLProviders
    -- ** Request constructor
    , listSAMLProviders

    -- * Response
    , ListSAMLProvidersResponse
    -- ** Response constructor
    , listSAMLProvidersResponse
    -- ** Response lenses
    , lsamlprSAMLProviderList
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types

data ListSAMLProviders = ListSAMLProviders
    deriving (Eq, Ord, Show, Generic)

-- | 'ListSAMLProviders' constructor.
listSAMLProviders :: ListSAMLProviders
listSAMLProviders = ListSAMLProviders
instance ToQuery ListSAMLProviders

instance ToPath ListSAMLProviders where
    toPath = const "/"

newtype ListSAMLProvidersResponse = ListSAMLProvidersResponse
    { _lsamlprSAMLProviderList :: [SAMLProviderListEntry]
    } deriving (Eq, Show, Generic, Monoid)

-- | 'ListSAMLProvidersResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsamlprSAMLProviderList' @::@ ['SAMLProviderListEntry']
--
listSAMLProvidersResponse :: ListSAMLProvidersResponse
listSAMLProvidersResponse = ListSAMLProvidersResponse
    { _lsamlprSAMLProviderList = mempty
    }

-- | The list of SAML providers for this account.
lsamlprSAMLProviderList :: Lens' ListSAMLProvidersResponse [SAMLProviderListEntry]
lsamlprSAMLProviderList =
    lens _lsamlprSAMLProviderList (\s a -> s { _lsamlprSAMLProviderList = a })
instance FromXML ListSAMLProvidersResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ListSAMLProvidersResponse"

instance AWSRequest ListSAMLProviders where
    type Sv ListSAMLProviders = IAM
    type Rs ListSAMLProviders = ListSAMLProvidersResponse

    request  = post "ListSAMLProviders"
    response = xmlResponse $ \h x -> ListSAMLProvidersResponse
        <$> x %| "SAMLProviderList"
