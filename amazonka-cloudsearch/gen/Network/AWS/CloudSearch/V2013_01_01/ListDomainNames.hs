{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.V2013_01_01.ListDomainNames
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists all search domains owned by an account.
module Network.AWS.CloudSearch.V2013_01_01.ListDomainNames
    (
    -- * Request
      ListDomainNames
    -- ** Request constructor
    , mkListDomainNames
    -- * Response
    , ListDomainNamesResponse
    -- ** Response lenses
    , ldnrsDomainNames
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

data ListDomainNames = ListDomainNames
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListDomainNames' request.
mkListDomainNames :: ListDomainNames
mkListDomainNames = ListDomainNames

instance ToQuery ListDomainNames where
    toQuery = genericQuery def

-- | The result of a ListDomainNames request. Contains a list of the domains
-- owned by an account.
newtype ListDomainNamesResponse = ListDomainNamesResponse
    { _ldnrsDomainNames :: Map Text Text
    } deriving (Show, Generic)

-- | The names of the search domains owned by an account.
ldnrsDomainNames :: Lens' ListDomainNamesResponse (Map Text Text)
ldnrsDomainNames =
    lens _ldnrsDomainNames (\s a -> s { _ldnrsDomainNames = a })

instance FromXML ListDomainNamesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListDomainNames where
    type Sv ListDomainNames = CloudSearch
    type Rs ListDomainNames = ListDomainNamesResponse

    request = post "ListDomainNames"
    response _ = xmlResponse
