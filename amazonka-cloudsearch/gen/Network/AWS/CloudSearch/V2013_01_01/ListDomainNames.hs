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
    , listDomainNames
    -- * Response
    , ListDomainNamesResponse
    -- ** Response lenses
    , ldnrDomainNames
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListDomainNames' request.
listDomainNames :: ListDomainNames
listDomainNames = ListDomainNames

data ListDomainNames = ListDomainNames
    deriving (Eq, Show, Generic)

instance ToQuery ListDomainNames where
    toQuery = genericQuery def

data ListDomainNamesResponse = ListDomainNamesResponse
    { _ldnrDomainNames :: Map Text Text
      -- ^ The names of the search domains owned by an account.
    } deriving (Show, Generic)

-- | The names of the search domains owned by an account.
ldnrDomainNames
    :: Functor f
    => (Map Text Text
    -> f (Map Text Text))
    -> ListDomainNamesResponse
    -> f ListDomainNamesResponse
ldnrDomainNames f x =
    (\y -> x { _ldnrDomainNames = y })
       <$> f (_ldnrDomainNames x)
{-# INLINE ldnrDomainNames #-}

instance FromXML ListDomainNamesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListDomainNames where
    type Sv ListDomainNames = CloudSearch
    type Rs ListDomainNames = ListDomainNamesResponse

    request = post "ListDomainNames"
    response _ = xmlResponse
