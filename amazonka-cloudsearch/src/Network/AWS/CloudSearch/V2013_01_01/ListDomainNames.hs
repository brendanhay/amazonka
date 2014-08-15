{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

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
module Network.AWS.CloudSearch.V2013_01_01.ListDomainNames where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

data ListDomainNames = ListDomainNames
    deriving (Eq, Show, Generic)

makeLenses ''ListDomainNames

instance ToQuery ListDomainNames where
    toQuery = genericQuery def

data ListDomainNamesResponse = ListDomainNamesResponse
    { _ldnrDomainNames :: Map Text Text
      -- ^ The names of the search domains owned by an account.
    } deriving (Show, Generic)

makeLenses ''ListDomainNamesResponse

instance FromXML ListDomainNamesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListDomainNames where
    type Sv ListDomainNames = CloudSearch
    type Rs ListDomainNames = ListDomainNamesResponse

    request = post "ListDomainNames"
    response _ = xmlResponse
