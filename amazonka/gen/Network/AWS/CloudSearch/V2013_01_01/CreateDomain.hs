{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudSearch.V2013_01_01.CreateDomain
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new search domain. For more information, see Creating a Search
-- Domain in the Amazon CloudSearch Developer Guide.
module Network.AWS.CloudSearch.V2013_01_01.CreateDomain where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

data CreateDomain = CreateDomain
    { _cdrDomainName :: Text
      -- ^ A name for the domain you are creating. Allowed characters are
      -- a-z (lower-case letters), 0-9, and hyphen (-). Domain names must
      -- start with a letter or number and be at least 3 and no more than
      -- 28 characters long.
    } deriving (Generic)

makeLenses ''CreateDomain

instance ToQuery CreateDomain where
    toQuery = genericToQuery def

data CreateDomainResponse = CreateDomainResponse
    { _cdsDomainStatus :: Maybe DomainStatus
      -- ^ The current status of the search domain.
    } deriving (Generic)

makeLenses ''CreateDomainResponse

instance FromXML CreateDomainResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateDomain where
    type Sv CreateDomain = CloudSearch
    type Rs CreateDomain = CreateDomainResponse

    request = post "CreateDomain"
    response _ = xmlResponse
