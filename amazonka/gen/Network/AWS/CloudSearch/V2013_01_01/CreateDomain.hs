{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.CloudSearch.V2013_01_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data CreateDomain = CreateDomain
    { _cdrDomainName :: Text
      -- ^ A name for the domain you are creating. Allowed characters are
      -- a-z (lower-case letters), 0-9, and hyphen (-). Domain names must
      -- start with a letter or number and be at least 3 and no more than
      -- 28 characters long.
    } deriving (Generic)

instance ToQuery CreateDomain where
    toQuery = genericToQuery def

instance AWSRequest CreateDomain where
    type Sv CreateDomain = CloudSearch
    type Rs CreateDomain = CreateDomainResponse

    request = post "CreateDomain"
    response _ = xmlResponse

data CreateDomainResponse = CreateDomainResponse
    { _cdsDomainStatus :: Maybe DomainStatus
      -- ^ The current status of the search domain.
    } deriving (Generic)

instance FromXML CreateDomainResponse where
    fromXMLOptions = xmlOptions
