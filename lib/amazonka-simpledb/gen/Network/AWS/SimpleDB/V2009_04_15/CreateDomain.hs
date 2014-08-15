{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Module      : Network.AWS.SimpleDB.V2009_04_15.CreateDomain
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CreateDomain operation creates a new domain. The domain name should be
-- unique among the domains associated with the Access Key ID provided in the
-- request. The CreateDomain operation may take 10 or more seconds to
-- complete. CreateDomain is an idempotent operation; running it multiple
-- times using the same domain name will not result in an error response. The
-- client can create up to 100 domains per account. If the client requires
-- additional domains, go to
-- http://aws.amazon.com/contact-us/simpledb-limit-request/.
module Network.AWS.SimpleDB.V2009_04_15.CreateDomain where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.SimpleDB.V2009_04_15.Types
import Network.AWS.Prelude

data CreateDomain = CreateDomain
    { _cdrDomainName :: Text
      -- ^ The name of the domain to create. The name can range between 3
      -- and 255 characters and can contain the following characters: a-z,
      -- A-Z, 0-9, '_', '-', and '.'.
    } deriving (Show, Generic)

makeLenses ''CreateDomain

instance ToQuery CreateDomain where
    toQuery = genericQuery def

data CreateDomainResponse = CreateDomainResponse
    deriving (Eq, Show, Generic)

makeLenses ''CreateDomainResponse

instance AWSRequest CreateDomain where
    type Sv CreateDomain = SimpleDB
    type Rs CreateDomain = CreateDomainResponse

    request = post "CreateDomain"
    response _ = nullaryResponse CreateDomainResponse
