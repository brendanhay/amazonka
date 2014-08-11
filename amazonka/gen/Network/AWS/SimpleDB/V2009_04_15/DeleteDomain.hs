{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SimpleDB.V2009_04_15.DeleteDomain
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DeleteDomain operation deletes a domain. Any items (and their
-- attributes) in the domain are deleted as well. The DeleteDomain operation
-- might take 10 or more seconds to complete. Running DeleteDomain on a domain
-- that does not exist or running the function multiple times using the same
-- domain name will not result in an error response.
module Network.AWS.SimpleDB.V2009_04_15.DeleteDomain where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.SimpleDB.V2009_04_15.Types
import Network.AWS.Prelude

data DeleteDomain = DeleteDomain
    { _ddrDomainName :: Text
      -- ^ The name of the domain to delete.
    } deriving (Show, Generic)

makeLenses ''DeleteDomain

instance ToQuery DeleteDomain where
    toQuery = genericToQuery def

data DeleteDomainResponse = DeleteDomainResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteDomainResponse

instance AWSRequest DeleteDomain where
    type Sv DeleteDomain = SimpleDB
    type Rs DeleteDomain = DeleteDomainResponse

    request = post "DeleteDomain"
    response _ = nullaryResponse DeleteDomainResponse
