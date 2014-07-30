{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
import           Network.AWS.SimpleDB.V2009_04_15.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data DeleteDomain = DeleteDomain
    { _ddrDomainName :: Text
      -- ^ The name of the domain to delete.
    } deriving (Generic)

instance ToQuery DeleteDomain where
    toQuery = genericToQuery def

instance AWSRequest DeleteDomain where
    type Sv DeleteDomain = SimpleDB
    type Rs DeleteDomain = DeleteDomainResponse

    request = post "DeleteDomain"
    response _ _ = return (Right DeleteDomainResponse)

data DeleteDomainResponse = DeleteDomainResponse
    deriving (Eq, Show, Generic)
