{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudSearch.V2013_01_01.DeleteIndexField
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes an IndexField from the search domain. For more information, see
-- Configuring Index Fields in the Amazon CloudSearch Developer Guide.
module Network.AWS.CloudSearch.V2013_01_01.DeleteIndexField where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

data DeleteIndexField = DeleteIndexField
    { _diftDomainName :: Text
      -- ^ A string that represents the name of a domain. Domain names are
      -- unique across the domains owned by an account within an AWS
      -- region. Domain names start with a letter or number and can
      -- contain the following characters: a-z (lowercase), 0-9, and -
      -- (hyphen).
    , _diftIndexFieldName :: Text
      -- ^ The name of the index field your want to remove from the domain's
      -- indexing options.
    } deriving (Generic)

makeLenses ''DeleteIndexField

instance ToQuery DeleteIndexField where
    toQuery = genericToQuery def

data DeleteIndexFieldResponse = DeleteIndexFieldResponse
    { _difuIndexField :: IndexFieldStatus
      -- ^ The status of the index field being deleted.
    } deriving (Generic)

makeLenses ''DeleteIndexFieldResponse

instance FromXML DeleteIndexFieldResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DeleteIndexField where
    type Sv DeleteIndexField = CloudSearch
    type Rs DeleteIndexField = DeleteIndexFieldResponse

    request = post "DeleteIndexField"
    response _ = xmlResponse
