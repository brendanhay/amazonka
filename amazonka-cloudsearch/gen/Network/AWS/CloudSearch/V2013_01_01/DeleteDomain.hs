{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.V2013_01_01.DeleteDomain
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Permanently deletes a search domain and all of its data. Once a domain has
-- been deleted, it cannot be recovered. For more information, see Deleting a
-- Search Domain in the Amazon CloudSearch Developer Guide.
module Network.AWS.CloudSearch.V2013_01_01.DeleteDomain where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

data DeleteDomain = DeleteDomain
    { _ddyDomainName :: Text
      -- ^ The name of the domain you want to permanently delete.
    } deriving (Show, Generic)

makeLenses ''DeleteDomain

instance ToQuery DeleteDomain where
    toQuery = genericQuery def

data DeleteDomainResponse = DeleteDomainResponse
    { _ddzDomainStatus :: Maybe DomainStatus
      -- ^ The current status of the search domain.
    } deriving (Show, Generic)

makeLenses ''DeleteDomainResponse

instance FromXML DeleteDomainResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DeleteDomain where
    type Sv DeleteDomain = CloudSearch
    type Rs DeleteDomain = DeleteDomainResponse

    request = post "DeleteDomain"
    response _ = xmlResponse
