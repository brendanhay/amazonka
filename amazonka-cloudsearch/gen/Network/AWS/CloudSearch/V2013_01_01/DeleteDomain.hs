{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
module Network.AWS.CloudSearch.V2013_01_01.DeleteDomain
    (
    -- * Request
      DeleteDomain
    -- ** Request constructor
    , deleteDomain
    -- ** Request lenses
    , ddrDomainName

    -- * Response
    , DeleteDomainResponse
    -- ** Response lenses
    , ddsDomainStatus
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteDomain' request.
deleteDomain :: Text -- ^ 'ddrDomainName'
             -> DeleteDomain
deleteDomain p1 = DeleteDomain
    { _ddrDomainName = p1
    }
{-# INLINE deleteDomain #-}

data DeleteDomain = DeleteDomain
    { _ddrDomainName :: Text
      -- ^ The name of the domain you want to permanently delete.
    } deriving (Show, Generic)

-- | The name of the domain you want to permanently delete.
ddrDomainName :: Lens' DeleteDomain (Text)
ddrDomainName f x =
    f (_ddrDomainName x)
        <&> \y -> x { _ddrDomainName = y }
{-# INLINE ddrDomainName #-}

instance ToQuery DeleteDomain where
    toQuery = genericQuery def

data DeleteDomainResponse = DeleteDomainResponse
    { _ddsDomainStatus :: Maybe DomainStatus
      -- ^ The current status of the search domain.
    } deriving (Show, Generic)

-- | The current status of the search domain.
ddsDomainStatus :: Lens' DeleteDomainResponse (Maybe DomainStatus)
ddsDomainStatus f x =
    f (_ddsDomainStatus x)
        <&> \y -> x { _ddsDomainStatus = y }
{-# INLINE ddsDomainStatus #-}

instance FromXML DeleteDomainResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DeleteDomain where
    type Sv DeleteDomain = CloudSearch
    type Rs DeleteDomain = DeleteDomainResponse

    request = post "DeleteDomain"
    response _ = xmlResponse
