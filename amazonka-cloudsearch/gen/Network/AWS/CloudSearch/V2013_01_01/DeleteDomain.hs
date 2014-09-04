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
    , mkDeleteDomainRequest
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

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteDomain' request.
mkDeleteDomainRequest :: Text -- ^ 'ddrDomainName'
                      -> DeleteDomain
mkDeleteDomainRequest p1 = DeleteDomain
    { _ddrDomainName = p1
    }
{-# INLINE mkDeleteDomainRequest #-}

newtype DeleteDomain = DeleteDomain
    { _ddrDomainName :: Text
      -- ^ The name of the domain you want to permanently delete.
    } deriving (Show, Generic)

-- | The name of the domain you want to permanently delete.
ddrDomainName :: Lens' DeleteDomain (Text)
ddrDomainName = lens _ddrDomainName (\s a -> s { _ddrDomainName = a })
{-# INLINE ddrDomainName #-}

instance ToQuery DeleteDomain where
    toQuery = genericQuery def

newtype DeleteDomainResponse = DeleteDomainResponse
    { _ddsDomainStatus :: Maybe DomainStatus
      -- ^ The current status of the search domain.
    } deriving (Show, Generic)

-- | The current status of the search domain.
ddsDomainStatus :: Lens' DeleteDomainResponse (Maybe DomainStatus)
ddsDomainStatus = lens _ddsDomainStatus (\s a -> s { _ddsDomainStatus = a })
{-# INLINE ddsDomainStatus #-}

instance FromXML DeleteDomainResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DeleteDomain where
    type Sv DeleteDomain = CloudSearch
    type Rs DeleteDomain = DeleteDomainResponse

    request = post "DeleteDomain"
    response _ = xmlResponse
