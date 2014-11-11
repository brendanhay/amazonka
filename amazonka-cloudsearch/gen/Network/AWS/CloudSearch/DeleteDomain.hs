{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.CloudSearch.DeleteDomain
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
module Network.AWS.CloudSearch.DeleteDomain
    (
    -- * Request
      DeleteDomain
    -- ** Request constructor
    , deleteDomain
    -- ** Request lenses
    , ddDomainName

    -- * Response
    , DeleteDomainResponse
    -- ** Response constructor
    , deleteDomainResponse
    -- ** Response lenses
    , ddrDomainStatus
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudSearch.Types

newtype DeleteDomain = DeleteDomain
    { _ddDomainName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'DeleteDomain' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddDomainName' @::@ 'Text'
--
deleteDomain :: Text -- ^ 'ddDomainName'
             -> DeleteDomain
deleteDomain p1 = DeleteDomain
    { _ddDomainName = p1
    }

-- | The name of the domain you want to permanently delete.
ddDomainName :: Lens' DeleteDomain Text
ddDomainName = lens _ddDomainName (\s a -> s { _ddDomainName = a })
instance ToQuery DeleteDomain

instance ToPath DeleteDomain where
    toPath = const "/"

newtype DeleteDomainResponse = DeleteDomainResponse
    { _ddrDomainStatus :: Maybe DomainStatus
    } deriving (Eq, Show, Generic)

-- | 'DeleteDomainResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddrDomainStatus' @::@ 'Maybe' 'DomainStatus'
--
deleteDomainResponse :: DeleteDomainResponse
deleteDomainResponse = DeleteDomainResponse
    { _ddrDomainStatus = Nothing
    }

ddrDomainStatus :: Lens' DeleteDomainResponse (Maybe DomainStatus)
ddrDomainStatus = lens _ddrDomainStatus (\s a -> s { _ddrDomainStatus = a })
instance FromXML DeleteDomainResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteDomainResponse"

instance AWSRequest DeleteDomain where
    type Sv DeleteDomain = CloudSearch
    type Rs DeleteDomain = DeleteDomainResponse

    request  = post "DeleteDomain"
    response = xmlResponse $ \h x -> DeleteDomainResponse
        <$> x %| "DomainStatus"
