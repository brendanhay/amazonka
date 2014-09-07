{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.V2013_01_01.IndexDocuments
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Tells the search domain to start indexing its documents using the latest
-- indexing options. This operation must be invoked to activate options whose
-- OptionStatus is RequiresIndexDocuments.
module Network.AWS.CloudSearch.V2013_01_01.IndexDocuments
    (
    -- * Request
      IndexDocuments
    -- ** Request constructor
    , mkIndexDocuments
    -- ** Request lenses
    , idDomainName

    -- * Response
    , IndexDocumentsResponse
    -- ** Response lenses
    , idrsFieldNames
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

-- | Container for the parameters to the IndexDocuments operation. Specifies the
-- name of the domain you want to re-index.
newtype IndexDocuments = IndexDocuments
    { _idDomainName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'IndexDocuments' request.
mkIndexDocuments :: Text -- ^ 'idDomainName'
                 -> IndexDocuments
mkIndexDocuments p1 = IndexDocuments
    { _idDomainName = p1
    }

-- | A string that represents the name of a domain. Domain names are unique
-- across the domains owned by an account within an AWS region. Domain names
-- start with a letter or number and can contain the following characters: a-z
-- (lowercase), 0-9, and - (hyphen).
idDomainName :: Lens' IndexDocuments Text
idDomainName = lens _idDomainName (\s a -> s { _idDomainName = a })

instance ToQuery IndexDocuments where
    toQuery = genericQuery def

-- | The result of an IndexDocuments request. Contains the status of the
-- indexing operation, including the fields being indexed.
newtype IndexDocumentsResponse = IndexDocumentsResponse
    { _idrsFieldNames :: [Text]
    } deriving (Show, Generic)

-- | The names of the fields that are currently being indexed.
idrsFieldNames :: Lens' IndexDocumentsResponse [Text]
idrsFieldNames = lens _idrsFieldNames (\s a -> s { _idrsFieldNames = a })

instance FromXML IndexDocumentsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest IndexDocuments where
    type Sv IndexDocuments = CloudSearch
    type Rs IndexDocuments = IndexDocumentsResponse

    request = post "IndexDocuments"
    response _ = xmlResponse
