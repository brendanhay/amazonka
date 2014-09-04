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
    , indexDocuments
    -- ** Request lenses
    , idrDomainName

    -- * Response
    , IndexDocumentsResponse
    -- ** Response lenses
    , idsFieldNames
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'IndexDocuments' request.
indexDocuments :: Text -- ^ 'idrDomainName'
               -> IndexDocuments
indexDocuments p1 = IndexDocuments
    { _idrDomainName = p1
    }
{-# INLINE indexDocuments #-}

data IndexDocuments = IndexDocuments
    { _idrDomainName :: Text
      -- ^ A string that represents the name of a domain. Domain names are
      -- unique across the domains owned by an account within an AWS
      -- region. Domain names start with a letter or number and can
      -- contain the following characters: a-z (lowercase), 0-9, and -
      -- (hyphen).
    } deriving (Show, Generic)

-- | A string that represents the name of a domain. Domain names are unique
-- across the domains owned by an account within an AWS region. Domain names
-- start with a letter or number and can contain the following characters: a-z
-- (lowercase), 0-9, and - (hyphen).
idrDomainName :: Lens' IndexDocuments (Text)
idrDomainName f x =
    f (_idrDomainName x)
        <&> \y -> x { _idrDomainName = y }
{-# INLINE idrDomainName #-}

instance ToQuery IndexDocuments where
    toQuery = genericQuery def

data IndexDocumentsResponse = IndexDocumentsResponse
    { _idsFieldNames :: [Text]
      -- ^ The names of the fields that are currently being indexed.
    } deriving (Show, Generic)

-- | The names of the fields that are currently being indexed.
idsFieldNames :: Lens' IndexDocumentsResponse ([Text])
idsFieldNames f x =
    f (_idsFieldNames x)
        <&> \y -> x { _idsFieldNames = y }
{-# INLINE idsFieldNames #-}

instance FromXML IndexDocumentsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest IndexDocuments where
    type Sv IndexDocuments = CloudSearch
    type Rs IndexDocuments = IndexDocumentsResponse

    request = post "IndexDocuments"
    response _ = xmlResponse
