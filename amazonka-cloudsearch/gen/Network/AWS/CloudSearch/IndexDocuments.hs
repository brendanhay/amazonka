{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.CloudSearch.IndexDocuments
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
module Network.AWS.CloudSearch.IndexDocuments
    (
    -- * Request
      IndexDocuments
    -- ** Request constructor
    , indexDocuments
    -- ** Request lenses
    , idDomainName

    -- * Response
    , IndexDocumentsResponse
    -- ** Response constructor
    , indexDocumentsResponse
    -- ** Response lenses
    , idrFieldNames
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudSearch.Types
import qualified GHC.Exts

newtype IndexDocuments = IndexDocuments
    { _idDomainName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'IndexDocuments' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'idDomainName' @::@ 'Text'
--
indexDocuments :: Text -- ^ 'idDomainName'
               -> IndexDocuments
indexDocuments p1 = IndexDocuments
    { _idDomainName = p1
    }

idDomainName :: Lens' IndexDocuments Text
idDomainName = lens _idDomainName (\s a -> s { _idDomainName = a })

instance ToQuery IndexDocuments

instance ToPath IndexDocuments where
    toPath = const "/"

newtype IndexDocumentsResponse = IndexDocumentsResponse
    { _idrFieldNames :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList IndexDocumentsResponse where
    type Item IndexDocumentsResponse = Text

    fromList = IndexDocumentsResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _idrFieldNames

-- | 'IndexDocumentsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'idrFieldNames' @::@ ['Text']
--
indexDocumentsResponse :: IndexDocumentsResponse
indexDocumentsResponse = IndexDocumentsResponse
    { _idrFieldNames = mempty
    }

-- | The names of the fields that are currently being indexed.
idrFieldNames :: Lens' IndexDocumentsResponse [Text]
idrFieldNames = lens _idrFieldNames (\s a -> s { _idrFieldNames = a })

instance AWSRequest IndexDocuments where
    type Sv IndexDocuments = CloudSearch
    type Rs IndexDocuments = IndexDocumentsResponse

    request  = post "IndexDocuments"
    response = xmlResponse $ \h x -> IndexDocumentsResponse
        <$> x %| "FieldNames"
