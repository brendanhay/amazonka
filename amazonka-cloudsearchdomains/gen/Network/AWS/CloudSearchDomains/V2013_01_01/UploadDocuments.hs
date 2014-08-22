{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearchDomains.V2013_01_01.UploadDocuments
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Posts a batch of documents to a search domain for indexing. A document
-- batch is a collection of add and delete operations that represent the
-- documents you want to add, update, or delete from your domain. Batches can
-- be described in either JSON or XML. Each item that you want Amazon
-- CloudSearch to return as a search result (such as a product) is represented
-- as a document. Every document has a unique ID and one or more fields that
-- contain the data that you want to search and return in results. Individual
-- documents cannot contain more than 1 MB of data. The entire batch cannot
-- exceed 5 MB. To get the best possible upload performance, group add and
-- delete operations in batches that are close the 5 MB limit. Submitting a
-- large volume of single-document batches can overload a domain's document
-- service. The endpoint for submitting UploadDocuments requests is
-- domain-specific. To get the document endpoint for your domain, use the
-- Amazon CloudSearch configuration service DescribeDomains action. A domain's
-- endpoints are also displayed on the domain dashboard in the Amazon
-- CloudSearch console. For more information about formatting your data for
-- Amazon CloudSearch, see Preparing Your Data in the Amazon CloudSearch
-- Developer Guide. For more information about uploading data for indexing,
-- see Uploading Data in the Amazon CloudSearch Developer Guide.
module Network.AWS.CloudSearchDomains.V2013_01_01.UploadDocuments where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.CloudSearchDomains.V2013_01_01.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

data UploadDocuments = UploadDocuments
    { _udrDocuments :: RqBody
      -- ^ A batch of documents formatted in JSON or HTML.
    , _udrContentType :: ContentType
      -- ^ The format of the batch you are uploading. Amazon CloudSearch
      -- supports two document batch formats: application/json
      -- application/xml.
    } deriving (Show, Generic)

makeLenses ''UploadDocuments

instance ToPath UploadDocuments where
    toPath = const "/2013-01-01/documents/batch"

instance ToQuery UploadDocuments where
    toQuery UploadDocuments{..} = mconcat
        [ "format=sdk"
        ]

instance ToHeaders UploadDocuments where
    toHeaders UploadDocuments{..} = concat
        [ "Content-Type" =: _udrContentType
        ]

instance ToJSON UploadDocuments

data UploadDocumentsResponse = UploadDocumentsResponse
    { _udsAdds :: Maybe Integer
      -- ^ The number of documents that were added to the search domain.
    , _udsDeletes :: Maybe Integer
      -- ^ The number of documents that were deleted from the search domain.
    , _udsWarnings :: [DocumentServiceWarning]
      -- ^ Any warnings returned by the document service about the documents
      -- being uploaded.
    , _udsStatus :: Maybe Text
      -- ^ The status of an UploadDocumentsRequest.
    } deriving (Show, Generic)

makeLenses ''UploadDocumentsResponse

instance FromJSON UploadDocumentsResponse

instance AWSRequest UploadDocuments where
    type Sv UploadDocuments = CloudSearchDomains
    type Rs UploadDocuments = UploadDocumentsResponse

    request = post
    response _ = jsonResponse
