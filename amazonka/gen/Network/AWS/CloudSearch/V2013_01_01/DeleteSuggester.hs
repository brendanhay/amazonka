{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudSearch.V2013_01_01.DeleteSuggester
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a suggester. For more information, see Getting Search Suggestions
-- in the Amazon CloudSearch Developer Guide.
module Network.AWS.CloudSearch.V2013_01_01.DeleteSuggester where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

data DeleteSuggester = DeleteSuggester
    { _dsvDomainName :: Text
      -- ^ A string that represents the name of a domain. Domain names are
      -- unique across the domains owned by an account within an AWS
      -- region. Domain names start with a letter or number and can
      -- contain the following characters: a-z (lowercase), 0-9, and -
      -- (hyphen).
    , _dsvSuggesterName :: Text
      -- ^ Specifies the name of the suggester you want to delete.
    } deriving (Show, Generic)

makeLenses ''DeleteSuggester

instance ToQuery DeleteSuggester where
    toQuery = genericToQuery def

data DeleteSuggesterResponse = DeleteSuggesterResponse
    { _dswSuggester :: SuggesterStatus
      -- ^ The status of the suggester being deleted.
    } deriving (Show, Generic)

makeLenses ''DeleteSuggesterResponse

instance AWSRequest DeleteSuggester where
    type Sv DeleteSuggester = CloudSearch
    type Rs DeleteSuggester = DeleteSuggesterResponse

    request = post "DeleteSuggester"
    response _ = cursorResponse $ \hs xml ->
        pure DeleteSuggesterResponse
            <*> xml %| "SuggesterStatus"
