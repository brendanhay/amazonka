{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudSearch.V2013_01_01.DeleteAnalysisScheme
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes an analysis scheme. For more information, see Configuring Analysis
-- Schemes in the Amazon CloudSearch Developer Guide.
module Network.AWS.CloudSearch.V2013_01_01.DeleteAnalysisScheme where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

data DeleteAnalysisScheme = DeleteAnalysisScheme
    { _dasrDomainName :: Text
      -- ^ A string that represents the name of a domain. Domain names are
      -- unique across the domains owned by an account within an AWS
      -- region. Domain names start with a letter or number and can
      -- contain the following characters: a-z (lowercase), 0-9, and -
      -- (hyphen).
    , _dasrAnalysisSchemeName :: Text
      -- ^ The name of the analysis scheme you want to delete.
    } deriving (Generic)

makeLenses ''DeleteAnalysisScheme

instance ToQuery DeleteAnalysisScheme where
    toQuery = genericToQuery def

data DeleteAnalysisSchemeResponse = DeleteAnalysisSchemeResponse
    { _dassAnalysisScheme :: AnalysisSchemeStatus
      -- ^ The status of the analysis scheme being deleted.
    } deriving (Generic)

makeLenses ''DeleteAnalysisSchemeResponse

instance FromXML DeleteAnalysisSchemeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DeleteAnalysisScheme where
    type Sv DeleteAnalysisScheme = CloudSearch
    type Rs DeleteAnalysisScheme = DeleteAnalysisSchemeResponse

    request = post "DeleteAnalysisScheme"
    response _ = xmlResponse
