{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Module      : Network.AWS.CloudSearch.V2013_01_01.DescribeAnalysisSchemes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets the analysis schemes configured for a domain. An analysis scheme
-- defines language-specific text processing options for a text field. Can be
-- limited to specific analysis schemes by name. By default, shows all
-- analysis schemes and includes any pending changes to the configuration. Set
-- the Deployed option to true to show the active configuration and exclude
-- pending changes. For more information, see Configuring Analysis Schemes in
-- the Amazon CloudSearch Developer Guide.
module Network.AWS.CloudSearch.V2013_01_01.DescribeAnalysisSchemes where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeAnalysisSchemes' request.
describeAnalysisSchemes :: Text -- ^ '_dastDomainName'
                        -> DescribeAnalysisSchemes
describeAnalysisSchemes p1 = DescribeAnalysisSchemes
    { _dastDomainName = p1
    , _dastDeployed = Nothing
    , _dastAnalysisSchemeNames = mempty
    }

data DescribeAnalysisSchemes = DescribeAnalysisSchemes
    { _dastDomainName :: Text
      -- ^ The name of the domain you want to describe.
    , _dastDeployed :: Maybe Bool
      -- ^ Whether to display the deployed configuration (true) or include
      -- any pending changes (false). Defaults to false.
    , _dastAnalysisSchemeNames :: [Text]
      -- ^ The analysis schemes you want to describe.
    } deriving (Show, Generic)

makeLenses ''DescribeAnalysisSchemes

instance ToQuery DescribeAnalysisSchemes where
    toQuery = genericQuery def

data DescribeAnalysisSchemesResponse = DescribeAnalysisSchemesResponse
    { _dasuAnalysisSchemes :: [AnalysisSchemeStatus]
      -- ^ The analysis scheme descriptions.
    } deriving (Show, Generic)

makeLenses ''DescribeAnalysisSchemesResponse

instance FromXML DescribeAnalysisSchemesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeAnalysisSchemes where
    type Sv DescribeAnalysisSchemes = CloudSearch
    type Rs DescribeAnalysisSchemes = DescribeAnalysisSchemesResponse

    request = post "DescribeAnalysisSchemes"
    response _ = xmlResponse
