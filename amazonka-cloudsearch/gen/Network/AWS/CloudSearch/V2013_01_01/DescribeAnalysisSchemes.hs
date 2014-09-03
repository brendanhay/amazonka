{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.CloudSearch.V2013_01_01.DescribeAnalysisSchemes
    (
    -- * Request
      DescribeAnalysisSchemes
    -- ** Request constructor
    , describeAnalysisSchemes
    -- ** Request lenses
    , dasvDomainName
    , dasvDeployed
    , dasvAnalysisSchemeNames

    -- * Response
    , DescribeAnalysisSchemesResponse
    -- ** Response lenses
    , daswAnalysisSchemes
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeAnalysisSchemes' request.
describeAnalysisSchemes :: Text -- ^ 'dasvDomainName'
                        -> DescribeAnalysisSchemes
describeAnalysisSchemes p1 = DescribeAnalysisSchemes
    { _dasvDomainName = p1
    , _dasvDeployed = Nothing
    , _dasvAnalysisSchemeNames = mempty
    }

data DescribeAnalysisSchemes = DescribeAnalysisSchemes
    { _dasvDomainName :: Text
      -- ^ The name of the domain you want to describe.
    , _dasvDeployed :: Maybe Bool
      -- ^ Whether to display the deployed configuration (true) or include
      -- any pending changes (false). Defaults to false.
    , _dasvAnalysisSchemeNames :: [Text]
      -- ^ The analysis schemes you want to describe.
    } deriving (Show, Generic)

-- | The name of the domain you want to describe.
dasvDomainName
    :: Functor f
    => (Text
    -> f (Text))
    -> DescribeAnalysisSchemes
    -> f DescribeAnalysisSchemes
dasvDomainName f x =
    (\y -> x { _dasvDomainName = y })
       <$> f (_dasvDomainName x)
{-# INLINE dasvDomainName #-}

-- | Whether to display the deployed configuration (true) or include any pending
-- changes (false). Defaults to false.
dasvDeployed
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> DescribeAnalysisSchemes
    -> f DescribeAnalysisSchemes
dasvDeployed f x =
    (\y -> x { _dasvDeployed = y })
       <$> f (_dasvDeployed x)
{-# INLINE dasvDeployed #-}

-- | The analysis schemes you want to describe.
dasvAnalysisSchemeNames
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> DescribeAnalysisSchemes
    -> f DescribeAnalysisSchemes
dasvAnalysisSchemeNames f x =
    (\y -> x { _dasvAnalysisSchemeNames = y })
       <$> f (_dasvAnalysisSchemeNames x)
{-# INLINE dasvAnalysisSchemeNames #-}

instance ToQuery DescribeAnalysisSchemes where
    toQuery = genericQuery def

data DescribeAnalysisSchemesResponse = DescribeAnalysisSchemesResponse
    { _daswAnalysisSchemes :: [AnalysisSchemeStatus]
      -- ^ The analysis scheme descriptions.
    } deriving (Show, Generic)

-- | The analysis scheme descriptions.
daswAnalysisSchemes
    :: Functor f
    => ([AnalysisSchemeStatus]
    -> f ([AnalysisSchemeStatus]))
    -> DescribeAnalysisSchemesResponse
    -> f DescribeAnalysisSchemesResponse
daswAnalysisSchemes f x =
    (\y -> x { _daswAnalysisSchemes = y })
       <$> f (_daswAnalysisSchemes x)
{-# INLINE daswAnalysisSchemes #-}

instance FromXML DescribeAnalysisSchemesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeAnalysisSchemes where
    type Sv DescribeAnalysisSchemes = CloudSearch
    type Rs DescribeAnalysisSchemes = DescribeAnalysisSchemesResponse

    request = post "DescribeAnalysisSchemes"
    response _ = xmlResponse
