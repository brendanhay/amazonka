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
    , mkDescribeAnalysisSchemes
    -- ** Request lenses
    , das2DomainName
    , das2AnalysisSchemeNames
    , das2Deployed

    -- * Response
    , DescribeAnalysisSchemesResponse
    -- ** Response lenses
    , dasrs1AnalysisSchemes
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

-- | Container for the parameters to the DescribeAnalysisSchemes operation.
-- Specifies the name of the domain you want to describe. To limit the
-- response to particular analysis schemes, specify the names of the analysis
-- schemes you want to describe. To show the active configuration and exclude
-- any pending changes, set the Deployed option to true.
data DescribeAnalysisSchemes = DescribeAnalysisSchemes
    { _das2DomainName :: Text
    , _das2AnalysisSchemeNames :: [Text]
    , _das2Deployed :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeAnalysisSchemes' request.
mkDescribeAnalysisSchemes :: Text -- ^ 'das2DomainName'
                          -> DescribeAnalysisSchemes
mkDescribeAnalysisSchemes p1 = DescribeAnalysisSchemes
    { _das2DomainName = p1
    , _das2AnalysisSchemeNames = mempty
    , _das2Deployed = Nothing
    }

-- | The name of the domain you want to describe.
das2DomainName :: Lens' DescribeAnalysisSchemes Text
das2DomainName = lens _das2DomainName (\s a -> s { _das2DomainName = a })

-- | The analysis schemes you want to describe.
das2AnalysisSchemeNames :: Lens' DescribeAnalysisSchemes [Text]
das2AnalysisSchemeNames =
    lens _das2AnalysisSchemeNames
         (\s a -> s { _das2AnalysisSchemeNames = a })

-- | Whether to display the deployed configuration (true) or include any pending
-- changes (false). Defaults to false.
das2Deployed :: Lens' DescribeAnalysisSchemes (Maybe Bool)
das2Deployed = lens _das2Deployed (\s a -> s { _das2Deployed = a })

instance ToQuery DescribeAnalysisSchemes where
    toQuery = genericQuery def

-- | The result of a DescribeAnalysisSchemes request. Contains the analysis
-- schemes configured for the domain specified in the request.
newtype DescribeAnalysisSchemesResponse = DescribeAnalysisSchemesResponse
    { _dasrs1AnalysisSchemes :: [AnalysisSchemeStatus]
    } deriving (Show, Generic)

-- | The analysis scheme descriptions.
dasrs1AnalysisSchemes :: Lens' DescribeAnalysisSchemesResponse [AnalysisSchemeStatus]
dasrs1AnalysisSchemes =
    lens _dasrs1AnalysisSchemes (\s a -> s { _dasrs1AnalysisSchemes = a })

instance FromXML DescribeAnalysisSchemesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeAnalysisSchemes where
    type Sv DescribeAnalysisSchemes = CloudSearch
    type Rs DescribeAnalysisSchemes = DescribeAnalysisSchemesResponse

    request = post "DescribeAnalysisSchemes"
    response _ = xmlResponse
