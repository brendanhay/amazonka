{-# LANGUAGE TemplateHaskell             #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.V2013_01_01.Lenses
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.CloudSearch.V2013_01_01.Lenses where

import Control.Lens.TH
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.CloudSearch.V2013_01_01.DescribeAvailabilityOptions
import Network.AWS.CloudSearch.V2013_01_01.DescribeExpressions
import Network.AWS.CloudSearch.V2013_01_01.DefineExpression
import Network.AWS.CloudSearch.V2013_01_01.DescribeScalingParameters
import Network.AWS.CloudSearch.V2013_01_01.DescribeServiceAccessPolicies
import Network.AWS.CloudSearch.V2013_01_01.DescribeSuggesters
import Network.AWS.CloudSearch.V2013_01_01.UpdateAvailabilityOptions
import Network.AWS.CloudSearch.V2013_01_01.DeleteExpression
import Network.AWS.CloudSearch.V2013_01_01.ListDomainNames
import Network.AWS.CloudSearch.V2013_01_01.DefineSuggester
import Network.AWS.CloudSearch.V2013_01_01.DescribeDomains
import Network.AWS.CloudSearch.V2013_01_01.DeleteAnalysisScheme
import Network.AWS.CloudSearch.V2013_01_01.DescribeAnalysisSchemes
import Network.AWS.CloudSearch.V2013_01_01.CreateDomain
import Network.AWS.CloudSearch.V2013_01_01.DescribeIndexFields
import Network.AWS.CloudSearch.V2013_01_01.DeleteSuggester
import Network.AWS.CloudSearch.V2013_01_01.DefineAnalysisScheme
import Network.AWS.CloudSearch.V2013_01_01.IndexDocuments
import Network.AWS.CloudSearch.V2013_01_01.DeleteIndexField
import Network.AWS.CloudSearch.V2013_01_01.UpdateServiceAccessPolicies
import Network.AWS.CloudSearch.V2013_01_01.UpdateScalingParameters
import Network.AWS.CloudSearch.V2013_01_01.BuildSuggesters
import Network.AWS.CloudSearch.V2013_01_01.DeleteDomain
import Network.AWS.CloudSearch.V2013_01_01.DefineIndexField

-- Newtypes
makeIso ''ServiceEndpoint

-- Products
makeLenses ''AccessPoliciesStatus
makeLenses ''AnalysisOptions
makeLenses ''AnalysisScheme
makeLenses ''AnalysisSchemeStatus
makeLenses ''AvailabilityOptionsStatus
makeLenses ''DateArrayOptions
makeLenses ''DateOptions
makeLenses ''DocumentSuggesterOptions
makeLenses ''DomainStatus
makeLenses ''DoubleArrayOptions
makeLenses ''DoubleOptions
makeLenses ''Expression
makeLenses ''ExpressionStatus
makeLenses ''IndexField
makeLenses ''IndexFieldStatus
makeLenses ''IntArrayOptions
makeLenses ''IntOptions
makeLenses ''LatLonOptions
makeLenses ''LiteralArrayOptions
makeLenses ''LiteralOptions
makeLenses ''OptionStatus
makeLenses ''ScalingParameters
makeLenses ''ScalingParametersStatus
makeLenses ''Suggester
makeLenses ''SuggesterStatus
makeLenses ''TextArrayOptions
makeLenses ''TextOptions

-- Requests
makeLenses ''DescribeAvailabilityOptions
makeLenses ''DescribeExpressions
makeLenses ''DefineExpression
makeLenses ''DescribeScalingParameters
makeLenses ''DescribeServiceAccessPolicies
makeLenses ''DescribeSuggesters
makeLenses ''UpdateAvailabilityOptions
makeLenses ''DeleteExpression
makeLenses ''ListDomainNames
makeLenses ''DefineSuggester
makeLenses ''DescribeDomains
makeLenses ''DeleteAnalysisScheme
makeLenses ''DescribeAnalysisSchemes
makeLenses ''CreateDomain
makeLenses ''DescribeIndexFields
makeLenses ''DeleteSuggester
makeLenses ''DefineAnalysisScheme
makeLenses ''IndexDocuments
makeLenses ''DeleteIndexField
makeLenses ''UpdateServiceAccessPolicies
makeLenses ''UpdateScalingParameters
makeLenses ''BuildSuggesters
makeLenses ''DeleteDomain
makeLenses ''DefineIndexField

-- Responses
makeLenses ''DescribeAvailabilityOptionsResponse
makeLenses ''DescribeExpressionsResponse
makeLenses ''DefineExpressionResponse
makeLenses ''DescribeScalingParametersResponse
makeLenses ''DescribeServiceAccessPoliciesResponse
makeLenses ''DescribeSuggestersResponse
makeLenses ''UpdateAvailabilityOptionsResponse
makeLenses ''DeleteExpressionResponse
makeLenses ''ListDomainNamesResponse
makeLenses ''DefineSuggesterResponse
makeLenses ''DescribeDomainsResponse
makeLenses ''DeleteAnalysisSchemeResponse
makeLenses ''DescribeAnalysisSchemesResponse
makeLenses ''CreateDomainResponse
makeLenses ''DescribeIndexFieldsResponse
makeLenses ''DeleteSuggesterResponse
makeLenses ''DefineAnalysisSchemeResponse
makeLenses ''IndexDocumentsResponse
makeLenses ''DeleteIndexFieldResponse
makeLenses ''UpdateServiceAccessPoliciesResponse
makeLenses ''UpdateScalingParametersResponse
makeLenses ''BuildSuggestersResponse
makeLenses ''DeleteDomainResponse
makeLenses ''DefineIndexFieldResponse
