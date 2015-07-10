{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Amazon CloudSearch Configuration Service
--
-- You use the Amazon CloudSearch configuration service to create,
-- configure, and manage search domains. Configuration service requests are
-- submitted using the AWS Query protocol. AWS Query requests are HTTP or
-- HTTPS requests submitted via HTTP GET or POST with a query parameter
-- named Action.
--
-- The endpoint for configuration service requests is region-specific:
-- cloudsearch./region/.amazonaws.com. For example,
-- cloudsearch.us-east-1.amazonaws.com. For a current list of supported
-- regions and endpoints, see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html#cloudsearch_region Regions and Endpoints>.
module Network.AWS.CloudSearch
    ( module Export
    ) where

import           Network.AWS.CloudSearch.BuildSuggesters               as Export
import           Network.AWS.CloudSearch.CreateDomain                  as Export
import           Network.AWS.CloudSearch.DefineAnalysisScheme          as Export
import           Network.AWS.CloudSearch.DefineExpression              as Export
import           Network.AWS.CloudSearch.DefineIndexField              as Export
import           Network.AWS.CloudSearch.DefineSuggester               as Export
import           Network.AWS.CloudSearch.DeleteAnalysisScheme          as Export
import           Network.AWS.CloudSearch.DeleteDomain                  as Export
import           Network.AWS.CloudSearch.DeleteExpression              as Export
import           Network.AWS.CloudSearch.DeleteIndexField              as Export
import           Network.AWS.CloudSearch.DeleteSuggester               as Export
import           Network.AWS.CloudSearch.DescribeAnalysisSchemes       as Export
import           Network.AWS.CloudSearch.DescribeAvailabilityOptions   as Export
import           Network.AWS.CloudSearch.DescribeDomains               as Export
import           Network.AWS.CloudSearch.DescribeExpressions           as Export
import           Network.AWS.CloudSearch.DescribeIndexFields           as Export
import           Network.AWS.CloudSearch.DescribeScalingParameters     as Export
import           Network.AWS.CloudSearch.DescribeServiceAccessPolicies as Export
import           Network.AWS.CloudSearch.DescribeSuggesters            as Export
import           Network.AWS.CloudSearch.IndexDocuments                as Export
import           Network.AWS.CloudSearch.ListDomainNames               as Export
import           Network.AWS.CloudSearch.Types                         as Export
import           Network.AWS.CloudSearch.UpdateAvailabilityOptions     as Export
import           Network.AWS.CloudSearch.UpdateScalingParameters       as Export
import           Network.AWS.CloudSearch.UpdateServiceAccessPolicies   as Export
import           Network.AWS.CloudSearch.Waiters                       as Export
