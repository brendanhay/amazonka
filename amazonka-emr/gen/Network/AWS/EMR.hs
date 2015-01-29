-- Module      : Network.AWS.EMR
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Amazon Elastic MapReduce (Amazon EMR) is a web service that makes it easy to
-- process large amounts of data efficiently. Amazon EMR uses Hadoop processing
-- combined with several AWS products to do such tasks as web indexing, data
-- mining, log file analysis, machine learning, scientific simulation, and data
-- warehousing.
module Network.AWS.EMR
    ( module Network.AWS.EMR.AddInstanceGroups
    , module Network.AWS.EMR.AddJobFlowSteps
    , module Network.AWS.EMR.AddTags
    , module Network.AWS.EMR.DescribeCluster
    , module Network.AWS.EMR.DescribeJobFlows
    , module Network.AWS.EMR.DescribeStep
    , module Network.AWS.EMR.ListBootstrapActions
    , module Network.AWS.EMR.ListClusters
    , module Network.AWS.EMR.ListInstanceGroups
    , module Network.AWS.EMR.ListInstances
    , module Network.AWS.EMR.ListSteps
    , module Network.AWS.EMR.ModifyInstanceGroups
    , module Network.AWS.EMR.RemoveTags
    , module Network.AWS.EMR.RunJobFlow
    , module Network.AWS.EMR.SetTerminationProtection
    , module Network.AWS.EMR.SetVisibleToAllUsers
    , module Network.AWS.EMR.TerminateJobFlows
    , module Network.AWS.EMR.Types
    , module Network.AWS.EMR.Waiters
    ) where

import Network.AWS.EMR.AddInstanceGroups
import Network.AWS.EMR.AddJobFlowSteps
import Network.AWS.EMR.AddTags
import Network.AWS.EMR.DescribeCluster
import Network.AWS.EMR.DescribeJobFlows
import Network.AWS.EMR.DescribeStep
import Network.AWS.EMR.ListBootstrapActions
import Network.AWS.EMR.ListClusters
import Network.AWS.EMR.ListInstanceGroups
import Network.AWS.EMR.ListInstances
import Network.AWS.EMR.ListSteps
import Network.AWS.EMR.ModifyInstanceGroups
import Network.AWS.EMR.RemoveTags
import Network.AWS.EMR.RunJobFlow
import Network.AWS.EMR.SetTerminationProtection
import Network.AWS.EMR.SetVisibleToAllUsers
import Network.AWS.EMR.TerminateJobFlows
import Network.AWS.EMR.Types
import Network.AWS.EMR.Waiters
