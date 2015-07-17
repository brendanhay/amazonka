{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Amazon Elastic MapReduce (Amazon EMR) is a web service that makes it
-- easy to process large amounts of data efficiently. Amazon EMR uses
-- Hadoop processing combined with several AWS products to do tasks such as
-- web indexing, data mining, log file analysis, machine learning,
-- scientific simulation, and data warehousing.
module Network.AWS.EMR
    ( module Export
    ) where

import           Network.AWS.EMR.AddInstanceGroups        as Export
import           Network.AWS.EMR.AddJobFlowSteps          as Export
import           Network.AWS.EMR.AddTags                  as Export
import           Network.AWS.EMR.DescribeCluster          as Export
import           Network.AWS.EMR.DescribeStep             as Export
import           Network.AWS.EMR.ListBootstrapActions     as Export
import           Network.AWS.EMR.ListClusters             as Export
import           Network.AWS.EMR.ListInstanceGroups       as Export
import           Network.AWS.EMR.ListInstances            as Export
import           Network.AWS.EMR.ListSteps                as Export
import           Network.AWS.EMR.ModifyInstanceGroups     as Export
import           Network.AWS.EMR.RemoveTags               as Export
import           Network.AWS.EMR.RunJobFlow               as Export
import           Network.AWS.EMR.SetTerminationProtection as Export
import           Network.AWS.EMR.SetVisibleToAllUsers     as Export
import           Network.AWS.EMR.TerminateJobFlows        as Export
import           Network.AWS.EMR.Types                    as Export
import           Network.AWS.EMR.Types.Product            as Export
import           Network.AWS.EMR.Types.Sum                as Export
import           Network.AWS.EMR.Waiters                  as Export
