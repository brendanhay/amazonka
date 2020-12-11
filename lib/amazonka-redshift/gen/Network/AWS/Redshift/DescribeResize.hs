{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeResize
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the last resize operation for the specified cluster. If no resize operation has ever been initiated for the specified cluster, a @HTTP 404@ error is returned. If a resize operation was initiated and completed, the status of the resize remains as @SUCCEEDED@ until the next resize.
--
-- A resize operation can be requested using 'ModifyCluster' and specifying a different number or type of nodes for the cluster.
module Network.AWS.Redshift.DescribeResize
  ( -- * Creating a request
    DescribeResize (..),
    mkDescribeResize,

    -- ** Request lenses
    drClusterIdentifier,

    -- * Destructuring the response
    ResizeProgressMessage (..),
    mkResizeProgressMessage,

    -- ** Response lenses
    rpmImportTablesNotStarted,
    rpmStatus,
    rpmEstimatedTimeToCompletionInSeconds,
    rpmAvgResizeRateInMegaBytesPerSecond,
    rpmTargetNumberOfNodes,
    rpmTargetEncryptionType,
    rpmTargetNodeType,
    rpmImportTablesInProgress,
    rpmResizeType,
    rpmImportTablesCompleted,
    rpmProgressInMegaBytes,
    rpmDataTransferProgressPercent,
    rpmTotalResizeDataInMegaBytes,
    rpmTargetClusterType,
    rpmMessage,
    rpmElapsedTimeInSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeResize' smart constructor.
newtype DescribeResize = DescribeResize'
  { clusterIdentifier ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeResize' with the minimum fields required to make a request.
--
-- * 'clusterIdentifier' - The unique identifier of a cluster whose resize progress you are requesting. This parameter is case-sensitive.
--
-- By default, resize operations for all clusters defined for an AWS account are returned.
mkDescribeResize ::
  -- | 'clusterIdentifier'
  Lude.Text ->
  DescribeResize
mkDescribeResize pClusterIdentifier_ =
  DescribeResize' {clusterIdentifier = pClusterIdentifier_}

-- | The unique identifier of a cluster whose resize progress you are requesting. This parameter is case-sensitive.
--
-- By default, resize operations for all clusters defined for an AWS account are returned.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drClusterIdentifier :: Lens.Lens' DescribeResize Lude.Text
drClusterIdentifier = Lens.lens (clusterIdentifier :: DescribeResize -> Lude.Text) (\s a -> s {clusterIdentifier = a} :: DescribeResize)
{-# DEPRECATED drClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

instance Lude.AWSRequest DescribeResize where
  type Rs DescribeResize = ResizeProgressMessage
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "DescribeResizeResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders DescribeResize where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeResize where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeResize where
  toQuery DescribeResize' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeResize" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "ClusterIdentifier" Lude.=: clusterIdentifier
      ]
