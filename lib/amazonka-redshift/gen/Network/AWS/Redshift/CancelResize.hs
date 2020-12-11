{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CancelResize
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a resize operation for a cluster.
module Network.AWS.Redshift.CancelResize
  ( -- * Creating a request
    CancelResize (..),
    mkCancelResize,

    -- ** Request lenses
    crClusterIdentifier,

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

-- | /See:/ 'mkCancelResize' smart constructor.
newtype CancelResize = CancelResize'
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

-- | Creates a value of 'CancelResize' with the minimum fields required to make a request.
--
-- * 'clusterIdentifier' - The unique identifier for the cluster that you want to cancel a resize operation for.
mkCancelResize ::
  -- | 'clusterIdentifier'
  Lude.Text ->
  CancelResize
mkCancelResize pClusterIdentifier_ =
  CancelResize' {clusterIdentifier = pClusterIdentifier_}

-- | The unique identifier for the cluster that you want to cancel a resize operation for.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crClusterIdentifier :: Lens.Lens' CancelResize Lude.Text
crClusterIdentifier = Lens.lens (clusterIdentifier :: CancelResize -> Lude.Text) (\s a -> s {clusterIdentifier = a} :: CancelResize)
{-# DEPRECATED crClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

instance Lude.AWSRequest CancelResize where
  type Rs CancelResize = ResizeProgressMessage
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "CancelResizeResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders CancelResize where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CancelResize where
  toPath = Lude.const "/"

instance Lude.ToQuery CancelResize where
  toQuery CancelResize' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CancelResize" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "ClusterIdentifier" Lude.=: clusterIdentifier
      ]
