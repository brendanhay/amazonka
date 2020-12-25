{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    Types.ResizeProgressMessage (..),
    Types.mkResizeProgressMessage,

    -- ** Response lenses
    Types.rpmAvgResizeRateInMegaBytesPerSecond,
    Types.rpmDataTransferProgressPercent,
    Types.rpmElapsedTimeInSeconds,
    Types.rpmEstimatedTimeToCompletionInSeconds,
    Types.rpmImportTablesCompleted,
    Types.rpmImportTablesInProgress,
    Types.rpmImportTablesNotStarted,
    Types.rpmMessage,
    Types.rpmProgressInMegaBytes,
    Types.rpmResizeType,
    Types.rpmStatus,
    Types.rpmTargetClusterType,
    Types.rpmTargetEncryptionType,
    Types.rpmTargetNodeType,
    Types.rpmTargetNumberOfNodes,
    Types.rpmTotalResizeDataInMegaBytes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCancelResize' smart constructor.
newtype CancelResize = CancelResize'
  { -- | The unique identifier for the cluster that you want to cancel a resize operation for.
    clusterIdentifier :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelResize' value with any optional fields omitted.
mkCancelResize ::
  -- | 'clusterIdentifier'
  Types.String ->
  CancelResize
mkCancelResize clusterIdentifier = CancelResize' {clusterIdentifier}

-- | The unique identifier for the cluster that you want to cancel a resize operation for.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crClusterIdentifier :: Lens.Lens' CancelResize Types.String
crClusterIdentifier = Lens.field @"clusterIdentifier"
{-# DEPRECATED crClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

instance Core.AWSRequest CancelResize where
  type Rs CancelResize = Types.ResizeProgressMessage
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CancelResize")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ClusterIdentifier" clusterIdentifier)
            )
      }
  response =
    Response.receiveXMLWrapper
      "CancelResizeResult"
      (\s h x -> Core.parseXML x)
