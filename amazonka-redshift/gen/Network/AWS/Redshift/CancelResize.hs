{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CancelResize
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a resize operation for a cluster.
module Network.AWS.Redshift.CancelResize
  ( -- * Creating a Request
    CancelResize (..),
    newCancelResize,

    -- * Request Lenses
    cancelResize_clusterIdentifier,

    -- * Destructuring the Response
    ResizeProgressMessage (..),
    newResizeProgressMessage,

    -- * Response Lenses
    resizeProgressMessage_status,
    resizeProgressMessage_estimatedTimeToCompletionInSeconds,
    resizeProgressMessage_importTablesNotStarted,
    resizeProgressMessage_targetNodeType,
    resizeProgressMessage_message,
    resizeProgressMessage_targetClusterType,
    resizeProgressMessage_avgResizeRateInMegaBytesPerSecond,
    resizeProgressMessage_targetEncryptionType,
    resizeProgressMessage_elapsedTimeInSeconds,
    resizeProgressMessage_targetNumberOfNodes,
    resizeProgressMessage_totalResizeDataInMegaBytes,
    resizeProgressMessage_progressInMegaBytes,
    resizeProgressMessage_dataTransferProgressPercent,
    resizeProgressMessage_importTablesCompleted,
    resizeProgressMessage_importTablesInProgress,
    resizeProgressMessage_resizeType,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCancelResize' smart constructor.
data CancelResize = CancelResize'
  { -- | The unique identifier for the cluster that you want to cancel a resize
    -- operation for.
    clusterIdentifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CancelResize' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterIdentifier', 'cancelResize_clusterIdentifier' - The unique identifier for the cluster that you want to cancel a resize
-- operation for.
newCancelResize ::
  -- | 'clusterIdentifier'
  Core.Text ->
  CancelResize
newCancelResize pClusterIdentifier_ =
  CancelResize'
    { clusterIdentifier =
        pClusterIdentifier_
    }

-- | The unique identifier for the cluster that you want to cancel a resize
-- operation for.
cancelResize_clusterIdentifier :: Lens.Lens' CancelResize Core.Text
cancelResize_clusterIdentifier = Lens.lens (\CancelResize' {clusterIdentifier} -> clusterIdentifier) (\s@CancelResize' {} a -> s {clusterIdentifier = a} :: CancelResize)

instance Core.AWSRequest CancelResize where
  type AWSResponse CancelResize = ResizeProgressMessage
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CancelResizeResult"
      (\s h x -> Core.parseXML x)

instance Core.Hashable CancelResize

instance Core.NFData CancelResize

instance Core.ToHeaders CancelResize where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CancelResize where
  toPath = Core.const "/"

instance Core.ToQuery CancelResize where
  toQuery CancelResize' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CancelResize" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "ClusterIdentifier" Core.=: clusterIdentifier
      ]
