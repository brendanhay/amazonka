{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCancelResize' smart constructor.
data CancelResize = CancelResize'
  { -- | The unique identifier for the cluster that you want to cancel a resize
    -- operation for.
    clusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  CancelResize
newCancelResize pClusterIdentifier_ =
  CancelResize'
    { clusterIdentifier =
        pClusterIdentifier_
    }

-- | The unique identifier for the cluster that you want to cancel a resize
-- operation for.
cancelResize_clusterIdentifier :: Lens.Lens' CancelResize Prelude.Text
cancelResize_clusterIdentifier = Lens.lens (\CancelResize' {clusterIdentifier} -> clusterIdentifier) (\s@CancelResize' {} a -> s {clusterIdentifier = a} :: CancelResize)

instance Prelude.AWSRequest CancelResize where
  type Rs CancelResize = ResizeProgressMessage
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CancelResizeResult"
      (\s h x -> Prelude.parseXML x)

instance Prelude.Hashable CancelResize

instance Prelude.NFData CancelResize

instance Prelude.ToHeaders CancelResize where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CancelResize where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CancelResize where
  toQuery CancelResize' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("CancelResize" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2012-12-01" :: Prelude.ByteString),
        "ClusterIdentifier" Prelude.=: clusterIdentifier
      ]
