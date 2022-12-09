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
-- Module      : Amazonka.Redshift.CancelResize
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a resize operation for a cluster.
module Amazonka.Redshift.CancelResize
  ( -- * Creating a Request
    CancelResize (..),
    newCancelResize,

    -- * Request Lenses
    cancelResize_clusterIdentifier,

    -- * Destructuring the Response
    ResizeProgressMessage (..),
    newResizeProgressMessage,

    -- * Response Lenses
    resizeProgressMessage_avgResizeRateInMegaBytesPerSecond,
    resizeProgressMessage_dataTransferProgressPercent,
    resizeProgressMessage_elapsedTimeInSeconds,
    resizeProgressMessage_estimatedTimeToCompletionInSeconds,
    resizeProgressMessage_importTablesCompleted,
    resizeProgressMessage_importTablesInProgress,
    resizeProgressMessage_importTablesNotStarted,
    resizeProgressMessage_message,
    resizeProgressMessage_progressInMegaBytes,
    resizeProgressMessage_resizeType,
    resizeProgressMessage_status,
    resizeProgressMessage_targetClusterType,
    resizeProgressMessage_targetEncryptionType,
    resizeProgressMessage_targetNodeType,
    resizeProgressMessage_targetNumberOfNodes,
    resizeProgressMessage_totalResizeDataInMegaBytes,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelResize' smart constructor.
data CancelResize = CancelResize'
  { -- | The unique identifier for the cluster that you want to cancel a resize
    -- operation for.
    clusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest CancelResize where
  type AWSResponse CancelResize = ResizeProgressMessage
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CancelResizeResult"
      (\s h x -> Data.parseXML x)

instance Prelude.Hashable CancelResize where
  hashWithSalt _salt CancelResize' {..} =
    _salt `Prelude.hashWithSalt` clusterIdentifier

instance Prelude.NFData CancelResize where
  rnf CancelResize' {..} = Prelude.rnf clusterIdentifier

instance Data.ToHeaders CancelResize where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CancelResize where
  toPath = Prelude.const "/"

instance Data.ToQuery CancelResize where
  toQuery CancelResize' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CancelResize" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "ClusterIdentifier" Data.=: clusterIdentifier
      ]
