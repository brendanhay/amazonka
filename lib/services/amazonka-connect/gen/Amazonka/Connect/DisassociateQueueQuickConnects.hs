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
-- Module      : Amazonka.Connect.DisassociateQueueQuickConnects
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Disassociates a set of quick connects from a queue.
module Amazonka.Connect.DisassociateQueueQuickConnects
  ( -- * Creating a Request
    DisassociateQueueQuickConnects (..),
    newDisassociateQueueQuickConnects,

    -- * Request Lenses
    disassociateQueueQuickConnects_instanceId,
    disassociateQueueQuickConnects_queueId,
    disassociateQueueQuickConnects_quickConnectIds,

    -- * Destructuring the Response
    DisassociateQueueQuickConnectsResponse (..),
    newDisassociateQueueQuickConnectsResponse,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateQueueQuickConnects' smart constructor.
data DisassociateQueueQuickConnects = DisassociateQueueQuickConnects'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier for the queue.
    queueId :: Prelude.Text,
    -- | The quick connects to disassociate from the queue.
    quickConnectIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateQueueQuickConnects' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'disassociateQueueQuickConnects_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'queueId', 'disassociateQueueQuickConnects_queueId' - The identifier for the queue.
--
-- 'quickConnectIds', 'disassociateQueueQuickConnects_quickConnectIds' - The quick connects to disassociate from the queue.
newDisassociateQueueQuickConnects ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'queueId'
  Prelude.Text ->
  -- | 'quickConnectIds'
  Prelude.NonEmpty Prelude.Text ->
  DisassociateQueueQuickConnects
newDisassociateQueueQuickConnects
  pInstanceId_
  pQueueId_
  pQuickConnectIds_ =
    DisassociateQueueQuickConnects'
      { instanceId =
          pInstanceId_,
        queueId = pQueueId_,
        quickConnectIds =
          Lens.coerced Lens.# pQuickConnectIds_
      }

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
disassociateQueueQuickConnects_instanceId :: Lens.Lens' DisassociateQueueQuickConnects Prelude.Text
disassociateQueueQuickConnects_instanceId = Lens.lens (\DisassociateQueueQuickConnects' {instanceId} -> instanceId) (\s@DisassociateQueueQuickConnects' {} a -> s {instanceId = a} :: DisassociateQueueQuickConnects)

-- | The identifier for the queue.
disassociateQueueQuickConnects_queueId :: Lens.Lens' DisassociateQueueQuickConnects Prelude.Text
disassociateQueueQuickConnects_queueId = Lens.lens (\DisassociateQueueQuickConnects' {queueId} -> queueId) (\s@DisassociateQueueQuickConnects' {} a -> s {queueId = a} :: DisassociateQueueQuickConnects)

-- | The quick connects to disassociate from the queue.
disassociateQueueQuickConnects_quickConnectIds :: Lens.Lens' DisassociateQueueQuickConnects (Prelude.NonEmpty Prelude.Text)
disassociateQueueQuickConnects_quickConnectIds = Lens.lens (\DisassociateQueueQuickConnects' {quickConnectIds} -> quickConnectIds) (\s@DisassociateQueueQuickConnects' {} a -> s {quickConnectIds = a} :: DisassociateQueueQuickConnects) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    DisassociateQueueQuickConnects
  where
  type
    AWSResponse DisassociateQueueQuickConnects =
      DisassociateQueueQuickConnectsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DisassociateQueueQuickConnectsResponse'

instance
  Prelude.Hashable
    DisassociateQueueQuickConnects
  where
  hashWithSalt
    _salt
    DisassociateQueueQuickConnects' {..} =
      _salt `Prelude.hashWithSalt` instanceId
        `Prelude.hashWithSalt` queueId
        `Prelude.hashWithSalt` quickConnectIds

instance
  Prelude.NFData
    DisassociateQueueQuickConnects
  where
  rnf DisassociateQueueQuickConnects' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf queueId
      `Prelude.seq` Prelude.rnf quickConnectIds

instance
  Core.ToHeaders
    DisassociateQueueQuickConnects
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DisassociateQueueQuickConnects where
  toJSON DisassociateQueueQuickConnects' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("QuickConnectIds" Core..= quickConnectIds)
          ]
      )

instance Core.ToPath DisassociateQueueQuickConnects where
  toPath DisassociateQueueQuickConnects' {..} =
    Prelude.mconcat
      [ "/queues/",
        Core.toBS instanceId,
        "/",
        Core.toBS queueId,
        "/disassociate-quick-connects"
      ]

instance Core.ToQuery DisassociateQueueQuickConnects where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateQueueQuickConnectsResponse' smart constructor.
data DisassociateQueueQuickConnectsResponse = DisassociateQueueQuickConnectsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateQueueQuickConnectsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateQueueQuickConnectsResponse ::
  DisassociateQueueQuickConnectsResponse
newDisassociateQueueQuickConnectsResponse =
  DisassociateQueueQuickConnectsResponse'

instance
  Prelude.NFData
    DisassociateQueueQuickConnectsResponse
  where
  rnf _ = ()
