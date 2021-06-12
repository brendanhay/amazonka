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
-- Module      : Network.AWS.Connect.DisassociateQueueQuickConnects
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Disassociates a set of quick connects from a queue.
module Network.AWS.Connect.DisassociateQueueQuickConnects
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

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateQueueQuickConnects' smart constructor.
data DisassociateQueueQuickConnects = DisassociateQueueQuickConnects'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text,
    -- | The identifier for the queue.
    queueId :: Core.Text,
    -- | The quick connects to disassociate from the queue.
    quickConnectIds :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateQueueQuickConnects' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'disassociateQueueQuickConnects_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'queueId', 'disassociateQueueQuickConnects_queueId' - The identifier for the queue.
--
-- 'quickConnectIds', 'disassociateQueueQuickConnects_quickConnectIds' - The quick connects to disassociate from the queue.
newDisassociateQueueQuickConnects ::
  -- | 'instanceId'
  Core.Text ->
  -- | 'queueId'
  Core.Text ->
  -- | 'quickConnectIds'
  Core.NonEmpty Core.Text ->
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
          Lens._Coerce Lens.# pQuickConnectIds_
      }

-- | The identifier of the Amazon Connect instance.
disassociateQueueQuickConnects_instanceId :: Lens.Lens' DisassociateQueueQuickConnects Core.Text
disassociateQueueQuickConnects_instanceId = Lens.lens (\DisassociateQueueQuickConnects' {instanceId} -> instanceId) (\s@DisassociateQueueQuickConnects' {} a -> s {instanceId = a} :: DisassociateQueueQuickConnects)

-- | The identifier for the queue.
disassociateQueueQuickConnects_queueId :: Lens.Lens' DisassociateQueueQuickConnects Core.Text
disassociateQueueQuickConnects_queueId = Lens.lens (\DisassociateQueueQuickConnects' {queueId} -> queueId) (\s@DisassociateQueueQuickConnects' {} a -> s {queueId = a} :: DisassociateQueueQuickConnects)

-- | The quick connects to disassociate from the queue.
disassociateQueueQuickConnects_quickConnectIds :: Lens.Lens' DisassociateQueueQuickConnects (Core.NonEmpty Core.Text)
disassociateQueueQuickConnects_quickConnectIds = Lens.lens (\DisassociateQueueQuickConnects' {quickConnectIds} -> quickConnectIds) (\s@DisassociateQueueQuickConnects' {} a -> s {quickConnectIds = a} :: DisassociateQueueQuickConnects) Core.. Lens._Coerce

instance
  Core.AWSRequest
    DisassociateQueueQuickConnects
  where
  type
    AWSResponse DisassociateQueueQuickConnects =
      DisassociateQueueQuickConnectsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DisassociateQueueQuickConnectsResponse'

instance Core.Hashable DisassociateQueueQuickConnects

instance Core.NFData DisassociateQueueQuickConnects

instance
  Core.ToHeaders
    DisassociateQueueQuickConnects
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DisassociateQueueQuickConnects where
  toJSON DisassociateQueueQuickConnects' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("QuickConnectIds" Core..= quickConnectIds)
          ]
      )

instance Core.ToPath DisassociateQueueQuickConnects where
  toPath DisassociateQueueQuickConnects' {..} =
    Core.mconcat
      [ "/queues/",
        Core.toBS instanceId,
        "/",
        Core.toBS queueId,
        "/disassociate-quick-connects"
      ]

instance Core.ToQuery DisassociateQueueQuickConnects where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDisassociateQueueQuickConnectsResponse' smart constructor.
data DisassociateQueueQuickConnectsResponse = DisassociateQueueQuickConnectsResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateQueueQuickConnectsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateQueueQuickConnectsResponse ::
  DisassociateQueueQuickConnectsResponse
newDisassociateQueueQuickConnectsResponse =
  DisassociateQueueQuickConnectsResponse'

instance
  Core.NFData
    DisassociateQueueQuickConnectsResponse
