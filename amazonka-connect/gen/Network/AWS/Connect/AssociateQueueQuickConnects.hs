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
-- Module      : Network.AWS.Connect.AssociateQueueQuickConnects
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Associates a set of quick connects with a queue.
module Network.AWS.Connect.AssociateQueueQuickConnects
  ( -- * Creating a Request
    AssociateQueueQuickConnects (..),
    newAssociateQueueQuickConnects,

    -- * Request Lenses
    associateQueueQuickConnects_instanceId,
    associateQueueQuickConnects_queueId,
    associateQueueQuickConnects_quickConnectIds,

    -- * Destructuring the Response
    AssociateQueueQuickConnectsResponse (..),
    newAssociateQueueQuickConnectsResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateQueueQuickConnects' smart constructor.
data AssociateQueueQuickConnects = AssociateQueueQuickConnects'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text,
    -- | The identifier for the queue.
    queueId :: Core.Text,
    -- | The quick connects to associate with this queue.
    quickConnectIds :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateQueueQuickConnects' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'associateQueueQuickConnects_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'queueId', 'associateQueueQuickConnects_queueId' - The identifier for the queue.
--
-- 'quickConnectIds', 'associateQueueQuickConnects_quickConnectIds' - The quick connects to associate with this queue.
newAssociateQueueQuickConnects ::
  -- | 'instanceId'
  Core.Text ->
  -- | 'queueId'
  Core.Text ->
  -- | 'quickConnectIds'
  Core.NonEmpty Core.Text ->
  AssociateQueueQuickConnects
newAssociateQueueQuickConnects
  pInstanceId_
  pQueueId_
  pQuickConnectIds_ =
    AssociateQueueQuickConnects'
      { instanceId =
          pInstanceId_,
        queueId = pQueueId_,
        quickConnectIds =
          Lens._Coerce Lens.# pQuickConnectIds_
      }

-- | The identifier of the Amazon Connect instance.
associateQueueQuickConnects_instanceId :: Lens.Lens' AssociateQueueQuickConnects Core.Text
associateQueueQuickConnects_instanceId = Lens.lens (\AssociateQueueQuickConnects' {instanceId} -> instanceId) (\s@AssociateQueueQuickConnects' {} a -> s {instanceId = a} :: AssociateQueueQuickConnects)

-- | The identifier for the queue.
associateQueueQuickConnects_queueId :: Lens.Lens' AssociateQueueQuickConnects Core.Text
associateQueueQuickConnects_queueId = Lens.lens (\AssociateQueueQuickConnects' {queueId} -> queueId) (\s@AssociateQueueQuickConnects' {} a -> s {queueId = a} :: AssociateQueueQuickConnects)

-- | The quick connects to associate with this queue.
associateQueueQuickConnects_quickConnectIds :: Lens.Lens' AssociateQueueQuickConnects (Core.NonEmpty Core.Text)
associateQueueQuickConnects_quickConnectIds = Lens.lens (\AssociateQueueQuickConnects' {quickConnectIds} -> quickConnectIds) (\s@AssociateQueueQuickConnects' {} a -> s {quickConnectIds = a} :: AssociateQueueQuickConnects) Core.. Lens._Coerce

instance Core.AWSRequest AssociateQueueQuickConnects where
  type
    AWSResponse AssociateQueueQuickConnects =
      AssociateQueueQuickConnectsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      AssociateQueueQuickConnectsResponse'

instance Core.Hashable AssociateQueueQuickConnects

instance Core.NFData AssociateQueueQuickConnects

instance Core.ToHeaders AssociateQueueQuickConnects where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AssociateQueueQuickConnects where
  toJSON AssociateQueueQuickConnects' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("QuickConnectIds" Core..= quickConnectIds)
          ]
      )

instance Core.ToPath AssociateQueueQuickConnects where
  toPath AssociateQueueQuickConnects' {..} =
    Core.mconcat
      [ "/queues/",
        Core.toBS instanceId,
        "/",
        Core.toBS queueId,
        "/associate-quick-connects"
      ]

instance Core.ToQuery AssociateQueueQuickConnects where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAssociateQueueQuickConnectsResponse' smart constructor.
data AssociateQueueQuickConnectsResponse = AssociateQueueQuickConnectsResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateQueueQuickConnectsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAssociateQueueQuickConnectsResponse ::
  AssociateQueueQuickConnectsResponse
newAssociateQueueQuickConnectsResponse =
  AssociateQueueQuickConnectsResponse'

instance
  Core.NFData
    AssociateQueueQuickConnectsResponse
