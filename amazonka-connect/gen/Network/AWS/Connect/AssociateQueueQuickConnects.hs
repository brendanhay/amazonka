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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateQueueQuickConnects' smart constructor.
data AssociateQueueQuickConnects = AssociateQueueQuickConnects'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text,
    -- | The identifier for the queue.
    queueId :: Prelude.Text,
    -- | The quick connects to associate with this queue.
    quickConnectIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'queueId'
  Prelude.Text ->
  -- | 'quickConnectIds'
  Prelude.NonEmpty Prelude.Text ->
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
          Prelude._Coerce Lens.# pQuickConnectIds_
      }

-- | The identifier of the Amazon Connect instance.
associateQueueQuickConnects_instanceId :: Lens.Lens' AssociateQueueQuickConnects Prelude.Text
associateQueueQuickConnects_instanceId = Lens.lens (\AssociateQueueQuickConnects' {instanceId} -> instanceId) (\s@AssociateQueueQuickConnects' {} a -> s {instanceId = a} :: AssociateQueueQuickConnects)

-- | The identifier for the queue.
associateQueueQuickConnects_queueId :: Lens.Lens' AssociateQueueQuickConnects Prelude.Text
associateQueueQuickConnects_queueId = Lens.lens (\AssociateQueueQuickConnects' {queueId} -> queueId) (\s@AssociateQueueQuickConnects' {} a -> s {queueId = a} :: AssociateQueueQuickConnects)

-- | The quick connects to associate with this queue.
associateQueueQuickConnects_quickConnectIds :: Lens.Lens' AssociateQueueQuickConnects (Prelude.NonEmpty Prelude.Text)
associateQueueQuickConnects_quickConnectIds = Lens.lens (\AssociateQueueQuickConnects' {quickConnectIds} -> quickConnectIds) (\s@AssociateQueueQuickConnects' {} a -> s {quickConnectIds = a} :: AssociateQueueQuickConnects) Prelude.. Prelude._Coerce

instance
  Prelude.AWSRequest
    AssociateQueueQuickConnects
  where
  type
    Rs AssociateQueueQuickConnects =
      AssociateQueueQuickConnectsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      AssociateQueueQuickConnectsResponse'

instance Prelude.Hashable AssociateQueueQuickConnects

instance Prelude.NFData AssociateQueueQuickConnects

instance
  Prelude.ToHeaders
    AssociateQueueQuickConnects
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AssociateQueueQuickConnects where
  toJSON AssociateQueueQuickConnects' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("QuickConnectIds" Prelude..= quickConnectIds)
          ]
      )

instance Prelude.ToPath AssociateQueueQuickConnects where
  toPath AssociateQueueQuickConnects' {..} =
    Prelude.mconcat
      [ "/queues/",
        Prelude.toBS instanceId,
        "/",
        Prelude.toBS queueId,
        "/associate-quick-connects"
      ]

instance Prelude.ToQuery AssociateQueueQuickConnects where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateQueueQuickConnectsResponse' smart constructor.
data AssociateQueueQuickConnectsResponse = AssociateQueueQuickConnectsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssociateQueueQuickConnectsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAssociateQueueQuickConnectsResponse ::
  AssociateQueueQuickConnectsResponse
newAssociateQueueQuickConnectsResponse =
  AssociateQueueQuickConnectsResponse'

instance
  Prelude.NFData
    AssociateQueueQuickConnectsResponse
