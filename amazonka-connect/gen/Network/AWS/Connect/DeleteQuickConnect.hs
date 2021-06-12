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
-- Module      : Network.AWS.Connect.DeleteQuickConnect
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Deletes a quick connect.
module Network.AWS.Connect.DeleteQuickConnect
  ( -- * Creating a Request
    DeleteQuickConnect (..),
    newDeleteQuickConnect,

    -- * Request Lenses
    deleteQuickConnect_instanceId,
    deleteQuickConnect_quickConnectId,

    -- * Destructuring the Response
    DeleteQuickConnectResponse (..),
    newDeleteQuickConnectResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteQuickConnect' smart constructor.
data DeleteQuickConnect = DeleteQuickConnect'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text,
    -- | The identifier for the quick connect.
    quickConnectId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteQuickConnect' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'deleteQuickConnect_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'quickConnectId', 'deleteQuickConnect_quickConnectId' - The identifier for the quick connect.
newDeleteQuickConnect ::
  -- | 'instanceId'
  Core.Text ->
  -- | 'quickConnectId'
  Core.Text ->
  DeleteQuickConnect
newDeleteQuickConnect pInstanceId_ pQuickConnectId_ =
  DeleteQuickConnect'
    { instanceId = pInstanceId_,
      quickConnectId = pQuickConnectId_
    }

-- | The identifier of the Amazon Connect instance.
deleteQuickConnect_instanceId :: Lens.Lens' DeleteQuickConnect Core.Text
deleteQuickConnect_instanceId = Lens.lens (\DeleteQuickConnect' {instanceId} -> instanceId) (\s@DeleteQuickConnect' {} a -> s {instanceId = a} :: DeleteQuickConnect)

-- | The identifier for the quick connect.
deleteQuickConnect_quickConnectId :: Lens.Lens' DeleteQuickConnect Core.Text
deleteQuickConnect_quickConnectId = Lens.lens (\DeleteQuickConnect' {quickConnectId} -> quickConnectId) (\s@DeleteQuickConnect' {} a -> s {quickConnectId = a} :: DeleteQuickConnect)

instance Core.AWSRequest DeleteQuickConnect where
  type
    AWSResponse DeleteQuickConnect =
      DeleteQuickConnectResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteQuickConnectResponse'

instance Core.Hashable DeleteQuickConnect

instance Core.NFData DeleteQuickConnect

instance Core.ToHeaders DeleteQuickConnect where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteQuickConnect where
  toPath DeleteQuickConnect' {..} =
    Core.mconcat
      [ "/quick-connects/",
        Core.toBS instanceId,
        "/",
        Core.toBS quickConnectId
      ]

instance Core.ToQuery DeleteQuickConnect where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteQuickConnectResponse' smart constructor.
data DeleteQuickConnectResponse = DeleteQuickConnectResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteQuickConnectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteQuickConnectResponse ::
  DeleteQuickConnectResponse
newDeleteQuickConnectResponse =
  DeleteQuickConnectResponse'

instance Core.NFData DeleteQuickConnectResponse
