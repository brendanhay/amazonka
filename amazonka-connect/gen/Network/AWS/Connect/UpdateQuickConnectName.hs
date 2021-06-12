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
-- Module      : Network.AWS.Connect.UpdateQuickConnectName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Updates the name and description of a quick connect. The request accepts
-- the following data in JSON format. At least @Name@ or @Description@ must
-- be provided.
module Network.AWS.Connect.UpdateQuickConnectName
  ( -- * Creating a Request
    UpdateQuickConnectName (..),
    newUpdateQuickConnectName,

    -- * Request Lenses
    updateQuickConnectName_name,
    updateQuickConnectName_description,
    updateQuickConnectName_instanceId,
    updateQuickConnectName_quickConnectId,

    -- * Destructuring the Response
    UpdateQuickConnectNameResponse (..),
    newUpdateQuickConnectNameResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateQuickConnectName' smart constructor.
data UpdateQuickConnectName = UpdateQuickConnectName'
  { -- | The name of the quick connect.
    name :: Core.Maybe Core.Text,
    -- | The description of the quick connect.
    description :: Core.Maybe Core.Text,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text,
    -- | The identifier for the quick connect.
    quickConnectId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateQuickConnectName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateQuickConnectName_name' - The name of the quick connect.
--
-- 'description', 'updateQuickConnectName_description' - The description of the quick connect.
--
-- 'instanceId', 'updateQuickConnectName_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'quickConnectId', 'updateQuickConnectName_quickConnectId' - The identifier for the quick connect.
newUpdateQuickConnectName ::
  -- | 'instanceId'
  Core.Text ->
  -- | 'quickConnectId'
  Core.Text ->
  UpdateQuickConnectName
newUpdateQuickConnectName
  pInstanceId_
  pQuickConnectId_ =
    UpdateQuickConnectName'
      { name = Core.Nothing,
        description = Core.Nothing,
        instanceId = pInstanceId_,
        quickConnectId = pQuickConnectId_
      }

-- | The name of the quick connect.
updateQuickConnectName_name :: Lens.Lens' UpdateQuickConnectName (Core.Maybe Core.Text)
updateQuickConnectName_name = Lens.lens (\UpdateQuickConnectName' {name} -> name) (\s@UpdateQuickConnectName' {} a -> s {name = a} :: UpdateQuickConnectName)

-- | The description of the quick connect.
updateQuickConnectName_description :: Lens.Lens' UpdateQuickConnectName (Core.Maybe Core.Text)
updateQuickConnectName_description = Lens.lens (\UpdateQuickConnectName' {description} -> description) (\s@UpdateQuickConnectName' {} a -> s {description = a} :: UpdateQuickConnectName)

-- | The identifier of the Amazon Connect instance.
updateQuickConnectName_instanceId :: Lens.Lens' UpdateQuickConnectName Core.Text
updateQuickConnectName_instanceId = Lens.lens (\UpdateQuickConnectName' {instanceId} -> instanceId) (\s@UpdateQuickConnectName' {} a -> s {instanceId = a} :: UpdateQuickConnectName)

-- | The identifier for the quick connect.
updateQuickConnectName_quickConnectId :: Lens.Lens' UpdateQuickConnectName Core.Text
updateQuickConnectName_quickConnectId = Lens.lens (\UpdateQuickConnectName' {quickConnectId} -> quickConnectId) (\s@UpdateQuickConnectName' {} a -> s {quickConnectId = a} :: UpdateQuickConnectName)

instance Core.AWSRequest UpdateQuickConnectName where
  type
    AWSResponse UpdateQuickConnectName =
      UpdateQuickConnectNameResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      UpdateQuickConnectNameResponse'

instance Core.Hashable UpdateQuickConnectName

instance Core.NFData UpdateQuickConnectName

instance Core.ToHeaders UpdateQuickConnectName where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateQuickConnectName where
  toJSON UpdateQuickConnectName' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Name" Core..=) Core.<$> name,
            ("Description" Core..=) Core.<$> description
          ]
      )

instance Core.ToPath UpdateQuickConnectName where
  toPath UpdateQuickConnectName' {..} =
    Core.mconcat
      [ "/quick-connects/",
        Core.toBS instanceId,
        "/",
        Core.toBS quickConnectId,
        "/name"
      ]

instance Core.ToQuery UpdateQuickConnectName where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateQuickConnectNameResponse' smart constructor.
data UpdateQuickConnectNameResponse = UpdateQuickConnectNameResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateQuickConnectNameResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateQuickConnectNameResponse ::
  UpdateQuickConnectNameResponse
newUpdateQuickConnectNameResponse =
  UpdateQuickConnectNameResponse'

instance Core.NFData UpdateQuickConnectNameResponse
