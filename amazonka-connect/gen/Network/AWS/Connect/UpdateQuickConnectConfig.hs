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
-- Module      : Network.AWS.Connect.UpdateQuickConnectConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Updates the configuration settings for the specified quick connect.
module Network.AWS.Connect.UpdateQuickConnectConfig
  ( -- * Creating a Request
    UpdateQuickConnectConfig (..),
    newUpdateQuickConnectConfig,

    -- * Request Lenses
    updateQuickConnectConfig_instanceId,
    updateQuickConnectConfig_quickConnectId,
    updateQuickConnectConfig_quickConnectConfig,

    -- * Destructuring the Response
    UpdateQuickConnectConfigResponse (..),
    newUpdateQuickConnectConfigResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateQuickConnectConfig' smart constructor.
data UpdateQuickConnectConfig = UpdateQuickConnectConfig'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text,
    -- | The identifier for the quick connect.
    quickConnectId :: Core.Text,
    -- | Information about the configuration settings for the quick connect.
    quickConnectConfig :: QuickConnectConfig
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateQuickConnectConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'updateQuickConnectConfig_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'quickConnectId', 'updateQuickConnectConfig_quickConnectId' - The identifier for the quick connect.
--
-- 'quickConnectConfig', 'updateQuickConnectConfig_quickConnectConfig' - Information about the configuration settings for the quick connect.
newUpdateQuickConnectConfig ::
  -- | 'instanceId'
  Core.Text ->
  -- | 'quickConnectId'
  Core.Text ->
  -- | 'quickConnectConfig'
  QuickConnectConfig ->
  UpdateQuickConnectConfig
newUpdateQuickConnectConfig
  pInstanceId_
  pQuickConnectId_
  pQuickConnectConfig_ =
    UpdateQuickConnectConfig'
      { instanceId =
          pInstanceId_,
        quickConnectId = pQuickConnectId_,
        quickConnectConfig = pQuickConnectConfig_
      }

-- | The identifier of the Amazon Connect instance.
updateQuickConnectConfig_instanceId :: Lens.Lens' UpdateQuickConnectConfig Core.Text
updateQuickConnectConfig_instanceId = Lens.lens (\UpdateQuickConnectConfig' {instanceId} -> instanceId) (\s@UpdateQuickConnectConfig' {} a -> s {instanceId = a} :: UpdateQuickConnectConfig)

-- | The identifier for the quick connect.
updateQuickConnectConfig_quickConnectId :: Lens.Lens' UpdateQuickConnectConfig Core.Text
updateQuickConnectConfig_quickConnectId = Lens.lens (\UpdateQuickConnectConfig' {quickConnectId} -> quickConnectId) (\s@UpdateQuickConnectConfig' {} a -> s {quickConnectId = a} :: UpdateQuickConnectConfig)

-- | Information about the configuration settings for the quick connect.
updateQuickConnectConfig_quickConnectConfig :: Lens.Lens' UpdateQuickConnectConfig QuickConnectConfig
updateQuickConnectConfig_quickConnectConfig = Lens.lens (\UpdateQuickConnectConfig' {quickConnectConfig} -> quickConnectConfig) (\s@UpdateQuickConnectConfig' {} a -> s {quickConnectConfig = a} :: UpdateQuickConnectConfig)

instance Core.AWSRequest UpdateQuickConnectConfig where
  type
    AWSResponse UpdateQuickConnectConfig =
      UpdateQuickConnectConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      UpdateQuickConnectConfigResponse'

instance Core.Hashable UpdateQuickConnectConfig

instance Core.NFData UpdateQuickConnectConfig

instance Core.ToHeaders UpdateQuickConnectConfig where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateQuickConnectConfig where
  toJSON UpdateQuickConnectConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("QuickConnectConfig" Core..= quickConnectConfig)
          ]
      )

instance Core.ToPath UpdateQuickConnectConfig where
  toPath UpdateQuickConnectConfig' {..} =
    Core.mconcat
      [ "/quick-connects/",
        Core.toBS instanceId,
        "/",
        Core.toBS quickConnectId,
        "/config"
      ]

instance Core.ToQuery UpdateQuickConnectConfig where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateQuickConnectConfigResponse' smart constructor.
data UpdateQuickConnectConfigResponse = UpdateQuickConnectConfigResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateQuickConnectConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateQuickConnectConfigResponse ::
  UpdateQuickConnectConfigResponse
newUpdateQuickConnectConfigResponse =
  UpdateQuickConnectConfigResponse'

instance Core.NFData UpdateQuickConnectConfigResponse
