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
-- Module      : Amazonka.Connect.UpdateQuickConnectConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration settings for the specified quick connect.
module Amazonka.Connect.UpdateQuickConnectConfig
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

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateQuickConnectConfig' smart constructor.
data UpdateQuickConnectConfig = UpdateQuickConnectConfig'
  { -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier for the quick connect.
    quickConnectId :: Prelude.Text,
    -- | Information about the configuration settings for the quick connect.
    quickConnectConfig :: QuickConnectConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateQuickConnectConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'updateQuickConnectConfig_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
--
-- 'quickConnectId', 'updateQuickConnectConfig_quickConnectId' - The identifier for the quick connect.
--
-- 'quickConnectConfig', 'updateQuickConnectConfig_quickConnectConfig' - Information about the configuration settings for the quick connect.
newUpdateQuickConnectConfig ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'quickConnectId'
  Prelude.Text ->
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

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
updateQuickConnectConfig_instanceId :: Lens.Lens' UpdateQuickConnectConfig Prelude.Text
updateQuickConnectConfig_instanceId = Lens.lens (\UpdateQuickConnectConfig' {instanceId} -> instanceId) (\s@UpdateQuickConnectConfig' {} a -> s {instanceId = a} :: UpdateQuickConnectConfig)

-- | The identifier for the quick connect.
updateQuickConnectConfig_quickConnectId :: Lens.Lens' UpdateQuickConnectConfig Prelude.Text
updateQuickConnectConfig_quickConnectId = Lens.lens (\UpdateQuickConnectConfig' {quickConnectId} -> quickConnectId) (\s@UpdateQuickConnectConfig' {} a -> s {quickConnectId = a} :: UpdateQuickConnectConfig)

-- | Information about the configuration settings for the quick connect.
updateQuickConnectConfig_quickConnectConfig :: Lens.Lens' UpdateQuickConnectConfig QuickConnectConfig
updateQuickConnectConfig_quickConnectConfig = Lens.lens (\UpdateQuickConnectConfig' {quickConnectConfig} -> quickConnectConfig) (\s@UpdateQuickConnectConfig' {} a -> s {quickConnectConfig = a} :: UpdateQuickConnectConfig)

instance Core.AWSRequest UpdateQuickConnectConfig where
  type
    AWSResponse UpdateQuickConnectConfig =
      UpdateQuickConnectConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      UpdateQuickConnectConfigResponse'

instance Prelude.Hashable UpdateQuickConnectConfig where
  hashWithSalt _salt UpdateQuickConnectConfig' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` quickConnectId
      `Prelude.hashWithSalt` quickConnectConfig

instance Prelude.NFData UpdateQuickConnectConfig where
  rnf UpdateQuickConnectConfig' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf quickConnectId
      `Prelude.seq` Prelude.rnf quickConnectConfig

instance Data.ToHeaders UpdateQuickConnectConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateQuickConnectConfig where
  toJSON UpdateQuickConnectConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("QuickConnectConfig" Data..= quickConnectConfig)
          ]
      )

instance Data.ToPath UpdateQuickConnectConfig where
  toPath UpdateQuickConnectConfig' {..} =
    Prelude.mconcat
      [ "/quick-connects/",
        Data.toBS instanceId,
        "/",
        Data.toBS quickConnectId,
        "/config"
      ]

instance Data.ToQuery UpdateQuickConnectConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateQuickConnectConfigResponse' smart constructor.
data UpdateQuickConnectConfigResponse = UpdateQuickConnectConfigResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateQuickConnectConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateQuickConnectConfigResponse ::
  UpdateQuickConnectConfigResponse
newUpdateQuickConnectConfigResponse =
  UpdateQuickConnectConfigResponse'

instance
  Prelude.NFData
    UpdateQuickConnectConfigResponse
  where
  rnf _ = ()
