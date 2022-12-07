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
-- Module      : Amazonka.MigrationHubStrategy.UpdateServerConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration of the specified server.
module Amazonka.MigrationHubStrategy.UpdateServerConfig
  ( -- * Creating a Request
    UpdateServerConfig (..),
    newUpdateServerConfig,

    -- * Request Lenses
    updateServerConfig_strategyOption,
    updateServerConfig_serverId,

    -- * Destructuring the Response
    UpdateServerConfigResponse (..),
    newUpdateServerConfigResponse,

    -- * Response Lenses
    updateServerConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateServerConfig' smart constructor.
data UpdateServerConfig = UpdateServerConfig'
  { -- | The preferred strategy options for the application component. See the
    -- response from GetServerStrategies.
    strategyOption :: Prelude.Maybe StrategyOption,
    -- | The ID of the server.
    serverId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateServerConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'strategyOption', 'updateServerConfig_strategyOption' - The preferred strategy options for the application component. See the
-- response from GetServerStrategies.
--
-- 'serverId', 'updateServerConfig_serverId' - The ID of the server.
newUpdateServerConfig ::
  -- | 'serverId'
  Prelude.Text ->
  UpdateServerConfig
newUpdateServerConfig pServerId_ =
  UpdateServerConfig'
    { strategyOption =
        Prelude.Nothing,
      serverId = pServerId_
    }

-- | The preferred strategy options for the application component. See the
-- response from GetServerStrategies.
updateServerConfig_strategyOption :: Lens.Lens' UpdateServerConfig (Prelude.Maybe StrategyOption)
updateServerConfig_strategyOption = Lens.lens (\UpdateServerConfig' {strategyOption} -> strategyOption) (\s@UpdateServerConfig' {} a -> s {strategyOption = a} :: UpdateServerConfig)

-- | The ID of the server.
updateServerConfig_serverId :: Lens.Lens' UpdateServerConfig Prelude.Text
updateServerConfig_serverId = Lens.lens (\UpdateServerConfig' {serverId} -> serverId) (\s@UpdateServerConfig' {} a -> s {serverId = a} :: UpdateServerConfig)

instance Core.AWSRequest UpdateServerConfig where
  type
    AWSResponse UpdateServerConfig =
      UpdateServerConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateServerConfigResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateServerConfig where
  hashWithSalt _salt UpdateServerConfig' {..} =
    _salt `Prelude.hashWithSalt` strategyOption
      `Prelude.hashWithSalt` serverId

instance Prelude.NFData UpdateServerConfig where
  rnf UpdateServerConfig' {..} =
    Prelude.rnf strategyOption
      `Prelude.seq` Prelude.rnf serverId

instance Data.ToHeaders UpdateServerConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateServerConfig where
  toJSON UpdateServerConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("strategyOption" Data..=)
              Prelude.<$> strategyOption,
            Prelude.Just ("serverId" Data..= serverId)
          ]
      )

instance Data.ToPath UpdateServerConfig where
  toPath = Prelude.const "/update-server-config/"

instance Data.ToQuery UpdateServerConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateServerConfigResponse' smart constructor.
data UpdateServerConfigResponse = UpdateServerConfigResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateServerConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateServerConfigResponse_httpStatus' - The response's http status code.
newUpdateServerConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateServerConfigResponse
newUpdateServerConfigResponse pHttpStatus_ =
  UpdateServerConfigResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateServerConfigResponse_httpStatus :: Lens.Lens' UpdateServerConfigResponse Prelude.Int
updateServerConfigResponse_httpStatus = Lens.lens (\UpdateServerConfigResponse' {httpStatus} -> httpStatus) (\s@UpdateServerConfigResponse' {} a -> s {httpStatus = a} :: UpdateServerConfigResponse)

instance Prelude.NFData UpdateServerConfigResponse where
  rnf UpdateServerConfigResponse' {..} =
    Prelude.rnf httpStatus
