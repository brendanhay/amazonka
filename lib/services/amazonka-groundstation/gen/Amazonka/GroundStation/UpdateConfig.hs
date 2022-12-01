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
-- Module      : Amazonka.GroundStation.UpdateConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the @Config@ used when scheduling contacts.
--
-- Updating a @Config@ will not update the execution parameters for
-- existing future contacts scheduled with this @Config@.
module Amazonka.GroundStation.UpdateConfig
  ( -- * Creating a Request
    UpdateConfig (..),
    newUpdateConfig,

    -- * Request Lenses
    updateConfig_configData,
    updateConfig_configId,
    updateConfig_configType,
    updateConfig_name,

    -- * Destructuring the Response
    ConfigIdResponse (..),
    newConfigIdResponse,

    -- * Response Lenses
    configIdResponse_configId,
    configIdResponse_configType,
    configIdResponse_configArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GroundStation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newUpdateConfig' smart constructor.
data UpdateConfig = UpdateConfig'
  { -- | Parameters of a @Config@.
    configData :: ConfigTypeData,
    -- | UUID of a @Config@.
    configId :: Prelude.Text,
    -- | Type of a @Config@.
    configType :: ConfigCapabilityType,
    -- | Name of a @Config@.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configData', 'updateConfig_configData' - Parameters of a @Config@.
--
-- 'configId', 'updateConfig_configId' - UUID of a @Config@.
--
-- 'configType', 'updateConfig_configType' - Type of a @Config@.
--
-- 'name', 'updateConfig_name' - Name of a @Config@.
newUpdateConfig ::
  -- | 'configData'
  ConfigTypeData ->
  -- | 'configId'
  Prelude.Text ->
  -- | 'configType'
  ConfigCapabilityType ->
  -- | 'name'
  Prelude.Text ->
  UpdateConfig
newUpdateConfig
  pConfigData_
  pConfigId_
  pConfigType_
  pName_ =
    UpdateConfig'
      { configData = pConfigData_,
        configId = pConfigId_,
        configType = pConfigType_,
        name = pName_
      }

-- | Parameters of a @Config@.
updateConfig_configData :: Lens.Lens' UpdateConfig ConfigTypeData
updateConfig_configData = Lens.lens (\UpdateConfig' {configData} -> configData) (\s@UpdateConfig' {} a -> s {configData = a} :: UpdateConfig)

-- | UUID of a @Config@.
updateConfig_configId :: Lens.Lens' UpdateConfig Prelude.Text
updateConfig_configId = Lens.lens (\UpdateConfig' {configId} -> configId) (\s@UpdateConfig' {} a -> s {configId = a} :: UpdateConfig)

-- | Type of a @Config@.
updateConfig_configType :: Lens.Lens' UpdateConfig ConfigCapabilityType
updateConfig_configType = Lens.lens (\UpdateConfig' {configType} -> configType) (\s@UpdateConfig' {} a -> s {configType = a} :: UpdateConfig)

-- | Name of a @Config@.
updateConfig_name :: Lens.Lens' UpdateConfig Prelude.Text
updateConfig_name = Lens.lens (\UpdateConfig' {name} -> name) (\s@UpdateConfig' {} a -> s {name = a} :: UpdateConfig)

instance Core.AWSRequest UpdateConfig where
  type AWSResponse UpdateConfig = ConfigIdResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable UpdateConfig where
  hashWithSalt _salt UpdateConfig' {..} =
    _salt `Prelude.hashWithSalt` configData
      `Prelude.hashWithSalt` configId
      `Prelude.hashWithSalt` configType
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateConfig where
  rnf UpdateConfig' {..} =
    Prelude.rnf configData
      `Prelude.seq` Prelude.rnf configId
      `Prelude.seq` Prelude.rnf configType
      `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders UpdateConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateConfig where
  toJSON UpdateConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("configData" Core..= configData),
            Prelude.Just ("name" Core..= name)
          ]
      )

instance Core.ToPath UpdateConfig where
  toPath UpdateConfig' {..} =
    Prelude.mconcat
      [ "/config/",
        Core.toBS configType,
        "/",
        Core.toBS configId
      ]

instance Core.ToQuery UpdateConfig where
  toQuery = Prelude.const Prelude.mempty
