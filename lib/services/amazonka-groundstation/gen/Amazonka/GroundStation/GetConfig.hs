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
-- Module      : Amazonka.GroundStation.GetConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns @Config@ information.
--
-- Only one @Config@ response can be returned.
module Amazonka.GroundStation.GetConfig
  ( -- * Creating a Request
    GetConfig (..),
    newGetConfig,

    -- * Request Lenses
    getConfig_configId,
    getConfig_configType,

    -- * Destructuring the Response
    GetConfigResponse (..),
    newGetConfigResponse,

    -- * Response Lenses
    getConfigResponse_configType,
    getConfigResponse_tags,
    getConfigResponse_httpStatus,
    getConfigResponse_configArn,
    getConfigResponse_configData,
    getConfigResponse_configId,
    getConfigResponse_name,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newGetConfig' smart constructor.
data GetConfig = GetConfig'
  { -- | UUID of a @Config@.
    configId :: Prelude.Text,
    -- | Type of a @Config@.
    configType :: ConfigCapabilityType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configId', 'getConfig_configId' - UUID of a @Config@.
--
-- 'configType', 'getConfig_configType' - Type of a @Config@.
newGetConfig ::
  -- | 'configId'
  Prelude.Text ->
  -- | 'configType'
  ConfigCapabilityType ->
  GetConfig
newGetConfig pConfigId_ pConfigType_ =
  GetConfig'
    { configId = pConfigId_,
      configType = pConfigType_
    }

-- | UUID of a @Config@.
getConfig_configId :: Lens.Lens' GetConfig Prelude.Text
getConfig_configId = Lens.lens (\GetConfig' {configId} -> configId) (\s@GetConfig' {} a -> s {configId = a} :: GetConfig)

-- | Type of a @Config@.
getConfig_configType :: Lens.Lens' GetConfig ConfigCapabilityType
getConfig_configType = Lens.lens (\GetConfig' {configType} -> configType) (\s@GetConfig' {} a -> s {configType = a} :: GetConfig)

instance Core.AWSRequest GetConfig where
  type AWSResponse GetConfig = GetConfigResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConfigResponse'
            Prelude.<$> (x Data..?> "configType")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "configArn")
            Prelude.<*> (x Data..:> "configData")
            Prelude.<*> (x Data..:> "configId")
            Prelude.<*> (x Data..:> "name")
      )

instance Prelude.Hashable GetConfig where
  hashWithSalt _salt GetConfig' {..} =
    _salt
      `Prelude.hashWithSalt` configId
      `Prelude.hashWithSalt` configType

instance Prelude.NFData GetConfig where
  rnf GetConfig' {..} =
    Prelude.rnf configId `Prelude.seq`
      Prelude.rnf configType

instance Data.ToHeaders GetConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetConfig where
  toPath GetConfig' {..} =
    Prelude.mconcat
      [ "/config/",
        Data.toBS configType,
        "/",
        Data.toBS configId
      ]

instance Data.ToQuery GetConfig where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newGetConfigResponse' smart constructor.
data GetConfigResponse = GetConfigResponse'
  { -- | Type of a @Config@.
    configType :: Prelude.Maybe ConfigCapabilityType,
    -- | Tags assigned to a @Config@.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | ARN of a @Config@
    configArn :: Prelude.Text,
    -- | Data elements in a @Config@.
    configData :: ConfigTypeData,
    -- | UUID of a @Config@.
    configId :: Prelude.Text,
    -- | Name of a @Config@.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configType', 'getConfigResponse_configType' - Type of a @Config@.
--
-- 'tags', 'getConfigResponse_tags' - Tags assigned to a @Config@.
--
-- 'httpStatus', 'getConfigResponse_httpStatus' - The response's http status code.
--
-- 'configArn', 'getConfigResponse_configArn' - ARN of a @Config@
--
-- 'configData', 'getConfigResponse_configData' - Data elements in a @Config@.
--
-- 'configId', 'getConfigResponse_configId' - UUID of a @Config@.
--
-- 'name', 'getConfigResponse_name' - Name of a @Config@.
newGetConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'configArn'
  Prelude.Text ->
  -- | 'configData'
  ConfigTypeData ->
  -- | 'configId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  GetConfigResponse
newGetConfigResponse
  pHttpStatus_
  pConfigArn_
  pConfigData_
  pConfigId_
  pName_ =
    GetConfigResponse'
      { configType = Prelude.Nothing,
        tags = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        configArn = pConfigArn_,
        configData = pConfigData_,
        configId = pConfigId_,
        name = pName_
      }

-- | Type of a @Config@.
getConfigResponse_configType :: Lens.Lens' GetConfigResponse (Prelude.Maybe ConfigCapabilityType)
getConfigResponse_configType = Lens.lens (\GetConfigResponse' {configType} -> configType) (\s@GetConfigResponse' {} a -> s {configType = a} :: GetConfigResponse)

-- | Tags assigned to a @Config@.
getConfigResponse_tags :: Lens.Lens' GetConfigResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getConfigResponse_tags = Lens.lens (\GetConfigResponse' {tags} -> tags) (\s@GetConfigResponse' {} a -> s {tags = a} :: GetConfigResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getConfigResponse_httpStatus :: Lens.Lens' GetConfigResponse Prelude.Int
getConfigResponse_httpStatus = Lens.lens (\GetConfigResponse' {httpStatus} -> httpStatus) (\s@GetConfigResponse' {} a -> s {httpStatus = a} :: GetConfigResponse)

-- | ARN of a @Config@
getConfigResponse_configArn :: Lens.Lens' GetConfigResponse Prelude.Text
getConfigResponse_configArn = Lens.lens (\GetConfigResponse' {configArn} -> configArn) (\s@GetConfigResponse' {} a -> s {configArn = a} :: GetConfigResponse)

-- | Data elements in a @Config@.
getConfigResponse_configData :: Lens.Lens' GetConfigResponse ConfigTypeData
getConfigResponse_configData = Lens.lens (\GetConfigResponse' {configData} -> configData) (\s@GetConfigResponse' {} a -> s {configData = a} :: GetConfigResponse)

-- | UUID of a @Config@.
getConfigResponse_configId :: Lens.Lens' GetConfigResponse Prelude.Text
getConfigResponse_configId = Lens.lens (\GetConfigResponse' {configId} -> configId) (\s@GetConfigResponse' {} a -> s {configId = a} :: GetConfigResponse)

-- | Name of a @Config@.
getConfigResponse_name :: Lens.Lens' GetConfigResponse Prelude.Text
getConfigResponse_name = Lens.lens (\GetConfigResponse' {name} -> name) (\s@GetConfigResponse' {} a -> s {name = a} :: GetConfigResponse)

instance Prelude.NFData GetConfigResponse where
  rnf GetConfigResponse' {..} =
    Prelude.rnf configType `Prelude.seq`
      Prelude.rnf tags `Prelude.seq`
        Prelude.rnf httpStatus `Prelude.seq`
          Prelude.rnf configArn `Prelude.seq`
            Prelude.rnf configData `Prelude.seq`
              Prelude.rnf configId `Prelude.seq`
                Prelude.rnf name
