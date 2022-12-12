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
-- Module      : Amazonka.GroundStation.DeleteConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a @Config@.
module Amazonka.GroundStation.DeleteConfig
  ( -- * Creating a Request
    DeleteConfig (..),
    newDeleteConfig,

    -- * Request Lenses
    deleteConfig_configId,
    deleteConfig_configType,

    -- * Destructuring the Response
    ConfigIdResponse (..),
    newConfigIdResponse,

    -- * Response Lenses
    configIdResponse_configArn,
    configIdResponse_configId,
    configIdResponse_configType,
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
-- /See:/ 'newDeleteConfig' smart constructor.
data DeleteConfig = DeleteConfig'
  { -- | UUID of a @Config@.
    configId :: Prelude.Text,
    -- | Type of a @Config@.
    configType :: ConfigCapabilityType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configId', 'deleteConfig_configId' - UUID of a @Config@.
--
-- 'configType', 'deleteConfig_configType' - Type of a @Config@.
newDeleteConfig ::
  -- | 'configId'
  Prelude.Text ->
  -- | 'configType'
  ConfigCapabilityType ->
  DeleteConfig
newDeleteConfig pConfigId_ pConfigType_ =
  DeleteConfig'
    { configId = pConfigId_,
      configType = pConfigType_
    }

-- | UUID of a @Config@.
deleteConfig_configId :: Lens.Lens' DeleteConfig Prelude.Text
deleteConfig_configId = Lens.lens (\DeleteConfig' {configId} -> configId) (\s@DeleteConfig' {} a -> s {configId = a} :: DeleteConfig)

-- | Type of a @Config@.
deleteConfig_configType :: Lens.Lens' DeleteConfig ConfigCapabilityType
deleteConfig_configType = Lens.lens (\DeleteConfig' {configType} -> configType) (\s@DeleteConfig' {} a -> s {configType = a} :: DeleteConfig)

instance Core.AWSRequest DeleteConfig where
  type AWSResponse DeleteConfig = ConfigIdResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable DeleteConfig where
  hashWithSalt _salt DeleteConfig' {..} =
    _salt `Prelude.hashWithSalt` configId
      `Prelude.hashWithSalt` configType

instance Prelude.NFData DeleteConfig where
  rnf DeleteConfig' {..} =
    Prelude.rnf configId
      `Prelude.seq` Prelude.rnf configType

instance Data.ToHeaders DeleteConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteConfig where
  toPath DeleteConfig' {..} =
    Prelude.mconcat
      [ "/config/",
        Data.toBS configType,
        "/",
        Data.toBS configId
      ]

instance Data.ToQuery DeleteConfig where
  toQuery = Prelude.const Prelude.mempty
