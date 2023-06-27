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
-- Module      : Amazonka.Comprehend.UpdateFlywheel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the configuration information for an existing flywheel.
module Amazonka.Comprehend.UpdateFlywheel
  ( -- * Creating a Request
    UpdateFlywheel (..),
    newUpdateFlywheel,

    -- * Request Lenses
    updateFlywheel_activeModelArn,
    updateFlywheel_dataAccessRoleArn,
    updateFlywheel_dataSecurityConfig,
    updateFlywheel_flywheelArn,

    -- * Destructuring the Response
    UpdateFlywheelResponse (..),
    newUpdateFlywheelResponse,

    -- * Response Lenses
    updateFlywheelResponse_flywheelProperties,
    updateFlywheelResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateFlywheel' smart constructor.
data UpdateFlywheel = UpdateFlywheel'
  { -- | The Amazon Resource Number (ARN) of the active model version.
    activeModelArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role that grants Amazon
    -- Comprehend permission to access the flywheel data.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Flywheel data security configuration.
    dataSecurityConfig :: Prelude.Maybe UpdateDataSecurityConfig,
    -- | The Amazon Resource Number (ARN) of the flywheel to update.
    flywheelArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFlywheel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeModelArn', 'updateFlywheel_activeModelArn' - The Amazon Resource Number (ARN) of the active model version.
--
-- 'dataAccessRoleArn', 'updateFlywheel_dataAccessRoleArn' - The Amazon Resource Name (ARN) of the IAM role that grants Amazon
-- Comprehend permission to access the flywheel data.
--
-- 'dataSecurityConfig', 'updateFlywheel_dataSecurityConfig' - Flywheel data security configuration.
--
-- 'flywheelArn', 'updateFlywheel_flywheelArn' - The Amazon Resource Number (ARN) of the flywheel to update.
newUpdateFlywheel ::
  -- | 'flywheelArn'
  Prelude.Text ->
  UpdateFlywheel
newUpdateFlywheel pFlywheelArn_ =
  UpdateFlywheel'
    { activeModelArn = Prelude.Nothing,
      dataAccessRoleArn = Prelude.Nothing,
      dataSecurityConfig = Prelude.Nothing,
      flywheelArn = pFlywheelArn_
    }

-- | The Amazon Resource Number (ARN) of the active model version.
updateFlywheel_activeModelArn :: Lens.Lens' UpdateFlywheel (Prelude.Maybe Prelude.Text)
updateFlywheel_activeModelArn = Lens.lens (\UpdateFlywheel' {activeModelArn} -> activeModelArn) (\s@UpdateFlywheel' {} a -> s {activeModelArn = a} :: UpdateFlywheel)

-- | The Amazon Resource Name (ARN) of the IAM role that grants Amazon
-- Comprehend permission to access the flywheel data.
updateFlywheel_dataAccessRoleArn :: Lens.Lens' UpdateFlywheel (Prelude.Maybe Prelude.Text)
updateFlywheel_dataAccessRoleArn = Lens.lens (\UpdateFlywheel' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@UpdateFlywheel' {} a -> s {dataAccessRoleArn = a} :: UpdateFlywheel)

-- | Flywheel data security configuration.
updateFlywheel_dataSecurityConfig :: Lens.Lens' UpdateFlywheel (Prelude.Maybe UpdateDataSecurityConfig)
updateFlywheel_dataSecurityConfig = Lens.lens (\UpdateFlywheel' {dataSecurityConfig} -> dataSecurityConfig) (\s@UpdateFlywheel' {} a -> s {dataSecurityConfig = a} :: UpdateFlywheel)

-- | The Amazon Resource Number (ARN) of the flywheel to update.
updateFlywheel_flywheelArn :: Lens.Lens' UpdateFlywheel Prelude.Text
updateFlywheel_flywheelArn = Lens.lens (\UpdateFlywheel' {flywheelArn} -> flywheelArn) (\s@UpdateFlywheel' {} a -> s {flywheelArn = a} :: UpdateFlywheel)

instance Core.AWSRequest UpdateFlywheel where
  type
    AWSResponse UpdateFlywheel =
      UpdateFlywheelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFlywheelResponse'
            Prelude.<$> (x Data..?> "FlywheelProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateFlywheel where
  hashWithSalt _salt UpdateFlywheel' {..} =
    _salt
      `Prelude.hashWithSalt` activeModelArn
      `Prelude.hashWithSalt` dataAccessRoleArn
      `Prelude.hashWithSalt` dataSecurityConfig
      `Prelude.hashWithSalt` flywheelArn

instance Prelude.NFData UpdateFlywheel where
  rnf UpdateFlywheel' {..} =
    Prelude.rnf activeModelArn
      `Prelude.seq` Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf dataSecurityConfig
      `Prelude.seq` Prelude.rnf flywheelArn

instance Data.ToHeaders UpdateFlywheel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.UpdateFlywheel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateFlywheel where
  toJSON UpdateFlywheel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ActiveModelArn" Data..=)
              Prelude.<$> activeModelArn,
            ("DataAccessRoleArn" Data..=)
              Prelude.<$> dataAccessRoleArn,
            ("DataSecurityConfig" Data..=)
              Prelude.<$> dataSecurityConfig,
            Prelude.Just ("FlywheelArn" Data..= flywheelArn)
          ]
      )

instance Data.ToPath UpdateFlywheel where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateFlywheel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFlywheelResponse' smart constructor.
data UpdateFlywheelResponse = UpdateFlywheelResponse'
  { -- | The flywheel properties.
    flywheelProperties :: Prelude.Maybe FlywheelProperties,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFlywheelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flywheelProperties', 'updateFlywheelResponse_flywheelProperties' - The flywheel properties.
--
-- 'httpStatus', 'updateFlywheelResponse_httpStatus' - The response's http status code.
newUpdateFlywheelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFlywheelResponse
newUpdateFlywheelResponse pHttpStatus_ =
  UpdateFlywheelResponse'
    { flywheelProperties =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The flywheel properties.
updateFlywheelResponse_flywheelProperties :: Lens.Lens' UpdateFlywheelResponse (Prelude.Maybe FlywheelProperties)
updateFlywheelResponse_flywheelProperties = Lens.lens (\UpdateFlywheelResponse' {flywheelProperties} -> flywheelProperties) (\s@UpdateFlywheelResponse' {} a -> s {flywheelProperties = a} :: UpdateFlywheelResponse)

-- | The response's http status code.
updateFlywheelResponse_httpStatus :: Lens.Lens' UpdateFlywheelResponse Prelude.Int
updateFlywheelResponse_httpStatus = Lens.lens (\UpdateFlywheelResponse' {httpStatus} -> httpStatus) (\s@UpdateFlywheelResponse' {} a -> s {httpStatus = a} :: UpdateFlywheelResponse)

instance Prelude.NFData UpdateFlywheelResponse where
  rnf UpdateFlywheelResponse' {..} =
    Prelude.rnf flywheelProperties
      `Prelude.seq` Prelude.rnf httpStatus
