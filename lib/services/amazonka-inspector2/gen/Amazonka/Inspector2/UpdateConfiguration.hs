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
-- Module      : Amazonka.Inspector2.UpdateConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates setting configurations for your Amazon Inspector account. When
-- you use this API as an Amazon Inspector delegated administrator this
-- updates the setting for all accounts you manage. Member accounts in an
-- organization cannot update this setting.
module Amazonka.Inspector2.UpdateConfiguration
  ( -- * Creating a Request
    UpdateConfiguration (..),
    newUpdateConfiguration,

    -- * Request Lenses
    updateConfiguration_ecrConfiguration,

    -- * Destructuring the Response
    UpdateConfigurationResponse (..),
    newUpdateConfigurationResponse,

    -- * Response Lenses
    updateConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateConfiguration' smart constructor.
data UpdateConfiguration = UpdateConfiguration'
  { -- | Specifies how the ECR automated re-scan will be updated for your
    -- environment.
    ecrConfiguration :: EcrConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ecrConfiguration', 'updateConfiguration_ecrConfiguration' - Specifies how the ECR automated re-scan will be updated for your
-- environment.
newUpdateConfiguration ::
  -- | 'ecrConfiguration'
  EcrConfiguration ->
  UpdateConfiguration
newUpdateConfiguration pEcrConfiguration_ =
  UpdateConfiguration'
    { ecrConfiguration =
        pEcrConfiguration_
    }

-- | Specifies how the ECR automated re-scan will be updated for your
-- environment.
updateConfiguration_ecrConfiguration :: Lens.Lens' UpdateConfiguration EcrConfiguration
updateConfiguration_ecrConfiguration = Lens.lens (\UpdateConfiguration' {ecrConfiguration} -> ecrConfiguration) (\s@UpdateConfiguration' {} a -> s {ecrConfiguration = a} :: UpdateConfiguration)

instance Core.AWSRequest UpdateConfiguration where
  type
    AWSResponse UpdateConfiguration =
      UpdateConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateConfiguration where
  hashWithSalt _salt UpdateConfiguration' {..} =
    _salt `Prelude.hashWithSalt` ecrConfiguration

instance Prelude.NFData UpdateConfiguration where
  rnf UpdateConfiguration' {..} =
    Prelude.rnf ecrConfiguration

instance Core.ToHeaders UpdateConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateConfiguration where
  toJSON UpdateConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ecrConfiguration" Core..= ecrConfiguration)
          ]
      )

instance Core.ToPath UpdateConfiguration where
  toPath = Prelude.const "/configuration/update"

instance Core.ToQuery UpdateConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateConfigurationResponse' smart constructor.
data UpdateConfigurationResponse = UpdateConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateConfigurationResponse_httpStatus' - The response's http status code.
newUpdateConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateConfigurationResponse
newUpdateConfigurationResponse pHttpStatus_ =
  UpdateConfigurationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateConfigurationResponse_httpStatus :: Lens.Lens' UpdateConfigurationResponse Prelude.Int
updateConfigurationResponse_httpStatus = Lens.lens (\UpdateConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateConfigurationResponse)

instance Prelude.NFData UpdateConfigurationResponse where
  rnf UpdateConfigurationResponse' {..} =
    Prelude.rnf httpStatus
