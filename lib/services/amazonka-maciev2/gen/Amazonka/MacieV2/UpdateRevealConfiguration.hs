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
-- Module      : Amazonka.MacieV2.UpdateRevealConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the status and configuration settings for retrieving occurrences
-- of sensitive data reported by findings.
module Amazonka.MacieV2.UpdateRevealConfiguration
  ( -- * Creating a Request
    UpdateRevealConfiguration (..),
    newUpdateRevealConfiguration,

    -- * Request Lenses
    updateRevealConfiguration_configuration,

    -- * Destructuring the Response
    UpdateRevealConfigurationResponse (..),
    newUpdateRevealConfigurationResponse,

    -- * Response Lenses
    updateRevealConfigurationResponse_configuration,
    updateRevealConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateRevealConfiguration' smart constructor.
data UpdateRevealConfiguration = UpdateRevealConfiguration'
  { -- | The new configuration settings and the status of the configuration for
    -- the account.
    configuration :: RevealConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRevealConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuration', 'updateRevealConfiguration_configuration' - The new configuration settings and the status of the configuration for
-- the account.
newUpdateRevealConfiguration ::
  -- | 'configuration'
  RevealConfiguration ->
  UpdateRevealConfiguration
newUpdateRevealConfiguration pConfiguration_ =
  UpdateRevealConfiguration'
    { configuration =
        pConfiguration_
    }

-- | The new configuration settings and the status of the configuration for
-- the account.
updateRevealConfiguration_configuration :: Lens.Lens' UpdateRevealConfiguration RevealConfiguration
updateRevealConfiguration_configuration = Lens.lens (\UpdateRevealConfiguration' {configuration} -> configuration) (\s@UpdateRevealConfiguration' {} a -> s {configuration = a} :: UpdateRevealConfiguration)

instance Core.AWSRequest UpdateRevealConfiguration where
  type
    AWSResponse UpdateRevealConfiguration =
      UpdateRevealConfigurationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRevealConfigurationResponse'
            Prelude.<$> (x Data..?> "configuration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRevealConfiguration where
  hashWithSalt _salt UpdateRevealConfiguration' {..} =
    _salt `Prelude.hashWithSalt` configuration

instance Prelude.NFData UpdateRevealConfiguration where
  rnf UpdateRevealConfiguration' {..} =
    Prelude.rnf configuration

instance Data.ToHeaders UpdateRevealConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRevealConfiguration where
  toJSON UpdateRevealConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("configuration" Data..= configuration)
          ]
      )

instance Data.ToPath UpdateRevealConfiguration where
  toPath = Prelude.const "/reveal-configuration"

instance Data.ToQuery UpdateRevealConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRevealConfigurationResponse' smart constructor.
data UpdateRevealConfigurationResponse = UpdateRevealConfigurationResponse'
  { -- | The new configuration settings and the status of the configuration for
    -- the account.
    configuration :: Prelude.Maybe RevealConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRevealConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuration', 'updateRevealConfigurationResponse_configuration' - The new configuration settings and the status of the configuration for
-- the account.
--
-- 'httpStatus', 'updateRevealConfigurationResponse_httpStatus' - The response's http status code.
newUpdateRevealConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRevealConfigurationResponse
newUpdateRevealConfigurationResponse pHttpStatus_ =
  UpdateRevealConfigurationResponse'
    { configuration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The new configuration settings and the status of the configuration for
-- the account.
updateRevealConfigurationResponse_configuration :: Lens.Lens' UpdateRevealConfigurationResponse (Prelude.Maybe RevealConfiguration)
updateRevealConfigurationResponse_configuration = Lens.lens (\UpdateRevealConfigurationResponse' {configuration} -> configuration) (\s@UpdateRevealConfigurationResponse' {} a -> s {configuration = a} :: UpdateRevealConfigurationResponse)

-- | The response's http status code.
updateRevealConfigurationResponse_httpStatus :: Lens.Lens' UpdateRevealConfigurationResponse Prelude.Int
updateRevealConfigurationResponse_httpStatus = Lens.lens (\UpdateRevealConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateRevealConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateRevealConfigurationResponse)

instance
  Prelude.NFData
    UpdateRevealConfigurationResponse
  where
  rnf UpdateRevealConfigurationResponse' {..} =
    Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf httpStatus
