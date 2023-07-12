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
-- Module      : Amazonka.ServiceCatalogAppRegistry.GetConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a @TagKey@ configuration from an account.
module Amazonka.ServiceCatalogAppRegistry.GetConfiguration
  ( -- * Creating a Request
    GetConfiguration (..),
    newGetConfiguration,

    -- * Destructuring the Response
    GetConfigurationResponse (..),
    newGetConfigurationResponse,

    -- * Response Lenses
    getConfigurationResponse_configuration,
    getConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalogAppRegistry.Types

-- | /See:/ 'newGetConfiguration' smart constructor.
data GetConfiguration = GetConfiguration'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetConfiguration ::
  GetConfiguration
newGetConfiguration = GetConfiguration'

instance Core.AWSRequest GetConfiguration where
  type
    AWSResponse GetConfiguration =
      GetConfigurationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConfigurationResponse'
            Prelude.<$> (x Data..?> "configuration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetConfiguration where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetConfiguration where
  rnf _ = ()

instance Data.ToHeaders GetConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetConfiguration where
  toPath = Prelude.const "/configuration"

instance Data.ToQuery GetConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetConfigurationResponse' smart constructor.
data GetConfigurationResponse = GetConfigurationResponse'
  { -- | Retrieves @TagKey@ configuration from an account.
    configuration :: Prelude.Maybe AppRegistryConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuration', 'getConfigurationResponse_configuration' - Retrieves @TagKey@ configuration from an account.
--
-- 'httpStatus', 'getConfigurationResponse_httpStatus' - The response's http status code.
newGetConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetConfigurationResponse
newGetConfigurationResponse pHttpStatus_ =
  GetConfigurationResponse'
    { configuration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Retrieves @TagKey@ configuration from an account.
getConfigurationResponse_configuration :: Lens.Lens' GetConfigurationResponse (Prelude.Maybe AppRegistryConfiguration)
getConfigurationResponse_configuration = Lens.lens (\GetConfigurationResponse' {configuration} -> configuration) (\s@GetConfigurationResponse' {} a -> s {configuration = a} :: GetConfigurationResponse)

-- | The response's http status code.
getConfigurationResponse_httpStatus :: Lens.Lens' GetConfigurationResponse Prelude.Int
getConfigurationResponse_httpStatus = Lens.lens (\GetConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetConfigurationResponse' {} a -> s {httpStatus = a} :: GetConfigurationResponse)

instance Prelude.NFData GetConfigurationResponse where
  rnf GetConfigurationResponse' {..} =
    Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf httpStatus
