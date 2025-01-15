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
-- Module      : Amazonka.Inspector2.GetConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves setting configurations for Inspector scans.
module Amazonka.Inspector2.GetConfiguration
  ( -- * Creating a Request
    GetConfiguration (..),
    newGetConfiguration,

    -- * Destructuring the Response
    GetConfigurationResponse (..),
    newGetConfigurationResponse,

    -- * Response Lenses
    getConfigurationResponse_ecrConfiguration,
    getConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConfigurationResponse'
            Prelude.<$> (x Data..?> "ecrConfiguration")
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

instance Data.ToJSON GetConfiguration where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath GetConfiguration where
  toPath = Prelude.const "/configuration/get"

instance Data.ToQuery GetConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetConfigurationResponse' smart constructor.
data GetConfigurationResponse = GetConfigurationResponse'
  { -- | Specifies how the ECR automated re-scan duration is currently configured
    -- for your environment.
    ecrConfiguration :: Prelude.Maybe EcrConfigurationState,
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
-- 'ecrConfiguration', 'getConfigurationResponse_ecrConfiguration' - Specifies how the ECR automated re-scan duration is currently configured
-- for your environment.
--
-- 'httpStatus', 'getConfigurationResponse_httpStatus' - The response's http status code.
newGetConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetConfigurationResponse
newGetConfigurationResponse pHttpStatus_ =
  GetConfigurationResponse'
    { ecrConfiguration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Specifies how the ECR automated re-scan duration is currently configured
-- for your environment.
getConfigurationResponse_ecrConfiguration :: Lens.Lens' GetConfigurationResponse (Prelude.Maybe EcrConfigurationState)
getConfigurationResponse_ecrConfiguration = Lens.lens (\GetConfigurationResponse' {ecrConfiguration} -> ecrConfiguration) (\s@GetConfigurationResponse' {} a -> s {ecrConfiguration = a} :: GetConfigurationResponse)

-- | The response's http status code.
getConfigurationResponse_httpStatus :: Lens.Lens' GetConfigurationResponse Prelude.Int
getConfigurationResponse_httpStatus = Lens.lens (\GetConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetConfigurationResponse' {} a -> s {httpStatus = a} :: GetConfigurationResponse)

instance Prelude.NFData GetConfigurationResponse where
  rnf GetConfigurationResponse' {..} =
    Prelude.rnf ecrConfiguration `Prelude.seq`
      Prelude.rnf httpStatus
