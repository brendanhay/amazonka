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
-- Module      : Amazonka.SecurityLake.GetDatalake
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the Amazon Security Lake configuration object for the
-- specified Amazon Web Services account ID. You can use the @GetDatalake@
-- API to know whether Security Lake is enabled for the current Region.
-- This API does not take input parameters.
module Amazonka.SecurityLake.GetDatalake
  ( -- * Creating a Request
    GetDatalake (..),
    newGetDatalake,

    -- * Destructuring the Response
    GetDatalakeResponse (..),
    newGetDatalakeResponse,

    -- * Response Lenses
    getDatalakeResponse_httpStatus,
    getDatalakeResponse_configurations,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newGetDatalake' smart constructor.
data GetDatalake = GetDatalake'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDatalake' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetDatalake ::
  GetDatalake
newGetDatalake = GetDatalake'

instance Core.AWSRequest GetDatalake where
  type AWSResponse GetDatalake = GetDatalakeResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDatalakeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "configurations"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable GetDatalake where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetDatalake where
  rnf _ = ()

instance Data.ToHeaders GetDatalake where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetDatalake where
  toPath = Prelude.const "/v1/datalake"

instance Data.ToQuery GetDatalake where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDatalakeResponse' smart constructor.
data GetDatalakeResponse = GetDatalakeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Retrieves the Security Lake configuration object.
    configurations :: Prelude.HashMap Region LakeConfigurationResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDatalakeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getDatalakeResponse_httpStatus' - The response's http status code.
--
-- 'configurations', 'getDatalakeResponse_configurations' - Retrieves the Security Lake configuration object.
newGetDatalakeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDatalakeResponse
newGetDatalakeResponse pHttpStatus_ =
  GetDatalakeResponse'
    { httpStatus = pHttpStatus_,
      configurations = Prelude.mempty
    }

-- | The response's http status code.
getDatalakeResponse_httpStatus :: Lens.Lens' GetDatalakeResponse Prelude.Int
getDatalakeResponse_httpStatus = Lens.lens (\GetDatalakeResponse' {httpStatus} -> httpStatus) (\s@GetDatalakeResponse' {} a -> s {httpStatus = a} :: GetDatalakeResponse)

-- | Retrieves the Security Lake configuration object.
getDatalakeResponse_configurations :: Lens.Lens' GetDatalakeResponse (Prelude.HashMap Region LakeConfigurationResponse)
getDatalakeResponse_configurations = Lens.lens (\GetDatalakeResponse' {configurations} -> configurations) (\s@GetDatalakeResponse' {} a -> s {configurations = a} :: GetDatalakeResponse) Prelude.. Lens.coerced

instance Prelude.NFData GetDatalakeResponse where
  rnf GetDatalakeResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf configurations
