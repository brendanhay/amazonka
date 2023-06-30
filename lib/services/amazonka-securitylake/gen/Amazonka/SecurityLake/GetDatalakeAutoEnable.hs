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
-- Module      : Amazonka.SecurityLake.GetDatalakeAutoEnable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the configuration that will be automatically set up for
-- accounts added to the organization after the organization has onboarded
-- to Amazon Security Lake. This API does not take input parameters.
module Amazonka.SecurityLake.GetDatalakeAutoEnable
  ( -- * Creating a Request
    GetDatalakeAutoEnable (..),
    newGetDatalakeAutoEnable,

    -- * Destructuring the Response
    GetDatalakeAutoEnableResponse (..),
    newGetDatalakeAutoEnableResponse,

    -- * Response Lenses
    getDatalakeAutoEnableResponse_httpStatus,
    getDatalakeAutoEnableResponse_autoEnableNewAccounts,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newGetDatalakeAutoEnable' smart constructor.
data GetDatalakeAutoEnable = GetDatalakeAutoEnable'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDatalakeAutoEnable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetDatalakeAutoEnable ::
  GetDatalakeAutoEnable
newGetDatalakeAutoEnable = GetDatalakeAutoEnable'

instance Core.AWSRequest GetDatalakeAutoEnable where
  type
    AWSResponse GetDatalakeAutoEnable =
      GetDatalakeAutoEnableResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDatalakeAutoEnableResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "autoEnableNewAccounts"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable GetDatalakeAutoEnable where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetDatalakeAutoEnable where
  rnf _ = ()

instance Data.ToHeaders GetDatalakeAutoEnable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetDatalakeAutoEnable where
  toPath = Prelude.const "/v1/datalake/autoenable"

instance Data.ToQuery GetDatalakeAutoEnable where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDatalakeAutoEnableResponse' smart constructor.
data GetDatalakeAutoEnableResponse = GetDatalakeAutoEnableResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The configuration for new accounts.
    autoEnableNewAccounts :: [AutoEnableNewRegionConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDatalakeAutoEnableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getDatalakeAutoEnableResponse_httpStatus' - The response's http status code.
--
-- 'autoEnableNewAccounts', 'getDatalakeAutoEnableResponse_autoEnableNewAccounts' - The configuration for new accounts.
newGetDatalakeAutoEnableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDatalakeAutoEnableResponse
newGetDatalakeAutoEnableResponse pHttpStatus_ =
  GetDatalakeAutoEnableResponse'
    { httpStatus =
        pHttpStatus_,
      autoEnableNewAccounts = Prelude.mempty
    }

-- | The response's http status code.
getDatalakeAutoEnableResponse_httpStatus :: Lens.Lens' GetDatalakeAutoEnableResponse Prelude.Int
getDatalakeAutoEnableResponse_httpStatus = Lens.lens (\GetDatalakeAutoEnableResponse' {httpStatus} -> httpStatus) (\s@GetDatalakeAutoEnableResponse' {} a -> s {httpStatus = a} :: GetDatalakeAutoEnableResponse)

-- | The configuration for new accounts.
getDatalakeAutoEnableResponse_autoEnableNewAccounts :: Lens.Lens' GetDatalakeAutoEnableResponse [AutoEnableNewRegionConfiguration]
getDatalakeAutoEnableResponse_autoEnableNewAccounts = Lens.lens (\GetDatalakeAutoEnableResponse' {autoEnableNewAccounts} -> autoEnableNewAccounts) (\s@GetDatalakeAutoEnableResponse' {} a -> s {autoEnableNewAccounts = a} :: GetDatalakeAutoEnableResponse) Prelude.. Lens.coerced

instance Prelude.NFData GetDatalakeAutoEnableResponse where
  rnf GetDatalakeAutoEnableResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf autoEnableNewAccounts
