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
-- Module      : Amazonka.SMS.GetAppValidationOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves output from validating an application.
module Amazonka.SMS.GetAppValidationOutput
  ( -- * Creating a Request
    GetAppValidationOutput (..),
    newGetAppValidationOutput,

    -- * Request Lenses
    getAppValidationOutput_appId,

    -- * Destructuring the Response
    GetAppValidationOutputResponse (..),
    newGetAppValidationOutputResponse,

    -- * Response Lenses
    getAppValidationOutputResponse_validationOutputList,
    getAppValidationOutputResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SMS.Types

-- | /See:/ 'newGetAppValidationOutput' smart constructor.
data GetAppValidationOutput = GetAppValidationOutput'
  { -- | The ID of the application.
    appId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAppValidationOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'getAppValidationOutput_appId' - The ID of the application.
newGetAppValidationOutput ::
  -- | 'appId'
  Prelude.Text ->
  GetAppValidationOutput
newGetAppValidationOutput pAppId_ =
  GetAppValidationOutput' {appId = pAppId_}

-- | The ID of the application.
getAppValidationOutput_appId :: Lens.Lens' GetAppValidationOutput Prelude.Text
getAppValidationOutput_appId = Lens.lens (\GetAppValidationOutput' {appId} -> appId) (\s@GetAppValidationOutput' {} a -> s {appId = a} :: GetAppValidationOutput)

instance Core.AWSRequest GetAppValidationOutput where
  type
    AWSResponse GetAppValidationOutput =
      GetAppValidationOutputResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAppValidationOutputResponse'
            Prelude.<$> ( x
                            Data..?> "validationOutputList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAppValidationOutput where
  hashWithSalt _salt GetAppValidationOutput' {..} =
    _salt `Prelude.hashWithSalt` appId

instance Prelude.NFData GetAppValidationOutput where
  rnf GetAppValidationOutput' {..} = Prelude.rnf appId

instance Data.ToHeaders GetAppValidationOutput where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSServerMigrationService_V2016_10_24.GetAppValidationOutput" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetAppValidationOutput where
  toJSON GetAppValidationOutput' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("appId" Data..= appId)]
      )

instance Data.ToPath GetAppValidationOutput where
  toPath = Prelude.const "/"

instance Data.ToQuery GetAppValidationOutput where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAppValidationOutputResponse' smart constructor.
data GetAppValidationOutputResponse = GetAppValidationOutputResponse'
  { -- | The validation output.
    validationOutputList :: Prelude.Maybe [ValidationOutput],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAppValidationOutputResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'validationOutputList', 'getAppValidationOutputResponse_validationOutputList' - The validation output.
--
-- 'httpStatus', 'getAppValidationOutputResponse_httpStatus' - The response's http status code.
newGetAppValidationOutputResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAppValidationOutputResponse
newGetAppValidationOutputResponse pHttpStatus_ =
  GetAppValidationOutputResponse'
    { validationOutputList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The validation output.
getAppValidationOutputResponse_validationOutputList :: Lens.Lens' GetAppValidationOutputResponse (Prelude.Maybe [ValidationOutput])
getAppValidationOutputResponse_validationOutputList = Lens.lens (\GetAppValidationOutputResponse' {validationOutputList} -> validationOutputList) (\s@GetAppValidationOutputResponse' {} a -> s {validationOutputList = a} :: GetAppValidationOutputResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getAppValidationOutputResponse_httpStatus :: Lens.Lens' GetAppValidationOutputResponse Prelude.Int
getAppValidationOutputResponse_httpStatus = Lens.lens (\GetAppValidationOutputResponse' {httpStatus} -> httpStatus) (\s@GetAppValidationOutputResponse' {} a -> s {httpStatus = a} :: GetAppValidationOutputResponse)

instance
  Prelude.NFData
    GetAppValidationOutputResponse
  where
  rnf GetAppValidationOutputResponse' {..} =
    Prelude.rnf validationOutputList
      `Prelude.seq` Prelude.rnf httpStatus
