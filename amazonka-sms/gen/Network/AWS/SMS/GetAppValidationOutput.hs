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
-- Module      : Network.AWS.SMS.GetAppValidationOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves output from validating an application.
module Network.AWS.SMS.GetAppValidationOutput
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAppValidationOutputResponse'
            Prelude.<$> ( x Core..?> "validationOutputList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAppValidationOutput

instance Prelude.NFData GetAppValidationOutput

instance Core.ToHeaders GetAppValidationOutput where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSServerMigrationService_V2016_10_24.GetAppValidationOutput" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetAppValidationOutput where
  toJSON GetAppValidationOutput' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("appId" Core..= appId)]
      )

instance Core.ToPath GetAppValidationOutput where
  toPath = Prelude.const "/"

instance Core.ToQuery GetAppValidationOutput where
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
getAppValidationOutputResponse_validationOutputList = Lens.lens (\GetAppValidationOutputResponse' {validationOutputList} -> validationOutputList) (\s@GetAppValidationOutputResponse' {} a -> s {validationOutputList = a} :: GetAppValidationOutputResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getAppValidationOutputResponse_httpStatus :: Lens.Lens' GetAppValidationOutputResponse Prelude.Int
getAppValidationOutputResponse_httpStatus = Lens.lens (\GetAppValidationOutputResponse' {httpStatus} -> httpStatus) (\s@GetAppValidationOutputResponse' {} a -> s {httpStatus = a} :: GetAppValidationOutputResponse)

instance
  Prelude.NFData
    GetAppValidationOutputResponse
