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
-- Module      : Amazonka.Athena.GetCalculationExecutionCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a pre-signed URL to a copy of the code that was executed for
-- the calculation.
module Amazonka.Athena.GetCalculationExecutionCode
  ( -- * Creating a Request
    GetCalculationExecutionCode (..),
    newGetCalculationExecutionCode,

    -- * Request Lenses
    getCalculationExecutionCode_calculationExecutionId,

    -- * Destructuring the Response
    GetCalculationExecutionCodeResponse (..),
    newGetCalculationExecutionCodeResponse,

    -- * Response Lenses
    getCalculationExecutionCodeResponse_codeBlock,
    getCalculationExecutionCodeResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCalculationExecutionCode' smart constructor.
data GetCalculationExecutionCode = GetCalculationExecutionCode'
  { -- | The calculation execution UUID.
    calculationExecutionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCalculationExecutionCode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'calculationExecutionId', 'getCalculationExecutionCode_calculationExecutionId' - The calculation execution UUID.
newGetCalculationExecutionCode ::
  -- | 'calculationExecutionId'
  Prelude.Text ->
  GetCalculationExecutionCode
newGetCalculationExecutionCode
  pCalculationExecutionId_ =
    GetCalculationExecutionCode'
      { calculationExecutionId =
          pCalculationExecutionId_
      }

-- | The calculation execution UUID.
getCalculationExecutionCode_calculationExecutionId :: Lens.Lens' GetCalculationExecutionCode Prelude.Text
getCalculationExecutionCode_calculationExecutionId = Lens.lens (\GetCalculationExecutionCode' {calculationExecutionId} -> calculationExecutionId) (\s@GetCalculationExecutionCode' {} a -> s {calculationExecutionId = a} :: GetCalculationExecutionCode)

instance Core.AWSRequest GetCalculationExecutionCode where
  type
    AWSResponse GetCalculationExecutionCode =
      GetCalculationExecutionCodeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCalculationExecutionCodeResponse'
            Prelude.<$> (x Data..?> "CodeBlock")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCalculationExecutionCode where
  hashWithSalt _salt GetCalculationExecutionCode' {..} =
    _salt `Prelude.hashWithSalt` calculationExecutionId

instance Prelude.NFData GetCalculationExecutionCode where
  rnf GetCalculationExecutionCode' {..} =
    Prelude.rnf calculationExecutionId

instance Data.ToHeaders GetCalculationExecutionCode where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.GetCalculationExecutionCode" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetCalculationExecutionCode where
  toJSON GetCalculationExecutionCode' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "CalculationExecutionId"
                  Data..= calculationExecutionId
              )
          ]
      )

instance Data.ToPath GetCalculationExecutionCode where
  toPath = Prelude.const "/"

instance Data.ToQuery GetCalculationExecutionCode where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCalculationExecutionCodeResponse' smart constructor.
data GetCalculationExecutionCodeResponse = GetCalculationExecutionCodeResponse'
  { -- | A pre-signed URL to the code that executed the calculation.
    codeBlock :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCalculationExecutionCodeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeBlock', 'getCalculationExecutionCodeResponse_codeBlock' - A pre-signed URL to the code that executed the calculation.
--
-- 'httpStatus', 'getCalculationExecutionCodeResponse_httpStatus' - The response's http status code.
newGetCalculationExecutionCodeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCalculationExecutionCodeResponse
newGetCalculationExecutionCodeResponse pHttpStatus_ =
  GetCalculationExecutionCodeResponse'
    { codeBlock =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A pre-signed URL to the code that executed the calculation.
getCalculationExecutionCodeResponse_codeBlock :: Lens.Lens' GetCalculationExecutionCodeResponse (Prelude.Maybe Prelude.Text)
getCalculationExecutionCodeResponse_codeBlock = Lens.lens (\GetCalculationExecutionCodeResponse' {codeBlock} -> codeBlock) (\s@GetCalculationExecutionCodeResponse' {} a -> s {codeBlock = a} :: GetCalculationExecutionCodeResponse)

-- | The response's http status code.
getCalculationExecutionCodeResponse_httpStatus :: Lens.Lens' GetCalculationExecutionCodeResponse Prelude.Int
getCalculationExecutionCodeResponse_httpStatus = Lens.lens (\GetCalculationExecutionCodeResponse' {httpStatus} -> httpStatus) (\s@GetCalculationExecutionCodeResponse' {} a -> s {httpStatus = a} :: GetCalculationExecutionCodeResponse)

instance
  Prelude.NFData
    GetCalculationExecutionCodeResponse
  where
  rnf GetCalculationExecutionCodeResponse' {..} =
    Prelude.rnf codeBlock
      `Prelude.seq` Prelude.rnf httpStatus
