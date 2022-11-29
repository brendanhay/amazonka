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
-- Module      : Amazonka.IAM.GenerateCredentialReport
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a credential report for the Amazon Web Services account. For
-- more information about the credential report, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/credential-reports.html Getting credential reports>
-- in the /IAM User Guide/.
module Amazonka.IAM.GenerateCredentialReport
  ( -- * Creating a Request
    GenerateCredentialReport (..),
    newGenerateCredentialReport,

    -- * Destructuring the Response
    GenerateCredentialReportResponse (..),
    newGenerateCredentialReportResponse,

    -- * Response Lenses
    generateCredentialReportResponse_state,
    generateCredentialReportResponse_description,
    generateCredentialReportResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGenerateCredentialReport' smart constructor.
data GenerateCredentialReport = GenerateCredentialReport'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GenerateCredentialReport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGenerateCredentialReport ::
  GenerateCredentialReport
newGenerateCredentialReport =
  GenerateCredentialReport'

instance Core.AWSRequest GenerateCredentialReport where
  type
    AWSResponse GenerateCredentialReport =
      GenerateCredentialReportResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GenerateCredentialReportResult"
      ( \s h x ->
          GenerateCredentialReportResponse'
            Prelude.<$> (x Core..@? "State")
            Prelude.<*> (x Core..@? "Description")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GenerateCredentialReport where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GenerateCredentialReport where
  rnf _ = ()

instance Core.ToHeaders GenerateCredentialReport where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GenerateCredentialReport where
  toPath = Prelude.const "/"

instance Core.ToQuery GenerateCredentialReport where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          [ "Action"
              Core.=: ("GenerateCredentialReport" :: Prelude.ByteString),
            "Version"
              Core.=: ("2010-05-08" :: Prelude.ByteString)
          ]
      )

-- | Contains the response to a successful GenerateCredentialReport request.
--
-- /See:/ 'newGenerateCredentialReportResponse' smart constructor.
data GenerateCredentialReportResponse = GenerateCredentialReportResponse'
  { -- | Information about the state of the credential report.
    state :: Prelude.Maybe ReportStateType,
    -- | Information about the credential report.
    description :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GenerateCredentialReportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'generateCredentialReportResponse_state' - Information about the state of the credential report.
--
-- 'description', 'generateCredentialReportResponse_description' - Information about the credential report.
--
-- 'httpStatus', 'generateCredentialReportResponse_httpStatus' - The response's http status code.
newGenerateCredentialReportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GenerateCredentialReportResponse
newGenerateCredentialReportResponse pHttpStatus_ =
  GenerateCredentialReportResponse'
    { state =
        Prelude.Nothing,
      description = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the state of the credential report.
generateCredentialReportResponse_state :: Lens.Lens' GenerateCredentialReportResponse (Prelude.Maybe ReportStateType)
generateCredentialReportResponse_state = Lens.lens (\GenerateCredentialReportResponse' {state} -> state) (\s@GenerateCredentialReportResponse' {} a -> s {state = a} :: GenerateCredentialReportResponse)

-- | Information about the credential report.
generateCredentialReportResponse_description :: Lens.Lens' GenerateCredentialReportResponse (Prelude.Maybe Prelude.Text)
generateCredentialReportResponse_description = Lens.lens (\GenerateCredentialReportResponse' {description} -> description) (\s@GenerateCredentialReportResponse' {} a -> s {description = a} :: GenerateCredentialReportResponse)

-- | The response's http status code.
generateCredentialReportResponse_httpStatus :: Lens.Lens' GenerateCredentialReportResponse Prelude.Int
generateCredentialReportResponse_httpStatus = Lens.lens (\GenerateCredentialReportResponse' {httpStatus} -> httpStatus) (\s@GenerateCredentialReportResponse' {} a -> s {httpStatus = a} :: GenerateCredentialReportResponse)

instance
  Prelude.NFData
    GenerateCredentialReportResponse
  where
  rnf GenerateCredentialReportResponse' {..} =
    Prelude.rnf state
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf httpStatus
