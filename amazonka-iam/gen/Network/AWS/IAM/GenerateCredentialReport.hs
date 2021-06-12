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
-- Module      : Network.AWS.IAM.GenerateCredentialReport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a credential report for the AWS account. For more information
-- about the credential report, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/credential-reports.html Getting credential reports>
-- in the /IAM User Guide/.
module Network.AWS.IAM.GenerateCredentialReport
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

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGenerateCredentialReport' smart constructor.
data GenerateCredentialReport = GenerateCredentialReport'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GenerateCredentialReportResult"
      ( \s h x ->
          GenerateCredentialReportResponse'
            Core.<$> (x Core..@? "State")
            Core.<*> (x Core..@? "Description")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GenerateCredentialReport

instance Core.NFData GenerateCredentialReport

instance Core.ToHeaders GenerateCredentialReport where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GenerateCredentialReport where
  toPath = Core.const "/"

instance Core.ToQuery GenerateCredentialReport where
  toQuery =
    Core.const
      ( Core.mconcat
          [ "Action"
              Core.=: ("GenerateCredentialReport" :: Core.ByteString),
            "Version" Core.=: ("2010-05-08" :: Core.ByteString)
          ]
      )

-- | Contains the response to a successful GenerateCredentialReport request.
--
-- /See:/ 'newGenerateCredentialReportResponse' smart constructor.
data GenerateCredentialReportResponse = GenerateCredentialReportResponse'
  { -- | Information about the state of the credential report.
    state :: Core.Maybe ReportStateType,
    -- | Information about the credential report.
    description :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GenerateCredentialReportResponse
newGenerateCredentialReportResponse pHttpStatus_ =
  GenerateCredentialReportResponse'
    { state =
        Core.Nothing,
      description = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the state of the credential report.
generateCredentialReportResponse_state :: Lens.Lens' GenerateCredentialReportResponse (Core.Maybe ReportStateType)
generateCredentialReportResponse_state = Lens.lens (\GenerateCredentialReportResponse' {state} -> state) (\s@GenerateCredentialReportResponse' {} a -> s {state = a} :: GenerateCredentialReportResponse)

-- | Information about the credential report.
generateCredentialReportResponse_description :: Lens.Lens' GenerateCredentialReportResponse (Core.Maybe Core.Text)
generateCredentialReportResponse_description = Lens.lens (\GenerateCredentialReportResponse' {description} -> description) (\s@GenerateCredentialReportResponse' {} a -> s {description = a} :: GenerateCredentialReportResponse)

-- | The response's http status code.
generateCredentialReportResponse_httpStatus :: Lens.Lens' GenerateCredentialReportResponse Core.Int
generateCredentialReportResponse_httpStatus = Lens.lens (\GenerateCredentialReportResponse' {httpStatus} -> httpStatus) (\s@GenerateCredentialReportResponse' {} a -> s {httpStatus = a} :: GenerateCredentialReportResponse)

instance Core.NFData GenerateCredentialReportResponse
