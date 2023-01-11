{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodePipeline.Types.ApprovalResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.ApprovalResult where

import Amazonka.CodePipeline.Types.ApprovalStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents information about the result of an approval request.
--
-- /See:/ 'newApprovalResult' smart constructor.
data ApprovalResult = ApprovalResult'
  { -- | The summary of the current status of the approval request.
    summary :: Prelude.Text,
    -- | The response submitted by a reviewer assigned to an approval action
    -- request.
    status :: ApprovalStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApprovalResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'summary', 'approvalResult_summary' - The summary of the current status of the approval request.
--
-- 'status', 'approvalResult_status' - The response submitted by a reviewer assigned to an approval action
-- request.
newApprovalResult ::
  -- | 'summary'
  Prelude.Text ->
  -- | 'status'
  ApprovalStatus ->
  ApprovalResult
newApprovalResult pSummary_ pStatus_ =
  ApprovalResult'
    { summary = pSummary_,
      status = pStatus_
    }

-- | The summary of the current status of the approval request.
approvalResult_summary :: Lens.Lens' ApprovalResult Prelude.Text
approvalResult_summary = Lens.lens (\ApprovalResult' {summary} -> summary) (\s@ApprovalResult' {} a -> s {summary = a} :: ApprovalResult)

-- | The response submitted by a reviewer assigned to an approval action
-- request.
approvalResult_status :: Lens.Lens' ApprovalResult ApprovalStatus
approvalResult_status = Lens.lens (\ApprovalResult' {status} -> status) (\s@ApprovalResult' {} a -> s {status = a} :: ApprovalResult)

instance Prelude.Hashable ApprovalResult where
  hashWithSalt _salt ApprovalResult' {..} =
    _salt `Prelude.hashWithSalt` summary
      `Prelude.hashWithSalt` status

instance Prelude.NFData ApprovalResult where
  rnf ApprovalResult' {..} =
    Prelude.rnf summary
      `Prelude.seq` Prelude.rnf status

instance Data.ToJSON ApprovalResult where
  toJSON ApprovalResult' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("summary" Data..= summary),
            Prelude.Just ("status" Data..= status)
          ]
      )
