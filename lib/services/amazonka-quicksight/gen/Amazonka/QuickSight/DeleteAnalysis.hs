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
-- Module      : Amazonka.QuickSight.DeleteAnalysis
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an analysis from Amazon QuickSight. You can optionally include a
-- recovery window during which you can restore the analysis. If you don\'t
-- specify a recovery window value, the operation defaults to 30 days.
-- Amazon QuickSight attaches a @DeletionTime@ stamp to the response that
-- specifies the end of the recovery window. At the end of the recovery
-- window, Amazon QuickSight deletes the analysis permanently.
--
-- At any time before recovery window ends, you can use the
-- @RestoreAnalysis@ API operation to remove the @DeletionTime@ stamp and
-- cancel the deletion of the analysis. The analysis remains visible in the
-- API until it\'s deleted, so you can describe it but you can\'t make a
-- template from it.
--
-- An analysis that\'s scheduled for deletion isn\'t accessible in the
-- Amazon QuickSight console. To access it in the console, restore it.
-- Deleting an analysis doesn\'t delete the dashboards that you publish
-- from it.
module Amazonka.QuickSight.DeleteAnalysis
  ( -- * Creating a Request
    DeleteAnalysis (..),
    newDeleteAnalysis,

    -- * Request Lenses
    deleteAnalysis_recoveryWindowInDays,
    deleteAnalysis_forceDeleteWithoutRecovery,
    deleteAnalysis_awsAccountId,
    deleteAnalysis_analysisId,

    -- * Destructuring the Response
    DeleteAnalysisResponse (..),
    newDeleteAnalysisResponse,

    -- * Response Lenses
    deleteAnalysisResponse_deletionTime,
    deleteAnalysisResponse_analysisId,
    deleteAnalysisResponse_requestId,
    deleteAnalysisResponse_arn,
    deleteAnalysisResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAnalysis' smart constructor.
data DeleteAnalysis = DeleteAnalysis'
  { -- | A value that specifies the number of days that Amazon QuickSight waits
    -- before it deletes the analysis. You can\'t use this parameter with the
    -- @ForceDeleteWithoutRecovery@ option in the same API call. The default
    -- value is 30.
    recoveryWindowInDays :: Prelude.Maybe Prelude.Natural,
    -- | This option defaults to the value @NoForceDeleteWithoutRecovery@. To
    -- immediately delete the analysis, add the @ForceDeleteWithoutRecovery@
    -- option. You can\'t restore an analysis after it\'s deleted.
    forceDeleteWithoutRecovery :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Amazon Web Services account where you want to delete an
    -- analysis.
    awsAccountId :: Prelude.Text,
    -- | The ID of the analysis that you\'re deleting.
    analysisId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAnalysis' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recoveryWindowInDays', 'deleteAnalysis_recoveryWindowInDays' - A value that specifies the number of days that Amazon QuickSight waits
-- before it deletes the analysis. You can\'t use this parameter with the
-- @ForceDeleteWithoutRecovery@ option in the same API call. The default
-- value is 30.
--
-- 'forceDeleteWithoutRecovery', 'deleteAnalysis_forceDeleteWithoutRecovery' - This option defaults to the value @NoForceDeleteWithoutRecovery@. To
-- immediately delete the analysis, add the @ForceDeleteWithoutRecovery@
-- option. You can\'t restore an analysis after it\'s deleted.
--
-- 'awsAccountId', 'deleteAnalysis_awsAccountId' - The ID of the Amazon Web Services account where you want to delete an
-- analysis.
--
-- 'analysisId', 'deleteAnalysis_analysisId' - The ID of the analysis that you\'re deleting.
newDeleteAnalysis ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'analysisId'
  Prelude.Text ->
  DeleteAnalysis
newDeleteAnalysis pAwsAccountId_ pAnalysisId_ =
  DeleteAnalysis'
    { recoveryWindowInDays =
        Prelude.Nothing,
      forceDeleteWithoutRecovery = Prelude.Nothing,
      awsAccountId = pAwsAccountId_,
      analysisId = pAnalysisId_
    }

-- | A value that specifies the number of days that Amazon QuickSight waits
-- before it deletes the analysis. You can\'t use this parameter with the
-- @ForceDeleteWithoutRecovery@ option in the same API call. The default
-- value is 30.
deleteAnalysis_recoveryWindowInDays :: Lens.Lens' DeleteAnalysis (Prelude.Maybe Prelude.Natural)
deleteAnalysis_recoveryWindowInDays = Lens.lens (\DeleteAnalysis' {recoveryWindowInDays} -> recoveryWindowInDays) (\s@DeleteAnalysis' {} a -> s {recoveryWindowInDays = a} :: DeleteAnalysis)

-- | This option defaults to the value @NoForceDeleteWithoutRecovery@. To
-- immediately delete the analysis, add the @ForceDeleteWithoutRecovery@
-- option. You can\'t restore an analysis after it\'s deleted.
deleteAnalysis_forceDeleteWithoutRecovery :: Lens.Lens' DeleteAnalysis (Prelude.Maybe Prelude.Bool)
deleteAnalysis_forceDeleteWithoutRecovery = Lens.lens (\DeleteAnalysis' {forceDeleteWithoutRecovery} -> forceDeleteWithoutRecovery) (\s@DeleteAnalysis' {} a -> s {forceDeleteWithoutRecovery = a} :: DeleteAnalysis)

-- | The ID of the Amazon Web Services account where you want to delete an
-- analysis.
deleteAnalysis_awsAccountId :: Lens.Lens' DeleteAnalysis Prelude.Text
deleteAnalysis_awsAccountId = Lens.lens (\DeleteAnalysis' {awsAccountId} -> awsAccountId) (\s@DeleteAnalysis' {} a -> s {awsAccountId = a} :: DeleteAnalysis)

-- | The ID of the analysis that you\'re deleting.
deleteAnalysis_analysisId :: Lens.Lens' DeleteAnalysis Prelude.Text
deleteAnalysis_analysisId = Lens.lens (\DeleteAnalysis' {analysisId} -> analysisId) (\s@DeleteAnalysis' {} a -> s {analysisId = a} :: DeleteAnalysis)

instance Core.AWSRequest DeleteAnalysis where
  type
    AWSResponse DeleteAnalysis =
      DeleteAnalysisResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteAnalysisResponse'
            Prelude.<$> (x Core..?> "DeletionTime")
            Prelude.<*> (x Core..?> "AnalysisId")
            Prelude.<*> (x Core..?> "RequestId")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAnalysis where
  hashWithSalt _salt DeleteAnalysis' {..} =
    _salt `Prelude.hashWithSalt` recoveryWindowInDays
      `Prelude.hashWithSalt` forceDeleteWithoutRecovery
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` analysisId

instance Prelude.NFData DeleteAnalysis where
  rnf DeleteAnalysis' {..} =
    Prelude.rnf recoveryWindowInDays
      `Prelude.seq` Prelude.rnf forceDeleteWithoutRecovery
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf analysisId

instance Core.ToHeaders DeleteAnalysis where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteAnalysis where
  toPath DeleteAnalysis' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Core.toBS awsAccountId,
        "/analyses/",
        Core.toBS analysisId
      ]

instance Core.ToQuery DeleteAnalysis where
  toQuery DeleteAnalysis' {..} =
    Prelude.mconcat
      [ "recovery-window-in-days"
          Core.=: recoveryWindowInDays,
        "force-delete-without-recovery"
          Core.=: forceDeleteWithoutRecovery
      ]

-- | /See:/ 'newDeleteAnalysisResponse' smart constructor.
data DeleteAnalysisResponse = DeleteAnalysisResponse'
  { -- | The date and time that the analysis is scheduled to be deleted.
    deletionTime :: Prelude.Maybe Core.POSIX,
    -- | The ID of the deleted analysis.
    analysisId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the deleted analysis.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAnalysisResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletionTime', 'deleteAnalysisResponse_deletionTime' - The date and time that the analysis is scheduled to be deleted.
--
-- 'analysisId', 'deleteAnalysisResponse_analysisId' - The ID of the deleted analysis.
--
-- 'requestId', 'deleteAnalysisResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'arn', 'deleteAnalysisResponse_arn' - The Amazon Resource Name (ARN) of the deleted analysis.
--
-- 'status', 'deleteAnalysisResponse_status' - The HTTP status of the request.
newDeleteAnalysisResponse ::
  -- | 'status'
  Prelude.Int ->
  DeleteAnalysisResponse
newDeleteAnalysisResponse pStatus_ =
  DeleteAnalysisResponse'
    { deletionTime =
        Prelude.Nothing,
      analysisId = Prelude.Nothing,
      requestId = Prelude.Nothing,
      arn = Prelude.Nothing,
      status = pStatus_
    }

-- | The date and time that the analysis is scheduled to be deleted.
deleteAnalysisResponse_deletionTime :: Lens.Lens' DeleteAnalysisResponse (Prelude.Maybe Prelude.UTCTime)
deleteAnalysisResponse_deletionTime = Lens.lens (\DeleteAnalysisResponse' {deletionTime} -> deletionTime) (\s@DeleteAnalysisResponse' {} a -> s {deletionTime = a} :: DeleteAnalysisResponse) Prelude.. Lens.mapping Core._Time

-- | The ID of the deleted analysis.
deleteAnalysisResponse_analysisId :: Lens.Lens' DeleteAnalysisResponse (Prelude.Maybe Prelude.Text)
deleteAnalysisResponse_analysisId = Lens.lens (\DeleteAnalysisResponse' {analysisId} -> analysisId) (\s@DeleteAnalysisResponse' {} a -> s {analysisId = a} :: DeleteAnalysisResponse)

-- | The Amazon Web Services request ID for this operation.
deleteAnalysisResponse_requestId :: Lens.Lens' DeleteAnalysisResponse (Prelude.Maybe Prelude.Text)
deleteAnalysisResponse_requestId = Lens.lens (\DeleteAnalysisResponse' {requestId} -> requestId) (\s@DeleteAnalysisResponse' {} a -> s {requestId = a} :: DeleteAnalysisResponse)

-- | The Amazon Resource Name (ARN) of the deleted analysis.
deleteAnalysisResponse_arn :: Lens.Lens' DeleteAnalysisResponse (Prelude.Maybe Prelude.Text)
deleteAnalysisResponse_arn = Lens.lens (\DeleteAnalysisResponse' {arn} -> arn) (\s@DeleteAnalysisResponse' {} a -> s {arn = a} :: DeleteAnalysisResponse)

-- | The HTTP status of the request.
deleteAnalysisResponse_status :: Lens.Lens' DeleteAnalysisResponse Prelude.Int
deleteAnalysisResponse_status = Lens.lens (\DeleteAnalysisResponse' {status} -> status) (\s@DeleteAnalysisResponse' {} a -> s {status = a} :: DeleteAnalysisResponse)

instance Prelude.NFData DeleteAnalysisResponse where
  rnf DeleteAnalysisResponse' {..} =
    Prelude.rnf deletionTime
      `Prelude.seq` Prelude.rnf analysisId
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf status
