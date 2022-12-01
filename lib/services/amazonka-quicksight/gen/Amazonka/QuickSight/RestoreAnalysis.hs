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
-- Module      : Amazonka.QuickSight.RestoreAnalysis
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores an analysis.
module Amazonka.QuickSight.RestoreAnalysis
  ( -- * Creating a Request
    RestoreAnalysis (..),
    newRestoreAnalysis,

    -- * Request Lenses
    restoreAnalysis_awsAccountId,
    restoreAnalysis_analysisId,

    -- * Destructuring the Response
    RestoreAnalysisResponse (..),
    newRestoreAnalysisResponse,

    -- * Response Lenses
    restoreAnalysisResponse_analysisId,
    restoreAnalysisResponse_requestId,
    restoreAnalysisResponse_arn,
    restoreAnalysisResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRestoreAnalysis' smart constructor.
data RestoreAnalysis = RestoreAnalysis'
  { -- | The ID of the Amazon Web Services account that contains the analysis.
    awsAccountId :: Prelude.Text,
    -- | The ID of the analysis that you\'re restoring.
    analysisId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreAnalysis' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'restoreAnalysis_awsAccountId' - The ID of the Amazon Web Services account that contains the analysis.
--
-- 'analysisId', 'restoreAnalysis_analysisId' - The ID of the analysis that you\'re restoring.
newRestoreAnalysis ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'analysisId'
  Prelude.Text ->
  RestoreAnalysis
newRestoreAnalysis pAwsAccountId_ pAnalysisId_ =
  RestoreAnalysis'
    { awsAccountId = pAwsAccountId_,
      analysisId = pAnalysisId_
    }

-- | The ID of the Amazon Web Services account that contains the analysis.
restoreAnalysis_awsAccountId :: Lens.Lens' RestoreAnalysis Prelude.Text
restoreAnalysis_awsAccountId = Lens.lens (\RestoreAnalysis' {awsAccountId} -> awsAccountId) (\s@RestoreAnalysis' {} a -> s {awsAccountId = a} :: RestoreAnalysis)

-- | The ID of the analysis that you\'re restoring.
restoreAnalysis_analysisId :: Lens.Lens' RestoreAnalysis Prelude.Text
restoreAnalysis_analysisId = Lens.lens (\RestoreAnalysis' {analysisId} -> analysisId) (\s@RestoreAnalysis' {} a -> s {analysisId = a} :: RestoreAnalysis)

instance Core.AWSRequest RestoreAnalysis where
  type
    AWSResponse RestoreAnalysis =
      RestoreAnalysisResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RestoreAnalysisResponse'
            Prelude.<$> (x Core..?> "AnalysisId")
            Prelude.<*> (x Core..?> "RequestId")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RestoreAnalysis where
  hashWithSalt _salt RestoreAnalysis' {..} =
    _salt `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` analysisId

instance Prelude.NFData RestoreAnalysis where
  rnf RestoreAnalysis' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf analysisId

instance Core.ToHeaders RestoreAnalysis where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RestoreAnalysis where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath RestoreAnalysis where
  toPath RestoreAnalysis' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Core.toBS awsAccountId,
        "/restore/analyses/",
        Core.toBS analysisId
      ]

instance Core.ToQuery RestoreAnalysis where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRestoreAnalysisResponse' smart constructor.
data RestoreAnalysisResponse = RestoreAnalysisResponse'
  { -- | The ID of the analysis that you\'re restoring.
    analysisId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the analysis that you\'re restoring.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreAnalysisResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analysisId', 'restoreAnalysisResponse_analysisId' - The ID of the analysis that you\'re restoring.
--
-- 'requestId', 'restoreAnalysisResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'arn', 'restoreAnalysisResponse_arn' - The Amazon Resource Name (ARN) of the analysis that you\'re restoring.
--
-- 'status', 'restoreAnalysisResponse_status' - The HTTP status of the request.
newRestoreAnalysisResponse ::
  -- | 'status'
  Prelude.Int ->
  RestoreAnalysisResponse
newRestoreAnalysisResponse pStatus_ =
  RestoreAnalysisResponse'
    { analysisId =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      arn = Prelude.Nothing,
      status = pStatus_
    }

-- | The ID of the analysis that you\'re restoring.
restoreAnalysisResponse_analysisId :: Lens.Lens' RestoreAnalysisResponse (Prelude.Maybe Prelude.Text)
restoreAnalysisResponse_analysisId = Lens.lens (\RestoreAnalysisResponse' {analysisId} -> analysisId) (\s@RestoreAnalysisResponse' {} a -> s {analysisId = a} :: RestoreAnalysisResponse)

-- | The Amazon Web Services request ID for this operation.
restoreAnalysisResponse_requestId :: Lens.Lens' RestoreAnalysisResponse (Prelude.Maybe Prelude.Text)
restoreAnalysisResponse_requestId = Lens.lens (\RestoreAnalysisResponse' {requestId} -> requestId) (\s@RestoreAnalysisResponse' {} a -> s {requestId = a} :: RestoreAnalysisResponse)

-- | The Amazon Resource Name (ARN) of the analysis that you\'re restoring.
restoreAnalysisResponse_arn :: Lens.Lens' RestoreAnalysisResponse (Prelude.Maybe Prelude.Text)
restoreAnalysisResponse_arn = Lens.lens (\RestoreAnalysisResponse' {arn} -> arn) (\s@RestoreAnalysisResponse' {} a -> s {arn = a} :: RestoreAnalysisResponse)

-- | The HTTP status of the request.
restoreAnalysisResponse_status :: Lens.Lens' RestoreAnalysisResponse Prelude.Int
restoreAnalysisResponse_status = Lens.lens (\RestoreAnalysisResponse' {status} -> status) (\s@RestoreAnalysisResponse' {} a -> s {status = a} :: RestoreAnalysisResponse)

instance Prelude.NFData RestoreAnalysisResponse where
  rnf RestoreAnalysisResponse' {..} =
    Prelude.rnf analysisId
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf status
