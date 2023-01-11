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
-- Module      : Amazonka.QuickSight.DescribeAnalysis
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a summary of the metadata for an analysis.
module Amazonka.QuickSight.DescribeAnalysis
  ( -- * Creating a Request
    DescribeAnalysis (..),
    newDescribeAnalysis,

    -- * Request Lenses
    describeAnalysis_awsAccountId,
    describeAnalysis_analysisId,

    -- * Destructuring the Response
    DescribeAnalysisResponse (..),
    newDescribeAnalysisResponse,

    -- * Response Lenses
    describeAnalysisResponse_analysis,
    describeAnalysisResponse_requestId,
    describeAnalysisResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAnalysis' smart constructor.
data DescribeAnalysis = DescribeAnalysis'
  { -- | The ID of the Amazon Web Services account that contains the analysis.
    -- You must be using the Amazon Web Services account that the analysis is
    -- in.
    awsAccountId :: Prelude.Text,
    -- | The ID of the analysis that you\'re describing. The ID is part of the
    -- URL of the analysis.
    analysisId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAnalysis' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'describeAnalysis_awsAccountId' - The ID of the Amazon Web Services account that contains the analysis.
-- You must be using the Amazon Web Services account that the analysis is
-- in.
--
-- 'analysisId', 'describeAnalysis_analysisId' - The ID of the analysis that you\'re describing. The ID is part of the
-- URL of the analysis.
newDescribeAnalysis ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'analysisId'
  Prelude.Text ->
  DescribeAnalysis
newDescribeAnalysis pAwsAccountId_ pAnalysisId_ =
  DescribeAnalysis'
    { awsAccountId = pAwsAccountId_,
      analysisId = pAnalysisId_
    }

-- | The ID of the Amazon Web Services account that contains the analysis.
-- You must be using the Amazon Web Services account that the analysis is
-- in.
describeAnalysis_awsAccountId :: Lens.Lens' DescribeAnalysis Prelude.Text
describeAnalysis_awsAccountId = Lens.lens (\DescribeAnalysis' {awsAccountId} -> awsAccountId) (\s@DescribeAnalysis' {} a -> s {awsAccountId = a} :: DescribeAnalysis)

-- | The ID of the analysis that you\'re describing. The ID is part of the
-- URL of the analysis.
describeAnalysis_analysisId :: Lens.Lens' DescribeAnalysis Prelude.Text
describeAnalysis_analysisId = Lens.lens (\DescribeAnalysis' {analysisId} -> analysisId) (\s@DescribeAnalysis' {} a -> s {analysisId = a} :: DescribeAnalysis)

instance Core.AWSRequest DescribeAnalysis where
  type
    AWSResponse DescribeAnalysis =
      DescribeAnalysisResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAnalysisResponse'
            Prelude.<$> (x Data..?> "Analysis")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAnalysis where
  hashWithSalt _salt DescribeAnalysis' {..} =
    _salt `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` analysisId

instance Prelude.NFData DescribeAnalysis where
  rnf DescribeAnalysis' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf analysisId

instance Data.ToHeaders DescribeAnalysis where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeAnalysis where
  toPath DescribeAnalysis' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/analyses/",
        Data.toBS analysisId
      ]

instance Data.ToQuery DescribeAnalysis where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAnalysisResponse' smart constructor.
data DescribeAnalysisResponse = DescribeAnalysisResponse'
  { -- | A metadata structure that contains summary information for the analysis
    -- that you\'re describing.
    analysis :: Prelude.Maybe Analysis,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAnalysisResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analysis', 'describeAnalysisResponse_analysis' - A metadata structure that contains summary information for the analysis
-- that you\'re describing.
--
-- 'requestId', 'describeAnalysisResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'describeAnalysisResponse_status' - The HTTP status of the request.
newDescribeAnalysisResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeAnalysisResponse
newDescribeAnalysisResponse pStatus_ =
  DescribeAnalysisResponse'
    { analysis =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | A metadata structure that contains summary information for the analysis
-- that you\'re describing.
describeAnalysisResponse_analysis :: Lens.Lens' DescribeAnalysisResponse (Prelude.Maybe Analysis)
describeAnalysisResponse_analysis = Lens.lens (\DescribeAnalysisResponse' {analysis} -> analysis) (\s@DescribeAnalysisResponse' {} a -> s {analysis = a} :: DescribeAnalysisResponse)

-- | The Amazon Web Services request ID for this operation.
describeAnalysisResponse_requestId :: Lens.Lens' DescribeAnalysisResponse (Prelude.Maybe Prelude.Text)
describeAnalysisResponse_requestId = Lens.lens (\DescribeAnalysisResponse' {requestId} -> requestId) (\s@DescribeAnalysisResponse' {} a -> s {requestId = a} :: DescribeAnalysisResponse)

-- | The HTTP status of the request.
describeAnalysisResponse_status :: Lens.Lens' DescribeAnalysisResponse Prelude.Int
describeAnalysisResponse_status = Lens.lens (\DescribeAnalysisResponse' {status} -> status) (\s@DescribeAnalysisResponse' {} a -> s {status = a} :: DescribeAnalysisResponse)

instance Prelude.NFData DescribeAnalysisResponse where
  rnf DescribeAnalysisResponse' {..} =
    Prelude.rnf analysis
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
