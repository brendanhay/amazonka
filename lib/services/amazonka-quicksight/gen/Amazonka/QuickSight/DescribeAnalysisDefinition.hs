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
-- Module      : Amazonka.QuickSight.DescribeAnalysisDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a detailed description of the definition of an analysis.
--
-- If you do not need to know details about the content of an Analysis, for
-- instance if you are trying to check the status of a recently created or
-- updated Analysis, use the
-- <https://docs.aws.amazon.com/quicksight/latest/APIReference/API_DescribeAnalysis.html DescribeAnalysis>
-- instead.
module Amazonka.QuickSight.DescribeAnalysisDefinition
  ( -- * Creating a Request
    DescribeAnalysisDefinition (..),
    newDescribeAnalysisDefinition,

    -- * Request Lenses
    describeAnalysisDefinition_awsAccountId,
    describeAnalysisDefinition_analysisId,

    -- * Destructuring the Response
    DescribeAnalysisDefinitionResponse (..),
    newDescribeAnalysisDefinitionResponse,

    -- * Response Lenses
    describeAnalysisDefinitionResponse_analysisId,
    describeAnalysisDefinitionResponse_definition,
    describeAnalysisDefinitionResponse_errors,
    describeAnalysisDefinitionResponse_name,
    describeAnalysisDefinitionResponse_requestId,
    describeAnalysisDefinitionResponse_resourceStatus,
    describeAnalysisDefinitionResponse_themeArn,
    describeAnalysisDefinitionResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAnalysisDefinition' smart constructor.
data DescribeAnalysisDefinition = DescribeAnalysisDefinition'
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
-- Create a value of 'DescribeAnalysisDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'describeAnalysisDefinition_awsAccountId' - The ID of the Amazon Web Services account that contains the analysis.
-- You must be using the Amazon Web Services account that the analysis is
-- in.
--
-- 'analysisId', 'describeAnalysisDefinition_analysisId' - The ID of the analysis that you\'re describing. The ID is part of the
-- URL of the analysis.
newDescribeAnalysisDefinition ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'analysisId'
  Prelude.Text ->
  DescribeAnalysisDefinition
newDescribeAnalysisDefinition
  pAwsAccountId_
  pAnalysisId_ =
    DescribeAnalysisDefinition'
      { awsAccountId =
          pAwsAccountId_,
        analysisId = pAnalysisId_
      }

-- | The ID of the Amazon Web Services account that contains the analysis.
-- You must be using the Amazon Web Services account that the analysis is
-- in.
describeAnalysisDefinition_awsAccountId :: Lens.Lens' DescribeAnalysisDefinition Prelude.Text
describeAnalysisDefinition_awsAccountId = Lens.lens (\DescribeAnalysisDefinition' {awsAccountId} -> awsAccountId) (\s@DescribeAnalysisDefinition' {} a -> s {awsAccountId = a} :: DescribeAnalysisDefinition)

-- | The ID of the analysis that you\'re describing. The ID is part of the
-- URL of the analysis.
describeAnalysisDefinition_analysisId :: Lens.Lens' DescribeAnalysisDefinition Prelude.Text
describeAnalysisDefinition_analysisId = Lens.lens (\DescribeAnalysisDefinition' {analysisId} -> analysisId) (\s@DescribeAnalysisDefinition' {} a -> s {analysisId = a} :: DescribeAnalysisDefinition)

instance Core.AWSRequest DescribeAnalysisDefinition where
  type
    AWSResponse DescribeAnalysisDefinition =
      DescribeAnalysisDefinitionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAnalysisDefinitionResponse'
            Prelude.<$> (x Data..?> "AnalysisId")
            Prelude.<*> (x Data..?> "Definition")
            Prelude.<*> (x Data..?> "Errors")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "ResourceStatus")
            Prelude.<*> (x Data..?> "ThemeArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAnalysisDefinition where
  hashWithSalt _salt DescribeAnalysisDefinition' {..} =
    _salt `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` analysisId

instance Prelude.NFData DescribeAnalysisDefinition where
  rnf DescribeAnalysisDefinition' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf analysisId

instance Data.ToHeaders DescribeAnalysisDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeAnalysisDefinition where
  toPath DescribeAnalysisDefinition' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/analyses/",
        Data.toBS analysisId,
        "/definition"
      ]

instance Data.ToQuery DescribeAnalysisDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAnalysisDefinitionResponse' smart constructor.
data DescribeAnalysisDefinitionResponse = DescribeAnalysisDefinitionResponse'
  { -- | The ID of the analysis described.
    analysisId :: Prelude.Maybe Prelude.Text,
    -- | The definition of an analysis.
    --
    -- A definition is the data model of all features in a Dashboard, Template,
    -- or Analysis.
    definition :: Prelude.Maybe AnalysisDefinition,
    -- | Errors associated with the analysis.
    errors :: Prelude.Maybe (Prelude.NonEmpty AnalysisError),
    -- | The descriptive name of the analysis.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | Status associated with the analysis.
    --
    -- -   @CREATION_IN_PROGRESS@
    --
    -- -   @CREATION_SUCCESSFUL@
    --
    -- -   @CREATION_FAILED@
    --
    -- -   @UPDATE_IN_PROGRESS@
    --
    -- -   @UPDATE_SUCCESSFUL@
    --
    -- -   @UPDATE_FAILED@
    --
    -- -   @DELETED@
    resourceStatus :: Prelude.Maybe ResourceStatus,
    -- | The ARN of the theme of the analysis.
    themeArn :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAnalysisDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analysisId', 'describeAnalysisDefinitionResponse_analysisId' - The ID of the analysis described.
--
-- 'definition', 'describeAnalysisDefinitionResponse_definition' - The definition of an analysis.
--
-- A definition is the data model of all features in a Dashboard, Template,
-- or Analysis.
--
-- 'errors', 'describeAnalysisDefinitionResponse_errors' - Errors associated with the analysis.
--
-- 'name', 'describeAnalysisDefinitionResponse_name' - The descriptive name of the analysis.
--
-- 'requestId', 'describeAnalysisDefinitionResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'resourceStatus', 'describeAnalysisDefinitionResponse_resourceStatus' - Status associated with the analysis.
--
-- -   @CREATION_IN_PROGRESS@
--
-- -   @CREATION_SUCCESSFUL@
--
-- -   @CREATION_FAILED@
--
-- -   @UPDATE_IN_PROGRESS@
--
-- -   @UPDATE_SUCCESSFUL@
--
-- -   @UPDATE_FAILED@
--
-- -   @DELETED@
--
-- 'themeArn', 'describeAnalysisDefinitionResponse_themeArn' - The ARN of the theme of the analysis.
--
-- 'status', 'describeAnalysisDefinitionResponse_status' - The HTTP status of the request.
newDescribeAnalysisDefinitionResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeAnalysisDefinitionResponse
newDescribeAnalysisDefinitionResponse pStatus_ =
  DescribeAnalysisDefinitionResponse'
    { analysisId =
        Prelude.Nothing,
      definition = Prelude.Nothing,
      errors = Prelude.Nothing,
      name = Prelude.Nothing,
      requestId = Prelude.Nothing,
      resourceStatus = Prelude.Nothing,
      themeArn = Prelude.Nothing,
      status = pStatus_
    }

-- | The ID of the analysis described.
describeAnalysisDefinitionResponse_analysisId :: Lens.Lens' DescribeAnalysisDefinitionResponse (Prelude.Maybe Prelude.Text)
describeAnalysisDefinitionResponse_analysisId = Lens.lens (\DescribeAnalysisDefinitionResponse' {analysisId} -> analysisId) (\s@DescribeAnalysisDefinitionResponse' {} a -> s {analysisId = a} :: DescribeAnalysisDefinitionResponse)

-- | The definition of an analysis.
--
-- A definition is the data model of all features in a Dashboard, Template,
-- or Analysis.
describeAnalysisDefinitionResponse_definition :: Lens.Lens' DescribeAnalysisDefinitionResponse (Prelude.Maybe AnalysisDefinition)
describeAnalysisDefinitionResponse_definition = Lens.lens (\DescribeAnalysisDefinitionResponse' {definition} -> definition) (\s@DescribeAnalysisDefinitionResponse' {} a -> s {definition = a} :: DescribeAnalysisDefinitionResponse)

-- | Errors associated with the analysis.
describeAnalysisDefinitionResponse_errors :: Lens.Lens' DescribeAnalysisDefinitionResponse (Prelude.Maybe (Prelude.NonEmpty AnalysisError))
describeAnalysisDefinitionResponse_errors = Lens.lens (\DescribeAnalysisDefinitionResponse' {errors} -> errors) (\s@DescribeAnalysisDefinitionResponse' {} a -> s {errors = a} :: DescribeAnalysisDefinitionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The descriptive name of the analysis.
describeAnalysisDefinitionResponse_name :: Lens.Lens' DescribeAnalysisDefinitionResponse (Prelude.Maybe Prelude.Text)
describeAnalysisDefinitionResponse_name = Lens.lens (\DescribeAnalysisDefinitionResponse' {name} -> name) (\s@DescribeAnalysisDefinitionResponse' {} a -> s {name = a} :: DescribeAnalysisDefinitionResponse)

-- | The Amazon Web Services request ID for this operation.
describeAnalysisDefinitionResponse_requestId :: Lens.Lens' DescribeAnalysisDefinitionResponse (Prelude.Maybe Prelude.Text)
describeAnalysisDefinitionResponse_requestId = Lens.lens (\DescribeAnalysisDefinitionResponse' {requestId} -> requestId) (\s@DescribeAnalysisDefinitionResponse' {} a -> s {requestId = a} :: DescribeAnalysisDefinitionResponse)

-- | Status associated with the analysis.
--
-- -   @CREATION_IN_PROGRESS@
--
-- -   @CREATION_SUCCESSFUL@
--
-- -   @CREATION_FAILED@
--
-- -   @UPDATE_IN_PROGRESS@
--
-- -   @UPDATE_SUCCESSFUL@
--
-- -   @UPDATE_FAILED@
--
-- -   @DELETED@
describeAnalysisDefinitionResponse_resourceStatus :: Lens.Lens' DescribeAnalysisDefinitionResponse (Prelude.Maybe ResourceStatus)
describeAnalysisDefinitionResponse_resourceStatus = Lens.lens (\DescribeAnalysisDefinitionResponse' {resourceStatus} -> resourceStatus) (\s@DescribeAnalysisDefinitionResponse' {} a -> s {resourceStatus = a} :: DescribeAnalysisDefinitionResponse)

-- | The ARN of the theme of the analysis.
describeAnalysisDefinitionResponse_themeArn :: Lens.Lens' DescribeAnalysisDefinitionResponse (Prelude.Maybe Prelude.Text)
describeAnalysisDefinitionResponse_themeArn = Lens.lens (\DescribeAnalysisDefinitionResponse' {themeArn} -> themeArn) (\s@DescribeAnalysisDefinitionResponse' {} a -> s {themeArn = a} :: DescribeAnalysisDefinitionResponse)

-- | The HTTP status of the request.
describeAnalysisDefinitionResponse_status :: Lens.Lens' DescribeAnalysisDefinitionResponse Prelude.Int
describeAnalysisDefinitionResponse_status = Lens.lens (\DescribeAnalysisDefinitionResponse' {status} -> status) (\s@DescribeAnalysisDefinitionResponse' {} a -> s {status = a} :: DescribeAnalysisDefinitionResponse)

instance
  Prelude.NFData
    DescribeAnalysisDefinitionResponse
  where
  rnf DescribeAnalysisDefinitionResponse' {..} =
    Prelude.rnf analysisId
      `Prelude.seq` Prelude.rnf definition
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf resourceStatus
      `Prelude.seq` Prelude.rnf themeArn
      `Prelude.seq` Prelude.rnf status
