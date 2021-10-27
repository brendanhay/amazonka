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
-- Module      : Network.AWS.QuickSight.CreateAnalysis
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an analysis in Amazon QuickSight.
module Network.AWS.QuickSight.CreateAnalysis
  ( -- * Creating a Request
    CreateAnalysis (..),
    newCreateAnalysis,

    -- * Request Lenses
    createAnalysis_themeArn,
    createAnalysis_parameters,
    createAnalysis_permissions,
    createAnalysis_tags,
    createAnalysis_awsAccountId,
    createAnalysis_analysisId,
    createAnalysis_name,
    createAnalysis_sourceEntity,

    -- * Destructuring the Response
    CreateAnalysisResponse (..),
    newCreateAnalysisResponse,

    -- * Response Lenses
    createAnalysisResponse_requestId,
    createAnalysisResponse_analysisId,
    createAnalysisResponse_arn,
    createAnalysisResponse_creationStatus,
    createAnalysisResponse_status,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.QuickSight.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateAnalysis' smart constructor.
data CreateAnalysis = CreateAnalysis'
  { -- | The ARN for the theme to apply to the analysis that you\'re creating. To
    -- see the theme in the Amazon QuickSight console, make sure that you have
    -- access to it.
    themeArn :: Prelude.Maybe Prelude.Text,
    -- | The parameter names and override values that you want to use. An
    -- analysis can have any parameter type, and some parameters might accept
    -- multiple values.
    parameters :: Prelude.Maybe Parameters,
    -- | A structure that describes the principals and the resource-level
    -- permissions on an analysis. You can use the @Permissions@ structure to
    -- grant permissions by providing a list of Identity and Access Management
    -- (IAM) action information for each principal listed by Amazon Resource
    -- Name (ARN).
    --
    -- To specify no permissions, omit @Permissions@.
    permissions :: Prelude.Maybe (Prelude.NonEmpty ResourcePermission),
    -- | Contains a map of the key-value pairs for the resource tag or tags
    -- assigned to the analysis.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The ID of the Amazon Web Services account where you are creating an
    -- analysis.
    awsAccountId :: Prelude.Text,
    -- | The ID for the analysis that you\'re creating. This ID displays in the
    -- URL of the analysis.
    analysisId :: Prelude.Text,
    -- | A descriptive name for the analysis that you\'re creating. This name
    -- displays for the analysis in the Amazon QuickSight console.
    name :: Prelude.Text,
    -- | A source entity to use for the analysis that you\'re creating. This
    -- metadata structure contains details that describe a source template and
    -- one or more datasets.
    sourceEntity :: AnalysisSourceEntity
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAnalysis' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'themeArn', 'createAnalysis_themeArn' - The ARN for the theme to apply to the analysis that you\'re creating. To
-- see the theme in the Amazon QuickSight console, make sure that you have
-- access to it.
--
-- 'parameters', 'createAnalysis_parameters' - The parameter names and override values that you want to use. An
-- analysis can have any parameter type, and some parameters might accept
-- multiple values.
--
-- 'permissions', 'createAnalysis_permissions' - A structure that describes the principals and the resource-level
-- permissions on an analysis. You can use the @Permissions@ structure to
-- grant permissions by providing a list of Identity and Access Management
-- (IAM) action information for each principal listed by Amazon Resource
-- Name (ARN).
--
-- To specify no permissions, omit @Permissions@.
--
-- 'tags', 'createAnalysis_tags' - Contains a map of the key-value pairs for the resource tag or tags
-- assigned to the analysis.
--
-- 'awsAccountId', 'createAnalysis_awsAccountId' - The ID of the Amazon Web Services account where you are creating an
-- analysis.
--
-- 'analysisId', 'createAnalysis_analysisId' - The ID for the analysis that you\'re creating. This ID displays in the
-- URL of the analysis.
--
-- 'name', 'createAnalysis_name' - A descriptive name for the analysis that you\'re creating. This name
-- displays for the analysis in the Amazon QuickSight console.
--
-- 'sourceEntity', 'createAnalysis_sourceEntity' - A source entity to use for the analysis that you\'re creating. This
-- metadata structure contains details that describe a source template and
-- one or more datasets.
newCreateAnalysis ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'analysisId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'sourceEntity'
  AnalysisSourceEntity ->
  CreateAnalysis
newCreateAnalysis
  pAwsAccountId_
  pAnalysisId_
  pName_
  pSourceEntity_ =
    CreateAnalysis'
      { themeArn = Prelude.Nothing,
        parameters = Prelude.Nothing,
        permissions = Prelude.Nothing,
        tags = Prelude.Nothing,
        awsAccountId = pAwsAccountId_,
        analysisId = pAnalysisId_,
        name = pName_,
        sourceEntity = pSourceEntity_
      }

-- | The ARN for the theme to apply to the analysis that you\'re creating. To
-- see the theme in the Amazon QuickSight console, make sure that you have
-- access to it.
createAnalysis_themeArn :: Lens.Lens' CreateAnalysis (Prelude.Maybe Prelude.Text)
createAnalysis_themeArn = Lens.lens (\CreateAnalysis' {themeArn} -> themeArn) (\s@CreateAnalysis' {} a -> s {themeArn = a} :: CreateAnalysis)

-- | The parameter names and override values that you want to use. An
-- analysis can have any parameter type, and some parameters might accept
-- multiple values.
createAnalysis_parameters :: Lens.Lens' CreateAnalysis (Prelude.Maybe Parameters)
createAnalysis_parameters = Lens.lens (\CreateAnalysis' {parameters} -> parameters) (\s@CreateAnalysis' {} a -> s {parameters = a} :: CreateAnalysis)

-- | A structure that describes the principals and the resource-level
-- permissions on an analysis. You can use the @Permissions@ structure to
-- grant permissions by providing a list of Identity and Access Management
-- (IAM) action information for each principal listed by Amazon Resource
-- Name (ARN).
--
-- To specify no permissions, omit @Permissions@.
createAnalysis_permissions :: Lens.Lens' CreateAnalysis (Prelude.Maybe (Prelude.NonEmpty ResourcePermission))
createAnalysis_permissions = Lens.lens (\CreateAnalysis' {permissions} -> permissions) (\s@CreateAnalysis' {} a -> s {permissions = a} :: CreateAnalysis) Prelude.. Lens.mapping Lens.coerced

-- | Contains a map of the key-value pairs for the resource tag or tags
-- assigned to the analysis.
createAnalysis_tags :: Lens.Lens' CreateAnalysis (Prelude.Maybe (Prelude.NonEmpty Tag))
createAnalysis_tags = Lens.lens (\CreateAnalysis' {tags} -> tags) (\s@CreateAnalysis' {} a -> s {tags = a} :: CreateAnalysis) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Amazon Web Services account where you are creating an
-- analysis.
createAnalysis_awsAccountId :: Lens.Lens' CreateAnalysis Prelude.Text
createAnalysis_awsAccountId = Lens.lens (\CreateAnalysis' {awsAccountId} -> awsAccountId) (\s@CreateAnalysis' {} a -> s {awsAccountId = a} :: CreateAnalysis)

-- | The ID for the analysis that you\'re creating. This ID displays in the
-- URL of the analysis.
createAnalysis_analysisId :: Lens.Lens' CreateAnalysis Prelude.Text
createAnalysis_analysisId = Lens.lens (\CreateAnalysis' {analysisId} -> analysisId) (\s@CreateAnalysis' {} a -> s {analysisId = a} :: CreateAnalysis)

-- | A descriptive name for the analysis that you\'re creating. This name
-- displays for the analysis in the Amazon QuickSight console.
createAnalysis_name :: Lens.Lens' CreateAnalysis Prelude.Text
createAnalysis_name = Lens.lens (\CreateAnalysis' {name} -> name) (\s@CreateAnalysis' {} a -> s {name = a} :: CreateAnalysis)

-- | A source entity to use for the analysis that you\'re creating. This
-- metadata structure contains details that describe a source template and
-- one or more datasets.
createAnalysis_sourceEntity :: Lens.Lens' CreateAnalysis AnalysisSourceEntity
createAnalysis_sourceEntity = Lens.lens (\CreateAnalysis' {sourceEntity} -> sourceEntity) (\s@CreateAnalysis' {} a -> s {sourceEntity = a} :: CreateAnalysis)

instance Core.AWSRequest CreateAnalysis where
  type
    AWSResponse CreateAnalysis =
      CreateAnalysisResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAnalysisResponse'
            Prelude.<$> (x Core..?> "RequestId")
            Prelude.<*> (x Core..?> "AnalysisId")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "CreationStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAnalysis

instance Prelude.NFData CreateAnalysis

instance Core.ToHeaders CreateAnalysis where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateAnalysis where
  toJSON CreateAnalysis' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ThemeArn" Core..=) Prelude.<$> themeArn,
            ("Parameters" Core..=) Prelude.<$> parameters,
            ("Permissions" Core..=) Prelude.<$> permissions,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("SourceEntity" Core..= sourceEntity)
          ]
      )

instance Core.ToPath CreateAnalysis where
  toPath CreateAnalysis' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Core.toBS awsAccountId,
        "/analyses/",
        Core.toBS analysisId
      ]

instance Core.ToQuery CreateAnalysis where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAnalysisResponse' smart constructor.
data CreateAnalysisResponse = CreateAnalysisResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the analysis.
    analysisId :: Prelude.Maybe Prelude.Text,
    -- | The ARN for the analysis.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The status of the creation of the analysis.
    creationStatus :: Prelude.Maybe ResourceStatus,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAnalysisResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'createAnalysisResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'analysisId', 'createAnalysisResponse_analysisId' - The ID of the analysis.
--
-- 'arn', 'createAnalysisResponse_arn' - The ARN for the analysis.
--
-- 'creationStatus', 'createAnalysisResponse_creationStatus' - The status of the creation of the analysis.
--
-- 'status', 'createAnalysisResponse_status' - The HTTP status of the request.
newCreateAnalysisResponse ::
  -- | 'status'
  Prelude.Int ->
  CreateAnalysisResponse
newCreateAnalysisResponse pStatus_ =
  CreateAnalysisResponse'
    { requestId =
        Prelude.Nothing,
      analysisId = Prelude.Nothing,
      arn = Prelude.Nothing,
      creationStatus = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
createAnalysisResponse_requestId :: Lens.Lens' CreateAnalysisResponse (Prelude.Maybe Prelude.Text)
createAnalysisResponse_requestId = Lens.lens (\CreateAnalysisResponse' {requestId} -> requestId) (\s@CreateAnalysisResponse' {} a -> s {requestId = a} :: CreateAnalysisResponse)

-- | The ID of the analysis.
createAnalysisResponse_analysisId :: Lens.Lens' CreateAnalysisResponse (Prelude.Maybe Prelude.Text)
createAnalysisResponse_analysisId = Lens.lens (\CreateAnalysisResponse' {analysisId} -> analysisId) (\s@CreateAnalysisResponse' {} a -> s {analysisId = a} :: CreateAnalysisResponse)

-- | The ARN for the analysis.
createAnalysisResponse_arn :: Lens.Lens' CreateAnalysisResponse (Prelude.Maybe Prelude.Text)
createAnalysisResponse_arn = Lens.lens (\CreateAnalysisResponse' {arn} -> arn) (\s@CreateAnalysisResponse' {} a -> s {arn = a} :: CreateAnalysisResponse)

-- | The status of the creation of the analysis.
createAnalysisResponse_creationStatus :: Lens.Lens' CreateAnalysisResponse (Prelude.Maybe ResourceStatus)
createAnalysisResponse_creationStatus = Lens.lens (\CreateAnalysisResponse' {creationStatus} -> creationStatus) (\s@CreateAnalysisResponse' {} a -> s {creationStatus = a} :: CreateAnalysisResponse)

-- | The HTTP status of the request.
createAnalysisResponse_status :: Lens.Lens' CreateAnalysisResponse Prelude.Int
createAnalysisResponse_status = Lens.lens (\CreateAnalysisResponse' {status} -> status) (\s@CreateAnalysisResponse' {} a -> s {status = a} :: CreateAnalysisResponse)

instance Prelude.NFData CreateAnalysisResponse
