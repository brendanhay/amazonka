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
-- Module      : Amazonka.QuickSight.DescribeTemplateDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a detailed description of the definition of a template.
--
-- If you do not need to know details about the content of a template, for
-- instance if you are trying to check the status of a recently created or
-- updated template, use the
-- <https://docs.aws.amazon.com/quicksight/latest/APIReference/API_DescribeTemplate.html DescribeTemplate>
-- instead.
module Amazonka.QuickSight.DescribeTemplateDefinition
  ( -- * Creating a Request
    DescribeTemplateDefinition (..),
    newDescribeTemplateDefinition,

    -- * Request Lenses
    describeTemplateDefinition_aliasName,
    describeTemplateDefinition_versionNumber,
    describeTemplateDefinition_awsAccountId,
    describeTemplateDefinition_templateId,

    -- * Destructuring the Response
    DescribeTemplateDefinitionResponse (..),
    newDescribeTemplateDefinitionResponse,

    -- * Response Lenses
    describeTemplateDefinitionResponse_definition,
    describeTemplateDefinitionResponse_errors,
    describeTemplateDefinitionResponse_name,
    describeTemplateDefinitionResponse_requestId,
    describeTemplateDefinitionResponse_resourceStatus,
    describeTemplateDefinitionResponse_templateId,
    describeTemplateDefinitionResponse_themeArn,
    describeTemplateDefinitionResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTemplateDefinition' smart constructor.
data DescribeTemplateDefinition = DescribeTemplateDefinition'
  { -- | The alias of the template that you want to describe. If you name a
    -- specific alias, you describe the version that the alias points to. You
    -- can specify the latest version of the template by providing the keyword
    -- @$LATEST@ in the @AliasName@ parameter. The keyword @$PUBLISHED@
    -- doesn\'t apply to templates.
    aliasName :: Prelude.Maybe Prelude.Text,
    -- | The version number of the template.
    versionNumber :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the Amazon Web Services account that contains the template.
    -- You must be using the Amazon Web Services account that the template is
    -- in.
    awsAccountId :: Prelude.Text,
    -- | The ID of the template that you\'re describing.
    templateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTemplateDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aliasName', 'describeTemplateDefinition_aliasName' - The alias of the template that you want to describe. If you name a
-- specific alias, you describe the version that the alias points to. You
-- can specify the latest version of the template by providing the keyword
-- @$LATEST@ in the @AliasName@ parameter. The keyword @$PUBLISHED@
-- doesn\'t apply to templates.
--
-- 'versionNumber', 'describeTemplateDefinition_versionNumber' - The version number of the template.
--
-- 'awsAccountId', 'describeTemplateDefinition_awsAccountId' - The ID of the Amazon Web Services account that contains the template.
-- You must be using the Amazon Web Services account that the template is
-- in.
--
-- 'templateId', 'describeTemplateDefinition_templateId' - The ID of the template that you\'re describing.
newDescribeTemplateDefinition ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'templateId'
  Prelude.Text ->
  DescribeTemplateDefinition
newDescribeTemplateDefinition
  pAwsAccountId_
  pTemplateId_ =
    DescribeTemplateDefinition'
      { aliasName =
          Prelude.Nothing,
        versionNumber = Prelude.Nothing,
        awsAccountId = pAwsAccountId_,
        templateId = pTemplateId_
      }

-- | The alias of the template that you want to describe. If you name a
-- specific alias, you describe the version that the alias points to. You
-- can specify the latest version of the template by providing the keyword
-- @$LATEST@ in the @AliasName@ parameter. The keyword @$PUBLISHED@
-- doesn\'t apply to templates.
describeTemplateDefinition_aliasName :: Lens.Lens' DescribeTemplateDefinition (Prelude.Maybe Prelude.Text)
describeTemplateDefinition_aliasName = Lens.lens (\DescribeTemplateDefinition' {aliasName} -> aliasName) (\s@DescribeTemplateDefinition' {} a -> s {aliasName = a} :: DescribeTemplateDefinition)

-- | The version number of the template.
describeTemplateDefinition_versionNumber :: Lens.Lens' DescribeTemplateDefinition (Prelude.Maybe Prelude.Natural)
describeTemplateDefinition_versionNumber = Lens.lens (\DescribeTemplateDefinition' {versionNumber} -> versionNumber) (\s@DescribeTemplateDefinition' {} a -> s {versionNumber = a} :: DescribeTemplateDefinition)

-- | The ID of the Amazon Web Services account that contains the template.
-- You must be using the Amazon Web Services account that the template is
-- in.
describeTemplateDefinition_awsAccountId :: Lens.Lens' DescribeTemplateDefinition Prelude.Text
describeTemplateDefinition_awsAccountId = Lens.lens (\DescribeTemplateDefinition' {awsAccountId} -> awsAccountId) (\s@DescribeTemplateDefinition' {} a -> s {awsAccountId = a} :: DescribeTemplateDefinition)

-- | The ID of the template that you\'re describing.
describeTemplateDefinition_templateId :: Lens.Lens' DescribeTemplateDefinition Prelude.Text
describeTemplateDefinition_templateId = Lens.lens (\DescribeTemplateDefinition' {templateId} -> templateId) (\s@DescribeTemplateDefinition' {} a -> s {templateId = a} :: DescribeTemplateDefinition)

instance Core.AWSRequest DescribeTemplateDefinition where
  type
    AWSResponse DescribeTemplateDefinition =
      DescribeTemplateDefinitionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTemplateDefinitionResponse'
            Prelude.<$> (x Data..?> "Definition")
            Prelude.<*> (x Data..?> "Errors")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "ResourceStatus")
            Prelude.<*> (x Data..?> "TemplateId")
            Prelude.<*> (x Data..?> "ThemeArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTemplateDefinition where
  hashWithSalt _salt DescribeTemplateDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` aliasName
      `Prelude.hashWithSalt` versionNumber
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` templateId

instance Prelude.NFData DescribeTemplateDefinition where
  rnf DescribeTemplateDefinition' {..} =
    Prelude.rnf aliasName
      `Prelude.seq` Prelude.rnf versionNumber
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf templateId

instance Data.ToHeaders DescribeTemplateDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeTemplateDefinition where
  toPath DescribeTemplateDefinition' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/templates/",
        Data.toBS templateId,
        "/definition"
      ]

instance Data.ToQuery DescribeTemplateDefinition where
  toQuery DescribeTemplateDefinition' {..} =
    Prelude.mconcat
      [ "alias-name" Data.=: aliasName,
        "version-number" Data.=: versionNumber
      ]

-- | /See:/ 'newDescribeTemplateDefinitionResponse' smart constructor.
data DescribeTemplateDefinitionResponse = DescribeTemplateDefinitionResponse'
  { -- | The definition of the template.
    --
    -- A definition is the data model of all features in a Dashboard, Template,
    -- or Analysis.
    definition :: Prelude.Maybe TemplateVersionDefinition,
    -- | Errors associated with the template version.
    errors :: Prelude.Maybe (Prelude.NonEmpty TemplateError),
    -- | The descriptive name of the template.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | Status associated with the template.
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
    -- | The ID of the template described.
    templateId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the theme of the template.
    themeArn :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTemplateDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'definition', 'describeTemplateDefinitionResponse_definition' - The definition of the template.
--
-- A definition is the data model of all features in a Dashboard, Template,
-- or Analysis.
--
-- 'errors', 'describeTemplateDefinitionResponse_errors' - Errors associated with the template version.
--
-- 'name', 'describeTemplateDefinitionResponse_name' - The descriptive name of the template.
--
-- 'requestId', 'describeTemplateDefinitionResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'resourceStatus', 'describeTemplateDefinitionResponse_resourceStatus' - Status associated with the template.
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
-- 'templateId', 'describeTemplateDefinitionResponse_templateId' - The ID of the template described.
--
-- 'themeArn', 'describeTemplateDefinitionResponse_themeArn' - The ARN of the theme of the template.
--
-- 'status', 'describeTemplateDefinitionResponse_status' - The HTTP status of the request.
newDescribeTemplateDefinitionResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeTemplateDefinitionResponse
newDescribeTemplateDefinitionResponse pStatus_ =
  DescribeTemplateDefinitionResponse'
    { definition =
        Prelude.Nothing,
      errors = Prelude.Nothing,
      name = Prelude.Nothing,
      requestId = Prelude.Nothing,
      resourceStatus = Prelude.Nothing,
      templateId = Prelude.Nothing,
      themeArn = Prelude.Nothing,
      status = pStatus_
    }

-- | The definition of the template.
--
-- A definition is the data model of all features in a Dashboard, Template,
-- or Analysis.
describeTemplateDefinitionResponse_definition :: Lens.Lens' DescribeTemplateDefinitionResponse (Prelude.Maybe TemplateVersionDefinition)
describeTemplateDefinitionResponse_definition = Lens.lens (\DescribeTemplateDefinitionResponse' {definition} -> definition) (\s@DescribeTemplateDefinitionResponse' {} a -> s {definition = a} :: DescribeTemplateDefinitionResponse)

-- | Errors associated with the template version.
describeTemplateDefinitionResponse_errors :: Lens.Lens' DescribeTemplateDefinitionResponse (Prelude.Maybe (Prelude.NonEmpty TemplateError))
describeTemplateDefinitionResponse_errors = Lens.lens (\DescribeTemplateDefinitionResponse' {errors} -> errors) (\s@DescribeTemplateDefinitionResponse' {} a -> s {errors = a} :: DescribeTemplateDefinitionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The descriptive name of the template.
describeTemplateDefinitionResponse_name :: Lens.Lens' DescribeTemplateDefinitionResponse (Prelude.Maybe Prelude.Text)
describeTemplateDefinitionResponse_name = Lens.lens (\DescribeTemplateDefinitionResponse' {name} -> name) (\s@DescribeTemplateDefinitionResponse' {} a -> s {name = a} :: DescribeTemplateDefinitionResponse)

-- | The Amazon Web Services request ID for this operation.
describeTemplateDefinitionResponse_requestId :: Lens.Lens' DescribeTemplateDefinitionResponse (Prelude.Maybe Prelude.Text)
describeTemplateDefinitionResponse_requestId = Lens.lens (\DescribeTemplateDefinitionResponse' {requestId} -> requestId) (\s@DescribeTemplateDefinitionResponse' {} a -> s {requestId = a} :: DescribeTemplateDefinitionResponse)

-- | Status associated with the template.
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
describeTemplateDefinitionResponse_resourceStatus :: Lens.Lens' DescribeTemplateDefinitionResponse (Prelude.Maybe ResourceStatus)
describeTemplateDefinitionResponse_resourceStatus = Lens.lens (\DescribeTemplateDefinitionResponse' {resourceStatus} -> resourceStatus) (\s@DescribeTemplateDefinitionResponse' {} a -> s {resourceStatus = a} :: DescribeTemplateDefinitionResponse)

-- | The ID of the template described.
describeTemplateDefinitionResponse_templateId :: Lens.Lens' DescribeTemplateDefinitionResponse (Prelude.Maybe Prelude.Text)
describeTemplateDefinitionResponse_templateId = Lens.lens (\DescribeTemplateDefinitionResponse' {templateId} -> templateId) (\s@DescribeTemplateDefinitionResponse' {} a -> s {templateId = a} :: DescribeTemplateDefinitionResponse)

-- | The ARN of the theme of the template.
describeTemplateDefinitionResponse_themeArn :: Lens.Lens' DescribeTemplateDefinitionResponse (Prelude.Maybe Prelude.Text)
describeTemplateDefinitionResponse_themeArn = Lens.lens (\DescribeTemplateDefinitionResponse' {themeArn} -> themeArn) (\s@DescribeTemplateDefinitionResponse' {} a -> s {themeArn = a} :: DescribeTemplateDefinitionResponse)

-- | The HTTP status of the request.
describeTemplateDefinitionResponse_status :: Lens.Lens' DescribeTemplateDefinitionResponse Prelude.Int
describeTemplateDefinitionResponse_status = Lens.lens (\DescribeTemplateDefinitionResponse' {status} -> status) (\s@DescribeTemplateDefinitionResponse' {} a -> s {status = a} :: DescribeTemplateDefinitionResponse)

instance
  Prelude.NFData
    DescribeTemplateDefinitionResponse
  where
  rnf DescribeTemplateDefinitionResponse' {..} =
    Prelude.rnf definition
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf resourceStatus
      `Prelude.seq` Prelude.rnf templateId
      `Prelude.seq` Prelude.rnf themeArn
      `Prelude.seq` Prelude.rnf status
