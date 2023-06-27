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
-- Module      : Amazonka.ResilienceHub.CreateRecommendationTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new recommendation template for the Resilience Hub
-- application.
module Amazonka.ResilienceHub.CreateRecommendationTemplate
  ( -- * Creating a Request
    CreateRecommendationTemplate (..),
    newCreateRecommendationTemplate,

    -- * Request Lenses
    createRecommendationTemplate_bucketName,
    createRecommendationTemplate_clientToken,
    createRecommendationTemplate_format,
    createRecommendationTemplate_recommendationIds,
    createRecommendationTemplate_recommendationTypes,
    createRecommendationTemplate_tags,
    createRecommendationTemplate_assessmentArn,
    createRecommendationTemplate_name,

    -- * Destructuring the Response
    CreateRecommendationTemplateResponse (..),
    newCreateRecommendationTemplateResponse,

    -- * Response Lenses
    createRecommendationTemplateResponse_recommendationTemplate,
    createRecommendationTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateRecommendationTemplate' smart constructor.
data CreateRecommendationTemplate = CreateRecommendationTemplate'
  { -- | The name of the Amazon S3 bucket that will contain the recommendation
    -- template.
    bucketName :: Prelude.Maybe Prelude.Text,
    -- | Used for an idempotency token. A client token is a unique,
    -- case-sensitive string of up to 64 ASCII characters. You should not reuse
    -- the same client token for other API requests.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The format for the recommendation template.
    --
    -- [CfnJson]
    --     The template is CloudFormation JSON.
    --
    -- [CfnYaml]
    --     The template is CloudFormation YAML.
    format :: Prelude.Maybe TemplateFormat,
    -- | Identifiers for the recommendations used to create a recommendation
    -- template.
    recommendationIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | An array of strings that specify the recommendation template type or
    -- types.
    --
    -- [Alarm]
    --     The template is an AlarmRecommendation template.
    --
    -- [Sop]
    --     The template is a SopRecommendation template.
    --
    -- [Test]
    --     The template is a TestRecommendation template.
    recommendationTypes :: Prelude.Maybe (Prelude.NonEmpty RenderRecommendationType),
    -- | The tags assigned to the resource. A tag is a label that you assign to
    -- an Amazon Web Services resource. Each tag consists of a key\/value pair.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The Amazon Resource Name (ARN) of the assessment. The format for this
    -- ARN is:
    -- arn:@partition@:resiliencehub:@region@:@account@:app-assessment\/@app-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/ guide.
    assessmentArn :: Prelude.Text,
    -- | The name for the recommendation template.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRecommendationTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketName', 'createRecommendationTemplate_bucketName' - The name of the Amazon S3 bucket that will contain the recommendation
-- template.
--
-- 'clientToken', 'createRecommendationTemplate_clientToken' - Used for an idempotency token. A client token is a unique,
-- case-sensitive string of up to 64 ASCII characters. You should not reuse
-- the same client token for other API requests.
--
-- 'format', 'createRecommendationTemplate_format' - The format for the recommendation template.
--
-- [CfnJson]
--     The template is CloudFormation JSON.
--
-- [CfnYaml]
--     The template is CloudFormation YAML.
--
-- 'recommendationIds', 'createRecommendationTemplate_recommendationIds' - Identifiers for the recommendations used to create a recommendation
-- template.
--
-- 'recommendationTypes', 'createRecommendationTemplate_recommendationTypes' - An array of strings that specify the recommendation template type or
-- types.
--
-- [Alarm]
--     The template is an AlarmRecommendation template.
--
-- [Sop]
--     The template is a SopRecommendation template.
--
-- [Test]
--     The template is a TestRecommendation template.
--
-- 'tags', 'createRecommendationTemplate_tags' - The tags assigned to the resource. A tag is a label that you assign to
-- an Amazon Web Services resource. Each tag consists of a key\/value pair.
--
-- 'assessmentArn', 'createRecommendationTemplate_assessmentArn' - The Amazon Resource Name (ARN) of the assessment. The format for this
-- ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app-assessment\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
--
-- 'name', 'createRecommendationTemplate_name' - The name for the recommendation template.
newCreateRecommendationTemplate ::
  -- | 'assessmentArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CreateRecommendationTemplate
newCreateRecommendationTemplate
  pAssessmentArn_
  pName_ =
    CreateRecommendationTemplate'
      { bucketName =
          Prelude.Nothing,
        clientToken = Prelude.Nothing,
        format = Prelude.Nothing,
        recommendationIds = Prelude.Nothing,
        recommendationTypes = Prelude.Nothing,
        tags = Prelude.Nothing,
        assessmentArn = pAssessmentArn_,
        name = pName_
      }

-- | The name of the Amazon S3 bucket that will contain the recommendation
-- template.
createRecommendationTemplate_bucketName :: Lens.Lens' CreateRecommendationTemplate (Prelude.Maybe Prelude.Text)
createRecommendationTemplate_bucketName = Lens.lens (\CreateRecommendationTemplate' {bucketName} -> bucketName) (\s@CreateRecommendationTemplate' {} a -> s {bucketName = a} :: CreateRecommendationTemplate)

-- | Used for an idempotency token. A client token is a unique,
-- case-sensitive string of up to 64 ASCII characters. You should not reuse
-- the same client token for other API requests.
createRecommendationTemplate_clientToken :: Lens.Lens' CreateRecommendationTemplate (Prelude.Maybe Prelude.Text)
createRecommendationTemplate_clientToken = Lens.lens (\CreateRecommendationTemplate' {clientToken} -> clientToken) (\s@CreateRecommendationTemplate' {} a -> s {clientToken = a} :: CreateRecommendationTemplate)

-- | The format for the recommendation template.
--
-- [CfnJson]
--     The template is CloudFormation JSON.
--
-- [CfnYaml]
--     The template is CloudFormation YAML.
createRecommendationTemplate_format :: Lens.Lens' CreateRecommendationTemplate (Prelude.Maybe TemplateFormat)
createRecommendationTemplate_format = Lens.lens (\CreateRecommendationTemplate' {format} -> format) (\s@CreateRecommendationTemplate' {} a -> s {format = a} :: CreateRecommendationTemplate)

-- | Identifiers for the recommendations used to create a recommendation
-- template.
createRecommendationTemplate_recommendationIds :: Lens.Lens' CreateRecommendationTemplate (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createRecommendationTemplate_recommendationIds = Lens.lens (\CreateRecommendationTemplate' {recommendationIds} -> recommendationIds) (\s@CreateRecommendationTemplate' {} a -> s {recommendationIds = a} :: CreateRecommendationTemplate) Prelude.. Lens.mapping Lens.coerced

-- | An array of strings that specify the recommendation template type or
-- types.
--
-- [Alarm]
--     The template is an AlarmRecommendation template.
--
-- [Sop]
--     The template is a SopRecommendation template.
--
-- [Test]
--     The template is a TestRecommendation template.
createRecommendationTemplate_recommendationTypes :: Lens.Lens' CreateRecommendationTemplate (Prelude.Maybe (Prelude.NonEmpty RenderRecommendationType))
createRecommendationTemplate_recommendationTypes = Lens.lens (\CreateRecommendationTemplate' {recommendationTypes} -> recommendationTypes) (\s@CreateRecommendationTemplate' {} a -> s {recommendationTypes = a} :: CreateRecommendationTemplate) Prelude.. Lens.mapping Lens.coerced

-- | The tags assigned to the resource. A tag is a label that you assign to
-- an Amazon Web Services resource. Each tag consists of a key\/value pair.
createRecommendationTemplate_tags :: Lens.Lens' CreateRecommendationTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createRecommendationTemplate_tags = Lens.lens (\CreateRecommendationTemplate' {tags} -> tags) (\s@CreateRecommendationTemplate' {} a -> s {tags = a} :: CreateRecommendationTemplate) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The Amazon Resource Name (ARN) of the assessment. The format for this
-- ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app-assessment\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
createRecommendationTemplate_assessmentArn :: Lens.Lens' CreateRecommendationTemplate Prelude.Text
createRecommendationTemplate_assessmentArn = Lens.lens (\CreateRecommendationTemplate' {assessmentArn} -> assessmentArn) (\s@CreateRecommendationTemplate' {} a -> s {assessmentArn = a} :: CreateRecommendationTemplate)

-- | The name for the recommendation template.
createRecommendationTemplate_name :: Lens.Lens' CreateRecommendationTemplate Prelude.Text
createRecommendationTemplate_name = Lens.lens (\CreateRecommendationTemplate' {name} -> name) (\s@CreateRecommendationTemplate' {} a -> s {name = a} :: CreateRecommendationTemplate)

instance Core.AWSRequest CreateRecommendationTemplate where
  type
    AWSResponse CreateRecommendationTemplate =
      CreateRecommendationTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRecommendationTemplateResponse'
            Prelude.<$> (x Data..?> "recommendationTemplate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateRecommendationTemplate
  where
  hashWithSalt _salt CreateRecommendationTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` recommendationIds
      `Prelude.hashWithSalt` recommendationTypes
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` assessmentArn
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateRecommendationTemplate where
  rnf CreateRecommendationTemplate' {..} =
    Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf recommendationIds
      `Prelude.seq` Prelude.rnf recommendationTypes
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf assessmentArn
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateRecommendationTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateRecommendationTemplate where
  toJSON CreateRecommendationTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bucketName" Data..=) Prelude.<$> bucketName,
            ("clientToken" Data..=) Prelude.<$> clientToken,
            ("format" Data..=) Prelude.<$> format,
            ("recommendationIds" Data..=)
              Prelude.<$> recommendationIds,
            ("recommendationTypes" Data..=)
              Prelude.<$> recommendationTypes,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("assessmentArn" Data..= assessmentArn),
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateRecommendationTemplate where
  toPath =
    Prelude.const "/create-recommendation-template"

instance Data.ToQuery CreateRecommendationTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateRecommendationTemplateResponse' smart constructor.
data CreateRecommendationTemplateResponse = CreateRecommendationTemplateResponse'
  { -- | The newly created recommendation template, returned as an object. This
    -- object includes the template\'s name, format, status, tags, Amazon S3
    -- bucket location, and more.
    recommendationTemplate :: Prelude.Maybe RecommendationTemplate,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRecommendationTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recommendationTemplate', 'createRecommendationTemplateResponse_recommendationTemplate' - The newly created recommendation template, returned as an object. This
-- object includes the template\'s name, format, status, tags, Amazon S3
-- bucket location, and more.
--
-- 'httpStatus', 'createRecommendationTemplateResponse_httpStatus' - The response's http status code.
newCreateRecommendationTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateRecommendationTemplateResponse
newCreateRecommendationTemplateResponse pHttpStatus_ =
  CreateRecommendationTemplateResponse'
    { recommendationTemplate =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The newly created recommendation template, returned as an object. This
-- object includes the template\'s name, format, status, tags, Amazon S3
-- bucket location, and more.
createRecommendationTemplateResponse_recommendationTemplate :: Lens.Lens' CreateRecommendationTemplateResponse (Prelude.Maybe RecommendationTemplate)
createRecommendationTemplateResponse_recommendationTemplate = Lens.lens (\CreateRecommendationTemplateResponse' {recommendationTemplate} -> recommendationTemplate) (\s@CreateRecommendationTemplateResponse' {} a -> s {recommendationTemplate = a} :: CreateRecommendationTemplateResponse)

-- | The response's http status code.
createRecommendationTemplateResponse_httpStatus :: Lens.Lens' CreateRecommendationTemplateResponse Prelude.Int
createRecommendationTemplateResponse_httpStatus = Lens.lens (\CreateRecommendationTemplateResponse' {httpStatus} -> httpStatus) (\s@CreateRecommendationTemplateResponse' {} a -> s {httpStatus = a} :: CreateRecommendationTemplateResponse)

instance
  Prelude.NFData
    CreateRecommendationTemplateResponse
  where
  rnf CreateRecommendationTemplateResponse' {..} =
    Prelude.rnf recommendationTemplate
      `Prelude.seq` Prelude.rnf httpStatus
