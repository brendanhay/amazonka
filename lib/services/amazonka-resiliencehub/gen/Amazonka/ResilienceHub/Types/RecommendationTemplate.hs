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
-- Module      : Amazonka.ResilienceHub.Types.RecommendationTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.RecommendationTemplate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResilienceHub.Types.RecommendationTemplateStatus
import Amazonka.ResilienceHub.Types.RenderRecommendationType
import Amazonka.ResilienceHub.Types.S3Location
import Amazonka.ResilienceHub.Types.TemplateFormat

-- | Defines a recommendation template created with the
-- CreateRecommendationTemplate action.
--
-- /See:/ 'newRecommendationTemplate' smart constructor.
data RecommendationTemplate = RecommendationTemplate'
  { -- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
    -- format for this ARN is:
    -- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
    -- information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/ guide.
    appArn :: Prelude.Maybe Prelude.Text,
    -- | The end time for the action.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The message for the recommendation template.
    message :: Prelude.Maybe Prelude.Text,
    -- | Indicates if replacements are needed.
    needsReplacements :: Prelude.Maybe Prelude.Bool,
    -- | Identifiers for the recommendations used in the recommendation template.
    recommendationIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The start time for the action.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The tags assigned to the resource. A tag is a label that you assign to
    -- an Amazon Web Services resource. Each tag consists of a key\/value pair.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The file location of the template.
    templatesLocation :: Prelude.Maybe S3Location,
    -- | The Amazon Resource Name (ARN) of the assessment. The format for this
    -- ARN is:
    -- arn:@partition@:resiliencehub:@region@:@account@:app-assessment\/@app-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/ guide.
    assessmentArn :: Prelude.Text,
    -- | The format of the recommendation template.
    --
    -- [CfnJson]
    --     The template is CloudFormation JSON.
    --
    -- [CfnYaml]
    --     The template is CloudFormation YAML.
    format :: TemplateFormat,
    -- | The name for the recommendation template.
    name :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the recommendation template.
    recommendationTemplateArn :: Prelude.Text,
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
    recommendationTypes :: Prelude.NonEmpty RenderRecommendationType,
    -- | The status of the action.
    status :: RecommendationTemplateStatus
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommendationTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appArn', 'recommendationTemplate_appArn' - The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
--
-- 'endTime', 'recommendationTemplate_endTime' - The end time for the action.
--
-- 'message', 'recommendationTemplate_message' - The message for the recommendation template.
--
-- 'needsReplacements', 'recommendationTemplate_needsReplacements' - Indicates if replacements are needed.
--
-- 'recommendationIds', 'recommendationTemplate_recommendationIds' - Identifiers for the recommendations used in the recommendation template.
--
-- 'startTime', 'recommendationTemplate_startTime' - The start time for the action.
--
-- 'tags', 'recommendationTemplate_tags' - The tags assigned to the resource. A tag is a label that you assign to
-- an Amazon Web Services resource. Each tag consists of a key\/value pair.
--
-- 'templatesLocation', 'recommendationTemplate_templatesLocation' - The file location of the template.
--
-- 'assessmentArn', 'recommendationTemplate_assessmentArn' - The Amazon Resource Name (ARN) of the assessment. The format for this
-- ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app-assessment\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
--
-- 'format', 'recommendationTemplate_format' - The format of the recommendation template.
--
-- [CfnJson]
--     The template is CloudFormation JSON.
--
-- [CfnYaml]
--     The template is CloudFormation YAML.
--
-- 'name', 'recommendationTemplate_name' - The name for the recommendation template.
--
-- 'recommendationTemplateArn', 'recommendationTemplate_recommendationTemplateArn' - The Amazon Resource Name (ARN) for the recommendation template.
--
-- 'recommendationTypes', 'recommendationTemplate_recommendationTypes' - An array of strings that specify the recommendation template type or
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
-- 'status', 'recommendationTemplate_status' - The status of the action.
newRecommendationTemplate ::
  -- | 'assessmentArn'
  Prelude.Text ->
  -- | 'format'
  TemplateFormat ->
  -- | 'name'
  Prelude.Text ->
  -- | 'recommendationTemplateArn'
  Prelude.Text ->
  -- | 'recommendationTypes'
  Prelude.NonEmpty RenderRecommendationType ->
  -- | 'status'
  RecommendationTemplateStatus ->
  RecommendationTemplate
newRecommendationTemplate
  pAssessmentArn_
  pFormat_
  pName_
  pRecommendationTemplateArn_
  pRecommendationTypes_
  pStatus_ =
    RecommendationTemplate'
      { appArn = Prelude.Nothing,
        endTime = Prelude.Nothing,
        message = Prelude.Nothing,
        needsReplacements = Prelude.Nothing,
        recommendationIds = Prelude.Nothing,
        startTime = Prelude.Nothing,
        tags = Prelude.Nothing,
        templatesLocation = Prelude.Nothing,
        assessmentArn = pAssessmentArn_,
        format = pFormat_,
        name = pName_,
        recommendationTemplateArn =
          pRecommendationTemplateArn_,
        recommendationTypes =
          Lens.coerced Lens.# pRecommendationTypes_,
        status = pStatus_
      }

-- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
recommendationTemplate_appArn :: Lens.Lens' RecommendationTemplate (Prelude.Maybe Prelude.Text)
recommendationTemplate_appArn = Lens.lens (\RecommendationTemplate' {appArn} -> appArn) (\s@RecommendationTemplate' {} a -> s {appArn = a} :: RecommendationTemplate)

-- | The end time for the action.
recommendationTemplate_endTime :: Lens.Lens' RecommendationTemplate (Prelude.Maybe Prelude.UTCTime)
recommendationTemplate_endTime = Lens.lens (\RecommendationTemplate' {endTime} -> endTime) (\s@RecommendationTemplate' {} a -> s {endTime = a} :: RecommendationTemplate) Prelude.. Lens.mapping Data._Time

-- | The message for the recommendation template.
recommendationTemplate_message :: Lens.Lens' RecommendationTemplate (Prelude.Maybe Prelude.Text)
recommendationTemplate_message = Lens.lens (\RecommendationTemplate' {message} -> message) (\s@RecommendationTemplate' {} a -> s {message = a} :: RecommendationTemplate)

-- | Indicates if replacements are needed.
recommendationTemplate_needsReplacements :: Lens.Lens' RecommendationTemplate (Prelude.Maybe Prelude.Bool)
recommendationTemplate_needsReplacements = Lens.lens (\RecommendationTemplate' {needsReplacements} -> needsReplacements) (\s@RecommendationTemplate' {} a -> s {needsReplacements = a} :: RecommendationTemplate)

-- | Identifiers for the recommendations used in the recommendation template.
recommendationTemplate_recommendationIds :: Lens.Lens' RecommendationTemplate (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
recommendationTemplate_recommendationIds = Lens.lens (\RecommendationTemplate' {recommendationIds} -> recommendationIds) (\s@RecommendationTemplate' {} a -> s {recommendationIds = a} :: RecommendationTemplate) Prelude.. Lens.mapping Lens.coerced

-- | The start time for the action.
recommendationTemplate_startTime :: Lens.Lens' RecommendationTemplate (Prelude.Maybe Prelude.UTCTime)
recommendationTemplate_startTime = Lens.lens (\RecommendationTemplate' {startTime} -> startTime) (\s@RecommendationTemplate' {} a -> s {startTime = a} :: RecommendationTemplate) Prelude.. Lens.mapping Data._Time

-- | The tags assigned to the resource. A tag is a label that you assign to
-- an Amazon Web Services resource. Each tag consists of a key\/value pair.
recommendationTemplate_tags :: Lens.Lens' RecommendationTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
recommendationTemplate_tags = Lens.lens (\RecommendationTemplate' {tags} -> tags) (\s@RecommendationTemplate' {} a -> s {tags = a} :: RecommendationTemplate) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The file location of the template.
recommendationTemplate_templatesLocation :: Lens.Lens' RecommendationTemplate (Prelude.Maybe S3Location)
recommendationTemplate_templatesLocation = Lens.lens (\RecommendationTemplate' {templatesLocation} -> templatesLocation) (\s@RecommendationTemplate' {} a -> s {templatesLocation = a} :: RecommendationTemplate)

-- | The Amazon Resource Name (ARN) of the assessment. The format for this
-- ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app-assessment\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
recommendationTemplate_assessmentArn :: Lens.Lens' RecommendationTemplate Prelude.Text
recommendationTemplate_assessmentArn = Lens.lens (\RecommendationTemplate' {assessmentArn} -> assessmentArn) (\s@RecommendationTemplate' {} a -> s {assessmentArn = a} :: RecommendationTemplate)

-- | The format of the recommendation template.
--
-- [CfnJson]
--     The template is CloudFormation JSON.
--
-- [CfnYaml]
--     The template is CloudFormation YAML.
recommendationTemplate_format :: Lens.Lens' RecommendationTemplate TemplateFormat
recommendationTemplate_format = Lens.lens (\RecommendationTemplate' {format} -> format) (\s@RecommendationTemplate' {} a -> s {format = a} :: RecommendationTemplate)

-- | The name for the recommendation template.
recommendationTemplate_name :: Lens.Lens' RecommendationTemplate Prelude.Text
recommendationTemplate_name = Lens.lens (\RecommendationTemplate' {name} -> name) (\s@RecommendationTemplate' {} a -> s {name = a} :: RecommendationTemplate)

-- | The Amazon Resource Name (ARN) for the recommendation template.
recommendationTemplate_recommendationTemplateArn :: Lens.Lens' RecommendationTemplate Prelude.Text
recommendationTemplate_recommendationTemplateArn = Lens.lens (\RecommendationTemplate' {recommendationTemplateArn} -> recommendationTemplateArn) (\s@RecommendationTemplate' {} a -> s {recommendationTemplateArn = a} :: RecommendationTemplate)

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
recommendationTemplate_recommendationTypes :: Lens.Lens' RecommendationTemplate (Prelude.NonEmpty RenderRecommendationType)
recommendationTemplate_recommendationTypes = Lens.lens (\RecommendationTemplate' {recommendationTypes} -> recommendationTypes) (\s@RecommendationTemplate' {} a -> s {recommendationTypes = a} :: RecommendationTemplate) Prelude.. Lens.coerced

-- | The status of the action.
recommendationTemplate_status :: Lens.Lens' RecommendationTemplate RecommendationTemplateStatus
recommendationTemplate_status = Lens.lens (\RecommendationTemplate' {status} -> status) (\s@RecommendationTemplate' {} a -> s {status = a} :: RecommendationTemplate)

instance Data.FromJSON RecommendationTemplate where
  parseJSON =
    Data.withObject
      "RecommendationTemplate"
      ( \x ->
          RecommendationTemplate'
            Prelude.<$> (x Data..:? "appArn")
            Prelude.<*> (x Data..:? "endTime")
            Prelude.<*> (x Data..:? "message")
            Prelude.<*> (x Data..:? "needsReplacements")
            Prelude.<*> (x Data..:? "recommendationIds")
            Prelude.<*> (x Data..:? "startTime")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "templatesLocation")
            Prelude.<*> (x Data..: "assessmentArn")
            Prelude.<*> (x Data..: "format")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "recommendationTemplateArn")
            Prelude.<*> (x Data..: "recommendationTypes")
            Prelude.<*> (x Data..: "status")
      )

instance Prelude.Hashable RecommendationTemplate where
  hashWithSalt _salt RecommendationTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` appArn
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` needsReplacements
      `Prelude.hashWithSalt` recommendationIds
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` templatesLocation
      `Prelude.hashWithSalt` assessmentArn
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` recommendationTemplateArn
      `Prelude.hashWithSalt` recommendationTypes
      `Prelude.hashWithSalt` status

instance Prelude.NFData RecommendationTemplate where
  rnf RecommendationTemplate' {..} =
    Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf needsReplacements
      `Prelude.seq` Prelude.rnf recommendationIds
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf templatesLocation
      `Prelude.seq` Prelude.rnf assessmentArn
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf recommendationTemplateArn
      `Prelude.seq` Prelude.rnf recommendationTypes
      `Prelude.seq` Prelude.rnf status
