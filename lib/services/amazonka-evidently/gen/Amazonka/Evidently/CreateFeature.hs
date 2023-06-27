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
-- Module      : Amazonka.Evidently.CreateFeature
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Evidently /feature/ that you want to launch or test. You can
-- define up to five variations of a feature, and use these variations in
-- your launches and experiments. A feature must be created in a project.
-- For information about creating a project, see
-- <https://docs.aws.amazon.com/cloudwatchevidently/latest/APIReference/API_CreateProject.html CreateProject>.
--
-- Don\'t use this operation to update an existing feature. Instead, use
-- <https://docs.aws.amazon.com/cloudwatchevidently/latest/APIReference/API_UpdateFeature.html UpdateFeature>.
module Amazonka.Evidently.CreateFeature
  ( -- * Creating a Request
    CreateFeature (..),
    newCreateFeature,

    -- * Request Lenses
    createFeature_defaultVariation,
    createFeature_description,
    createFeature_entityOverrides,
    createFeature_evaluationStrategy,
    createFeature_tags,
    createFeature_name,
    createFeature_project,
    createFeature_variations,

    -- * Destructuring the Response
    CreateFeatureResponse (..),
    newCreateFeatureResponse,

    -- * Response Lenses
    createFeatureResponse_feature,
    createFeatureResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateFeature' smart constructor.
data CreateFeature = CreateFeature'
  { -- | The name of the variation to use as the default variation. The default
    -- variation is served to users who are not allocated to any ongoing
    -- launches or experiments of this feature.
    --
    -- This variation must also be listed in the @variations@ structure.
    --
    -- If you omit @defaultVariation@, the first variation listed in the
    -- @variations@ structure is used as the default variation.
    defaultVariation :: Prelude.Maybe Prelude.Text,
    -- | An optional description of the feature.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specify users that should always be served a specific variation of a
    -- feature. Each user is specified by a key-value pair . For each key,
    -- specify a user by entering their user ID, account ID, or some other
    -- identifier. For the value, specify the name of the variation that they
    -- are to be served.
    --
    -- This parameter is limited to 2500 overrides or a total of 40KB. The 40KB
    -- limit includes an overhead of 6 bytes per override.
    entityOverrides :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specify @ALL_RULES@ to activate the traffic allocation specified by any
    -- ongoing launches or experiments. Specify @DEFAULT_VARIATION@ to serve
    -- the default variation to all users instead.
    evaluationStrategy :: Prelude.Maybe FeatureEvaluationStrategy,
    -- | Assigns one or more tags (key-value pairs) to the feature.
    --
    -- Tags can help you organize and categorize your resources. You can also
    -- use them to scope user permissions by granting a user permission to
    -- access or change only resources with certain tag values.
    --
    -- Tags don\'t have any semantic meaning to Amazon Web Services and are
    -- interpreted strictly as strings of characters.
    --
    -- You can associate as many as 50 tags with a feature.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name for the new feature.
    name :: Prelude.Text,
    -- | The name or ARN of the project that is to contain the new feature.
    project :: Prelude.Text,
    -- | An array of structures that contain the configuration of the feature\'s
    -- different variations.
    variations :: Prelude.NonEmpty VariationConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFeature' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultVariation', 'createFeature_defaultVariation' - The name of the variation to use as the default variation. The default
-- variation is served to users who are not allocated to any ongoing
-- launches or experiments of this feature.
--
-- This variation must also be listed in the @variations@ structure.
--
-- If you omit @defaultVariation@, the first variation listed in the
-- @variations@ structure is used as the default variation.
--
-- 'description', 'createFeature_description' - An optional description of the feature.
--
-- 'entityOverrides', 'createFeature_entityOverrides' - Specify users that should always be served a specific variation of a
-- feature. Each user is specified by a key-value pair . For each key,
-- specify a user by entering their user ID, account ID, or some other
-- identifier. For the value, specify the name of the variation that they
-- are to be served.
--
-- This parameter is limited to 2500 overrides or a total of 40KB. The 40KB
-- limit includes an overhead of 6 bytes per override.
--
-- 'evaluationStrategy', 'createFeature_evaluationStrategy' - Specify @ALL_RULES@ to activate the traffic allocation specified by any
-- ongoing launches or experiments. Specify @DEFAULT_VARIATION@ to serve
-- the default variation to all users instead.
--
-- 'tags', 'createFeature_tags' - Assigns one or more tags (key-value pairs) to the feature.
--
-- Tags can help you organize and categorize your resources. You can also
-- use them to scope user permissions by granting a user permission to
-- access or change only resources with certain tag values.
--
-- Tags don\'t have any semantic meaning to Amazon Web Services and are
-- interpreted strictly as strings of characters.
--
-- You can associate as many as 50 tags with a feature.
--
-- For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>.
--
-- 'name', 'createFeature_name' - The name for the new feature.
--
-- 'project', 'createFeature_project' - The name or ARN of the project that is to contain the new feature.
--
-- 'variations', 'createFeature_variations' - An array of structures that contain the configuration of the feature\'s
-- different variations.
newCreateFeature ::
  -- | 'name'
  Prelude.Text ->
  -- | 'project'
  Prelude.Text ->
  -- | 'variations'
  Prelude.NonEmpty VariationConfig ->
  CreateFeature
newCreateFeature pName_ pProject_ pVariations_ =
  CreateFeature'
    { defaultVariation = Prelude.Nothing,
      description = Prelude.Nothing,
      entityOverrides = Prelude.Nothing,
      evaluationStrategy = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_,
      project = pProject_,
      variations = Lens.coerced Lens.# pVariations_
    }

-- | The name of the variation to use as the default variation. The default
-- variation is served to users who are not allocated to any ongoing
-- launches or experiments of this feature.
--
-- This variation must also be listed in the @variations@ structure.
--
-- If you omit @defaultVariation@, the first variation listed in the
-- @variations@ structure is used as the default variation.
createFeature_defaultVariation :: Lens.Lens' CreateFeature (Prelude.Maybe Prelude.Text)
createFeature_defaultVariation = Lens.lens (\CreateFeature' {defaultVariation} -> defaultVariation) (\s@CreateFeature' {} a -> s {defaultVariation = a} :: CreateFeature)

-- | An optional description of the feature.
createFeature_description :: Lens.Lens' CreateFeature (Prelude.Maybe Prelude.Text)
createFeature_description = Lens.lens (\CreateFeature' {description} -> description) (\s@CreateFeature' {} a -> s {description = a} :: CreateFeature)

-- | Specify users that should always be served a specific variation of a
-- feature. Each user is specified by a key-value pair . For each key,
-- specify a user by entering their user ID, account ID, or some other
-- identifier. For the value, specify the name of the variation that they
-- are to be served.
--
-- This parameter is limited to 2500 overrides or a total of 40KB. The 40KB
-- limit includes an overhead of 6 bytes per override.
createFeature_entityOverrides :: Lens.Lens' CreateFeature (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createFeature_entityOverrides = Lens.lens (\CreateFeature' {entityOverrides} -> entityOverrides) (\s@CreateFeature' {} a -> s {entityOverrides = a} :: CreateFeature) Prelude.. Lens.mapping Lens.coerced

-- | Specify @ALL_RULES@ to activate the traffic allocation specified by any
-- ongoing launches or experiments. Specify @DEFAULT_VARIATION@ to serve
-- the default variation to all users instead.
createFeature_evaluationStrategy :: Lens.Lens' CreateFeature (Prelude.Maybe FeatureEvaluationStrategy)
createFeature_evaluationStrategy = Lens.lens (\CreateFeature' {evaluationStrategy} -> evaluationStrategy) (\s@CreateFeature' {} a -> s {evaluationStrategy = a} :: CreateFeature)

-- | Assigns one or more tags (key-value pairs) to the feature.
--
-- Tags can help you organize and categorize your resources. You can also
-- use them to scope user permissions by granting a user permission to
-- access or change only resources with certain tag values.
--
-- Tags don\'t have any semantic meaning to Amazon Web Services and are
-- interpreted strictly as strings of characters.
--
-- You can associate as many as 50 tags with a feature.
--
-- For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>.
createFeature_tags :: Lens.Lens' CreateFeature (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createFeature_tags = Lens.lens (\CreateFeature' {tags} -> tags) (\s@CreateFeature' {} a -> s {tags = a} :: CreateFeature) Prelude.. Lens.mapping Lens.coerced

-- | The name for the new feature.
createFeature_name :: Lens.Lens' CreateFeature Prelude.Text
createFeature_name = Lens.lens (\CreateFeature' {name} -> name) (\s@CreateFeature' {} a -> s {name = a} :: CreateFeature)

-- | The name or ARN of the project that is to contain the new feature.
createFeature_project :: Lens.Lens' CreateFeature Prelude.Text
createFeature_project = Lens.lens (\CreateFeature' {project} -> project) (\s@CreateFeature' {} a -> s {project = a} :: CreateFeature)

-- | An array of structures that contain the configuration of the feature\'s
-- different variations.
createFeature_variations :: Lens.Lens' CreateFeature (Prelude.NonEmpty VariationConfig)
createFeature_variations = Lens.lens (\CreateFeature' {variations} -> variations) (\s@CreateFeature' {} a -> s {variations = a} :: CreateFeature) Prelude.. Lens.coerced

instance Core.AWSRequest CreateFeature where
  type
    AWSResponse CreateFeature =
      CreateFeatureResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFeatureResponse'
            Prelude.<$> (x Data..?> "feature")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFeature where
  hashWithSalt _salt CreateFeature' {..} =
    _salt
      `Prelude.hashWithSalt` defaultVariation
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` entityOverrides
      `Prelude.hashWithSalt` evaluationStrategy
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` project
      `Prelude.hashWithSalt` variations

instance Prelude.NFData CreateFeature where
  rnf CreateFeature' {..} =
    Prelude.rnf defaultVariation
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf entityOverrides
      `Prelude.seq` Prelude.rnf evaluationStrategy
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf project
      `Prelude.seq` Prelude.rnf variations

instance Data.ToHeaders CreateFeature where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateFeature where
  toJSON CreateFeature' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("defaultVariation" Data..=)
              Prelude.<$> defaultVariation,
            ("description" Data..=) Prelude.<$> description,
            ("entityOverrides" Data..=)
              Prelude.<$> entityOverrides,
            ("evaluationStrategy" Data..=)
              Prelude.<$> evaluationStrategy,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("variations" Data..= variations)
          ]
      )

instance Data.ToPath CreateFeature where
  toPath CreateFeature' {..} =
    Prelude.mconcat
      ["/projects/", Data.toBS project, "/features"]

instance Data.ToQuery CreateFeature where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFeatureResponse' smart constructor.
data CreateFeatureResponse = CreateFeatureResponse'
  { -- | A structure that contains information about the new feature.
    feature :: Prelude.Maybe Feature,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFeatureResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'feature', 'createFeatureResponse_feature' - A structure that contains information about the new feature.
--
-- 'httpStatus', 'createFeatureResponse_httpStatus' - The response's http status code.
newCreateFeatureResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFeatureResponse
newCreateFeatureResponse pHttpStatus_ =
  CreateFeatureResponse'
    { feature = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that contains information about the new feature.
createFeatureResponse_feature :: Lens.Lens' CreateFeatureResponse (Prelude.Maybe Feature)
createFeatureResponse_feature = Lens.lens (\CreateFeatureResponse' {feature} -> feature) (\s@CreateFeatureResponse' {} a -> s {feature = a} :: CreateFeatureResponse)

-- | The response's http status code.
createFeatureResponse_httpStatus :: Lens.Lens' CreateFeatureResponse Prelude.Int
createFeatureResponse_httpStatus = Lens.lens (\CreateFeatureResponse' {httpStatus} -> httpStatus) (\s@CreateFeatureResponse' {} a -> s {httpStatus = a} :: CreateFeatureResponse)

instance Prelude.NFData CreateFeatureResponse where
  rnf CreateFeatureResponse' {..} =
    Prelude.rnf feature
      `Prelude.seq` Prelude.rnf httpStatus
