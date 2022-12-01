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
-- Module      : Amazonka.Evidently.UpdateFeature
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing feature.
--
-- You can\'t use this operation to update the tags of an existing feature.
-- Instead, use
-- <https://docs.aws.amazon.com/cloudwatchevidently/latest/APIReference/API_TagResource.html TagResource>.
module Amazonka.Evidently.UpdateFeature
  ( -- * Creating a Request
    UpdateFeature (..),
    newUpdateFeature,

    -- * Request Lenses
    updateFeature_evaluationStrategy,
    updateFeature_description,
    updateFeature_addOrUpdateVariations,
    updateFeature_entityOverrides,
    updateFeature_removeVariations,
    updateFeature_defaultVariation,
    updateFeature_feature,
    updateFeature_project,

    -- * Destructuring the Response
    UpdateFeatureResponse (..),
    newUpdateFeatureResponse,

    -- * Response Lenses
    updateFeatureResponse_httpStatus,
    updateFeatureResponse_feature,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Evidently.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateFeature' smart constructor.
data UpdateFeature = UpdateFeature'
  { -- | Specify @ALL_RULES@ to activate the traffic allocation specified by any
    -- ongoing launches or experiments. Specify @DEFAULT_VARIATION@ to serve
    -- the default variation to all users instead.
    evaluationStrategy :: Prelude.Maybe FeatureEvaluationStrategy,
    -- | An optional description of the feature.
    description :: Prelude.Maybe Prelude.Text,
    -- | To update variation configurations for this feature, or add new ones,
    -- specify this structure. In this array, include any variations that you
    -- want to add or update. If the array includes a variation name that
    -- already exists for this feature, it is updated. If it includes a new
    -- variation name, it is added as a new variation.
    addOrUpdateVariations :: Prelude.Maybe (Prelude.NonEmpty VariationConfig),
    -- | Specified users that should always be served a specific variation of a
    -- feature. Each user is specified by a key-value pair . For each key,
    -- specify a user by entering their user ID, account ID, or some other
    -- identifier. For the value, specify the name of the variation that they
    -- are to be served.
    entityOverrides :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Removes a variation from the feature. If the variation you specify
    -- doesn\'t exist, then this makes no change and does not report an error.
    --
    -- This operation fails if you try to remove a variation that is part of an
    -- ongoing launch or experiment.
    removeVariations :: Prelude.Maybe [Prelude.Text],
    -- | The name of the variation to use as the default variation. The default
    -- variation is served to users who are not allocated to any ongoing
    -- launches or experiments of this feature.
    defaultVariation :: Prelude.Maybe Prelude.Text,
    -- | The name of the feature to be updated.
    feature :: Prelude.Text,
    -- | The name or ARN of the project that contains the feature to be updated.
    project :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFeature' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evaluationStrategy', 'updateFeature_evaluationStrategy' - Specify @ALL_RULES@ to activate the traffic allocation specified by any
-- ongoing launches or experiments. Specify @DEFAULT_VARIATION@ to serve
-- the default variation to all users instead.
--
-- 'description', 'updateFeature_description' - An optional description of the feature.
--
-- 'addOrUpdateVariations', 'updateFeature_addOrUpdateVariations' - To update variation configurations for this feature, or add new ones,
-- specify this structure. In this array, include any variations that you
-- want to add or update. If the array includes a variation name that
-- already exists for this feature, it is updated. If it includes a new
-- variation name, it is added as a new variation.
--
-- 'entityOverrides', 'updateFeature_entityOverrides' - Specified users that should always be served a specific variation of a
-- feature. Each user is specified by a key-value pair . For each key,
-- specify a user by entering their user ID, account ID, or some other
-- identifier. For the value, specify the name of the variation that they
-- are to be served.
--
-- 'removeVariations', 'updateFeature_removeVariations' - Removes a variation from the feature. If the variation you specify
-- doesn\'t exist, then this makes no change and does not report an error.
--
-- This operation fails if you try to remove a variation that is part of an
-- ongoing launch or experiment.
--
-- 'defaultVariation', 'updateFeature_defaultVariation' - The name of the variation to use as the default variation. The default
-- variation is served to users who are not allocated to any ongoing
-- launches or experiments of this feature.
--
-- 'feature', 'updateFeature_feature' - The name of the feature to be updated.
--
-- 'project', 'updateFeature_project' - The name or ARN of the project that contains the feature to be updated.
newUpdateFeature ::
  -- | 'feature'
  Prelude.Text ->
  -- | 'project'
  Prelude.Text ->
  UpdateFeature
newUpdateFeature pFeature_ pProject_ =
  UpdateFeature'
    { evaluationStrategy =
        Prelude.Nothing,
      description = Prelude.Nothing,
      addOrUpdateVariations = Prelude.Nothing,
      entityOverrides = Prelude.Nothing,
      removeVariations = Prelude.Nothing,
      defaultVariation = Prelude.Nothing,
      feature = pFeature_,
      project = pProject_
    }

-- | Specify @ALL_RULES@ to activate the traffic allocation specified by any
-- ongoing launches or experiments. Specify @DEFAULT_VARIATION@ to serve
-- the default variation to all users instead.
updateFeature_evaluationStrategy :: Lens.Lens' UpdateFeature (Prelude.Maybe FeatureEvaluationStrategy)
updateFeature_evaluationStrategy = Lens.lens (\UpdateFeature' {evaluationStrategy} -> evaluationStrategy) (\s@UpdateFeature' {} a -> s {evaluationStrategy = a} :: UpdateFeature)

-- | An optional description of the feature.
updateFeature_description :: Lens.Lens' UpdateFeature (Prelude.Maybe Prelude.Text)
updateFeature_description = Lens.lens (\UpdateFeature' {description} -> description) (\s@UpdateFeature' {} a -> s {description = a} :: UpdateFeature)

-- | To update variation configurations for this feature, or add new ones,
-- specify this structure. In this array, include any variations that you
-- want to add or update. If the array includes a variation name that
-- already exists for this feature, it is updated. If it includes a new
-- variation name, it is added as a new variation.
updateFeature_addOrUpdateVariations :: Lens.Lens' UpdateFeature (Prelude.Maybe (Prelude.NonEmpty VariationConfig))
updateFeature_addOrUpdateVariations = Lens.lens (\UpdateFeature' {addOrUpdateVariations} -> addOrUpdateVariations) (\s@UpdateFeature' {} a -> s {addOrUpdateVariations = a} :: UpdateFeature) Prelude.. Lens.mapping Lens.coerced

-- | Specified users that should always be served a specific variation of a
-- feature. Each user is specified by a key-value pair . For each key,
-- specify a user by entering their user ID, account ID, or some other
-- identifier. For the value, specify the name of the variation that they
-- are to be served.
updateFeature_entityOverrides :: Lens.Lens' UpdateFeature (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateFeature_entityOverrides = Lens.lens (\UpdateFeature' {entityOverrides} -> entityOverrides) (\s@UpdateFeature' {} a -> s {entityOverrides = a} :: UpdateFeature) Prelude.. Lens.mapping Lens.coerced

-- | Removes a variation from the feature. If the variation you specify
-- doesn\'t exist, then this makes no change and does not report an error.
--
-- This operation fails if you try to remove a variation that is part of an
-- ongoing launch or experiment.
updateFeature_removeVariations :: Lens.Lens' UpdateFeature (Prelude.Maybe [Prelude.Text])
updateFeature_removeVariations = Lens.lens (\UpdateFeature' {removeVariations} -> removeVariations) (\s@UpdateFeature' {} a -> s {removeVariations = a} :: UpdateFeature) Prelude.. Lens.mapping Lens.coerced

-- | The name of the variation to use as the default variation. The default
-- variation is served to users who are not allocated to any ongoing
-- launches or experiments of this feature.
updateFeature_defaultVariation :: Lens.Lens' UpdateFeature (Prelude.Maybe Prelude.Text)
updateFeature_defaultVariation = Lens.lens (\UpdateFeature' {defaultVariation} -> defaultVariation) (\s@UpdateFeature' {} a -> s {defaultVariation = a} :: UpdateFeature)

-- | The name of the feature to be updated.
updateFeature_feature :: Lens.Lens' UpdateFeature Prelude.Text
updateFeature_feature = Lens.lens (\UpdateFeature' {feature} -> feature) (\s@UpdateFeature' {} a -> s {feature = a} :: UpdateFeature)

-- | The name or ARN of the project that contains the feature to be updated.
updateFeature_project :: Lens.Lens' UpdateFeature Prelude.Text
updateFeature_project = Lens.lens (\UpdateFeature' {project} -> project) (\s@UpdateFeature' {} a -> s {project = a} :: UpdateFeature)

instance Core.AWSRequest UpdateFeature where
  type
    AWSResponse UpdateFeature =
      UpdateFeatureResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFeatureResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "feature")
      )

instance Prelude.Hashable UpdateFeature where
  hashWithSalt _salt UpdateFeature' {..} =
    _salt `Prelude.hashWithSalt` evaluationStrategy
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` addOrUpdateVariations
      `Prelude.hashWithSalt` entityOverrides
      `Prelude.hashWithSalt` removeVariations
      `Prelude.hashWithSalt` defaultVariation
      `Prelude.hashWithSalt` feature
      `Prelude.hashWithSalt` project

instance Prelude.NFData UpdateFeature where
  rnf UpdateFeature' {..} =
    Prelude.rnf evaluationStrategy
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf addOrUpdateVariations
      `Prelude.seq` Prelude.rnf entityOverrides
      `Prelude.seq` Prelude.rnf removeVariations
      `Prelude.seq` Prelude.rnf defaultVariation
      `Prelude.seq` Prelude.rnf feature
      `Prelude.seq` Prelude.rnf project

instance Core.ToHeaders UpdateFeature where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateFeature where
  toJSON UpdateFeature' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("evaluationStrategy" Core..=)
              Prelude.<$> evaluationStrategy,
            ("description" Core..=) Prelude.<$> description,
            ("addOrUpdateVariations" Core..=)
              Prelude.<$> addOrUpdateVariations,
            ("entityOverrides" Core..=)
              Prelude.<$> entityOverrides,
            ("removeVariations" Core..=)
              Prelude.<$> removeVariations,
            ("defaultVariation" Core..=)
              Prelude.<$> defaultVariation
          ]
      )

instance Core.ToPath UpdateFeature where
  toPath UpdateFeature' {..} =
    Prelude.mconcat
      [ "/projects/",
        Core.toBS project,
        "/features/",
        Core.toBS feature
      ]

instance Core.ToQuery UpdateFeature where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFeatureResponse' smart constructor.
data UpdateFeatureResponse = UpdateFeatureResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A structure that contains information about the updated feature.
    feature :: Feature
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFeatureResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateFeatureResponse_httpStatus' - The response's http status code.
--
-- 'feature', 'updateFeatureResponse_feature' - A structure that contains information about the updated feature.
newUpdateFeatureResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'feature'
  Feature ->
  UpdateFeatureResponse
newUpdateFeatureResponse pHttpStatus_ pFeature_ =
  UpdateFeatureResponse'
    { httpStatus = pHttpStatus_,
      feature = pFeature_
    }

-- | The response's http status code.
updateFeatureResponse_httpStatus :: Lens.Lens' UpdateFeatureResponse Prelude.Int
updateFeatureResponse_httpStatus = Lens.lens (\UpdateFeatureResponse' {httpStatus} -> httpStatus) (\s@UpdateFeatureResponse' {} a -> s {httpStatus = a} :: UpdateFeatureResponse)

-- | A structure that contains information about the updated feature.
updateFeatureResponse_feature :: Lens.Lens' UpdateFeatureResponse Feature
updateFeatureResponse_feature = Lens.lens (\UpdateFeatureResponse' {feature} -> feature) (\s@UpdateFeatureResponse' {} a -> s {feature = a} :: UpdateFeatureResponse)

instance Prelude.NFData UpdateFeatureResponse where
  rnf UpdateFeatureResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf feature
