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
-- Module      : Amazonka.FIS.Types.UpdateExperimentTemplateTargetInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.UpdateExperimentTemplateTargetInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FIS.Types.ExperimentTemplateTargetInputFilter
import qualified Amazonka.Prelude as Prelude

-- | Specifies a target for an experiment. You must specify at least one
-- Amazon Resource Name (ARN) or at least one resource tag. You cannot
-- specify both.
--
-- /See:/ 'newUpdateExperimentTemplateTargetInput' smart constructor.
data UpdateExperimentTemplateTargetInput = UpdateExperimentTemplateTargetInput'
  { -- | The filters to apply to identify target resources using specific
    -- attributes.
    filters :: Prelude.Maybe [ExperimentTemplateTargetInputFilter],
    -- | The tags for the target resources.
    resourceTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The resource type parameters.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Names (ARNs) of the targets.
    resourceArns :: Prelude.Maybe [Prelude.Text],
    -- | The resource type. The resource type must be supported for the specified
    -- action.
    resourceType :: Prelude.Text,
    -- | Scopes the identified resources to a specific count or percentage.
    selectionMode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateExperimentTemplateTargetInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'updateExperimentTemplateTargetInput_filters' - The filters to apply to identify target resources using specific
-- attributes.
--
-- 'resourceTags', 'updateExperimentTemplateTargetInput_resourceTags' - The tags for the target resources.
--
-- 'parameters', 'updateExperimentTemplateTargetInput_parameters' - The resource type parameters.
--
-- 'resourceArns', 'updateExperimentTemplateTargetInput_resourceArns' - The Amazon Resource Names (ARNs) of the targets.
--
-- 'resourceType', 'updateExperimentTemplateTargetInput_resourceType' - The resource type. The resource type must be supported for the specified
-- action.
--
-- 'selectionMode', 'updateExperimentTemplateTargetInput_selectionMode' - Scopes the identified resources to a specific count or percentage.
newUpdateExperimentTemplateTargetInput ::
  -- | 'resourceType'
  Prelude.Text ->
  -- | 'selectionMode'
  Prelude.Text ->
  UpdateExperimentTemplateTargetInput
newUpdateExperimentTemplateTargetInput
  pResourceType_
  pSelectionMode_ =
    UpdateExperimentTemplateTargetInput'
      { filters =
          Prelude.Nothing,
        resourceTags = Prelude.Nothing,
        parameters = Prelude.Nothing,
        resourceArns = Prelude.Nothing,
        resourceType = pResourceType_,
        selectionMode = pSelectionMode_
      }

-- | The filters to apply to identify target resources using specific
-- attributes.
updateExperimentTemplateTargetInput_filters :: Lens.Lens' UpdateExperimentTemplateTargetInput (Prelude.Maybe [ExperimentTemplateTargetInputFilter])
updateExperimentTemplateTargetInput_filters = Lens.lens (\UpdateExperimentTemplateTargetInput' {filters} -> filters) (\s@UpdateExperimentTemplateTargetInput' {} a -> s {filters = a} :: UpdateExperimentTemplateTargetInput) Prelude.. Lens.mapping Lens.coerced

-- | The tags for the target resources.
updateExperimentTemplateTargetInput_resourceTags :: Lens.Lens' UpdateExperimentTemplateTargetInput (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateExperimentTemplateTargetInput_resourceTags = Lens.lens (\UpdateExperimentTemplateTargetInput' {resourceTags} -> resourceTags) (\s@UpdateExperimentTemplateTargetInput' {} a -> s {resourceTags = a} :: UpdateExperimentTemplateTargetInput) Prelude.. Lens.mapping Lens.coerced

-- | The resource type parameters.
updateExperimentTemplateTargetInput_parameters :: Lens.Lens' UpdateExperimentTemplateTargetInput (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateExperimentTemplateTargetInput_parameters = Lens.lens (\UpdateExperimentTemplateTargetInput' {parameters} -> parameters) (\s@UpdateExperimentTemplateTargetInput' {} a -> s {parameters = a} :: UpdateExperimentTemplateTargetInput) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Names (ARNs) of the targets.
updateExperimentTemplateTargetInput_resourceArns :: Lens.Lens' UpdateExperimentTemplateTargetInput (Prelude.Maybe [Prelude.Text])
updateExperimentTemplateTargetInput_resourceArns = Lens.lens (\UpdateExperimentTemplateTargetInput' {resourceArns} -> resourceArns) (\s@UpdateExperimentTemplateTargetInput' {} a -> s {resourceArns = a} :: UpdateExperimentTemplateTargetInput) Prelude.. Lens.mapping Lens.coerced

-- | The resource type. The resource type must be supported for the specified
-- action.
updateExperimentTemplateTargetInput_resourceType :: Lens.Lens' UpdateExperimentTemplateTargetInput Prelude.Text
updateExperimentTemplateTargetInput_resourceType = Lens.lens (\UpdateExperimentTemplateTargetInput' {resourceType} -> resourceType) (\s@UpdateExperimentTemplateTargetInput' {} a -> s {resourceType = a} :: UpdateExperimentTemplateTargetInput)

-- | Scopes the identified resources to a specific count or percentage.
updateExperimentTemplateTargetInput_selectionMode :: Lens.Lens' UpdateExperimentTemplateTargetInput Prelude.Text
updateExperimentTemplateTargetInput_selectionMode = Lens.lens (\UpdateExperimentTemplateTargetInput' {selectionMode} -> selectionMode) (\s@UpdateExperimentTemplateTargetInput' {} a -> s {selectionMode = a} :: UpdateExperimentTemplateTargetInput)

instance
  Prelude.Hashable
    UpdateExperimentTemplateTargetInput
  where
  hashWithSalt
    _salt
    UpdateExperimentTemplateTargetInput' {..} =
      _salt `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` resourceTags
        `Prelude.hashWithSalt` parameters
        `Prelude.hashWithSalt` resourceArns
        `Prelude.hashWithSalt` resourceType
        `Prelude.hashWithSalt` selectionMode

instance
  Prelude.NFData
    UpdateExperimentTemplateTargetInput
  where
  rnf UpdateExperimentTemplateTargetInput' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf resourceTags
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf resourceArns
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf selectionMode

instance
  Data.ToJSON
    UpdateExperimentTemplateTargetInput
  where
  toJSON UpdateExperimentTemplateTargetInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filters" Data..=) Prelude.<$> filters,
            ("resourceTags" Data..=) Prelude.<$> resourceTags,
            ("parameters" Data..=) Prelude.<$> parameters,
            ("resourceArns" Data..=) Prelude.<$> resourceArns,
            Prelude.Just ("resourceType" Data..= resourceType),
            Prelude.Just
              ("selectionMode" Data..= selectionMode)
          ]
      )
