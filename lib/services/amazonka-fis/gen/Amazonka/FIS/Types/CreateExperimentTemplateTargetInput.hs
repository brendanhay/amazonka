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
-- Module      : Amazonka.FIS.Types.CreateExperimentTemplateTargetInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.CreateExperimentTemplateTargetInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FIS.Types.ExperimentTemplateTargetInputFilter
import qualified Amazonka.Prelude as Prelude

-- | Specifies a target for an experiment. You must specify at least one
-- Amazon Resource Name (ARN) or at least one resource tag. You cannot
-- specify both ARNs and tags.
--
-- For more information, see
-- <https://docs.aws.amazon.com/fis/latest/userguide/targets.html Targets>
-- in the /Fault Injection Simulator User Guide/.
--
-- /See:/ 'newCreateExperimentTemplateTargetInput' smart constructor.
data CreateExperimentTemplateTargetInput = CreateExperimentTemplateTargetInput'
  { -- | The filters to apply to identify target resources using specific
    -- attributes.
    filters :: Prelude.Maybe [ExperimentTemplateTargetInputFilter],
    -- | The tags for the target resources.
    resourceTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The resource type parameters.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Names (ARNs) of the resources.
    resourceArns :: Prelude.Maybe [Prelude.Text],
    -- | The resource type. The resource type must be supported for the specified
    -- action.
    resourceType :: Prelude.Text,
    -- | Scopes the identified resources to a specific count of the resources at
    -- random, or a percentage of the resources. All identified resources are
    -- included in the target.
    --
    -- -   ALL - Run the action on all identified targets. This is the default.
    --
    -- -   COUNT(n) - Run the action on the specified number of targets, chosen
    --     from the identified targets at random. For example, COUNT(1) selects
    --     one of the targets.
    --
    -- -   PERCENT(n) - Run the action on the specified percentage of targets,
    --     chosen from the identified targets at random. For example,
    --     PERCENT(25) selects 25% of the targets.
    selectionMode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateExperimentTemplateTargetInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'createExperimentTemplateTargetInput_filters' - The filters to apply to identify target resources using specific
-- attributes.
--
-- 'resourceTags', 'createExperimentTemplateTargetInput_resourceTags' - The tags for the target resources.
--
-- 'parameters', 'createExperimentTemplateTargetInput_parameters' - The resource type parameters.
--
-- 'resourceArns', 'createExperimentTemplateTargetInput_resourceArns' - The Amazon Resource Names (ARNs) of the resources.
--
-- 'resourceType', 'createExperimentTemplateTargetInput_resourceType' - The resource type. The resource type must be supported for the specified
-- action.
--
-- 'selectionMode', 'createExperimentTemplateTargetInput_selectionMode' - Scopes the identified resources to a specific count of the resources at
-- random, or a percentage of the resources. All identified resources are
-- included in the target.
--
-- -   ALL - Run the action on all identified targets. This is the default.
--
-- -   COUNT(n) - Run the action on the specified number of targets, chosen
--     from the identified targets at random. For example, COUNT(1) selects
--     one of the targets.
--
-- -   PERCENT(n) - Run the action on the specified percentage of targets,
--     chosen from the identified targets at random. For example,
--     PERCENT(25) selects 25% of the targets.
newCreateExperimentTemplateTargetInput ::
  -- | 'resourceType'
  Prelude.Text ->
  -- | 'selectionMode'
  Prelude.Text ->
  CreateExperimentTemplateTargetInput
newCreateExperimentTemplateTargetInput
  pResourceType_
  pSelectionMode_ =
    CreateExperimentTemplateTargetInput'
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
createExperimentTemplateTargetInput_filters :: Lens.Lens' CreateExperimentTemplateTargetInput (Prelude.Maybe [ExperimentTemplateTargetInputFilter])
createExperimentTemplateTargetInput_filters = Lens.lens (\CreateExperimentTemplateTargetInput' {filters} -> filters) (\s@CreateExperimentTemplateTargetInput' {} a -> s {filters = a} :: CreateExperimentTemplateTargetInput) Prelude.. Lens.mapping Lens.coerced

-- | The tags for the target resources.
createExperimentTemplateTargetInput_resourceTags :: Lens.Lens' CreateExperimentTemplateTargetInput (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createExperimentTemplateTargetInput_resourceTags = Lens.lens (\CreateExperimentTemplateTargetInput' {resourceTags} -> resourceTags) (\s@CreateExperimentTemplateTargetInput' {} a -> s {resourceTags = a} :: CreateExperimentTemplateTargetInput) Prelude.. Lens.mapping Lens.coerced

-- | The resource type parameters.
createExperimentTemplateTargetInput_parameters :: Lens.Lens' CreateExperimentTemplateTargetInput (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createExperimentTemplateTargetInput_parameters = Lens.lens (\CreateExperimentTemplateTargetInput' {parameters} -> parameters) (\s@CreateExperimentTemplateTargetInput' {} a -> s {parameters = a} :: CreateExperimentTemplateTargetInput) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Names (ARNs) of the resources.
createExperimentTemplateTargetInput_resourceArns :: Lens.Lens' CreateExperimentTemplateTargetInput (Prelude.Maybe [Prelude.Text])
createExperimentTemplateTargetInput_resourceArns = Lens.lens (\CreateExperimentTemplateTargetInput' {resourceArns} -> resourceArns) (\s@CreateExperimentTemplateTargetInput' {} a -> s {resourceArns = a} :: CreateExperimentTemplateTargetInput) Prelude.. Lens.mapping Lens.coerced

-- | The resource type. The resource type must be supported for the specified
-- action.
createExperimentTemplateTargetInput_resourceType :: Lens.Lens' CreateExperimentTemplateTargetInput Prelude.Text
createExperimentTemplateTargetInput_resourceType = Lens.lens (\CreateExperimentTemplateTargetInput' {resourceType} -> resourceType) (\s@CreateExperimentTemplateTargetInput' {} a -> s {resourceType = a} :: CreateExperimentTemplateTargetInput)

-- | Scopes the identified resources to a specific count of the resources at
-- random, or a percentage of the resources. All identified resources are
-- included in the target.
--
-- -   ALL - Run the action on all identified targets. This is the default.
--
-- -   COUNT(n) - Run the action on the specified number of targets, chosen
--     from the identified targets at random. For example, COUNT(1) selects
--     one of the targets.
--
-- -   PERCENT(n) - Run the action on the specified percentage of targets,
--     chosen from the identified targets at random. For example,
--     PERCENT(25) selects 25% of the targets.
createExperimentTemplateTargetInput_selectionMode :: Lens.Lens' CreateExperimentTemplateTargetInput Prelude.Text
createExperimentTemplateTargetInput_selectionMode = Lens.lens (\CreateExperimentTemplateTargetInput' {selectionMode} -> selectionMode) (\s@CreateExperimentTemplateTargetInput' {} a -> s {selectionMode = a} :: CreateExperimentTemplateTargetInput)

instance
  Prelude.Hashable
    CreateExperimentTemplateTargetInput
  where
  hashWithSalt
    _salt
    CreateExperimentTemplateTargetInput' {..} =
      _salt `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` resourceTags
        `Prelude.hashWithSalt` parameters
        `Prelude.hashWithSalt` resourceArns
        `Prelude.hashWithSalt` resourceType
        `Prelude.hashWithSalt` selectionMode

instance
  Prelude.NFData
    CreateExperimentTemplateTargetInput
  where
  rnf CreateExperimentTemplateTargetInput' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf resourceTags
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf resourceArns
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf selectionMode

instance
  Data.ToJSON
    CreateExperimentTemplateTargetInput
  where
  toJSON CreateExperimentTemplateTargetInput' {..} =
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
