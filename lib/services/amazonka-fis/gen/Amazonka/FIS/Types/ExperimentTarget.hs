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
-- Module      : Amazonka.FIS.Types.ExperimentTarget
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.ExperimentTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FIS.Types.ExperimentTargetFilter
import qualified Amazonka.Prelude as Prelude

-- | Describes a target for an experiment.
--
-- /See:/ 'newExperimentTarget' smart constructor.
data ExperimentTarget = ExperimentTarget'
  { -- | The filters to apply to identify target resources using specific
    -- attributes.
    filters :: Prelude.Maybe [ExperimentTargetFilter],
    -- | The resource type parameters.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Names (ARNs) of the resources.
    resourceArns :: Prelude.Maybe [Prelude.Text],
    -- | The tags for the target resources.
    resourceTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The resource type.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | Scopes the identified resources to a specific count or percentage.
    selectionMode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExperimentTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'experimentTarget_filters' - The filters to apply to identify target resources using specific
-- attributes.
--
-- 'parameters', 'experimentTarget_parameters' - The resource type parameters.
--
-- 'resourceArns', 'experimentTarget_resourceArns' - The Amazon Resource Names (ARNs) of the resources.
--
-- 'resourceTags', 'experimentTarget_resourceTags' - The tags for the target resources.
--
-- 'resourceType', 'experimentTarget_resourceType' - The resource type.
--
-- 'selectionMode', 'experimentTarget_selectionMode' - Scopes the identified resources to a specific count or percentage.
newExperimentTarget ::
  ExperimentTarget
newExperimentTarget =
  ExperimentTarget'
    { filters = Prelude.Nothing,
      parameters = Prelude.Nothing,
      resourceArns = Prelude.Nothing,
      resourceTags = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      selectionMode = Prelude.Nothing
    }

-- | The filters to apply to identify target resources using specific
-- attributes.
experimentTarget_filters :: Lens.Lens' ExperimentTarget (Prelude.Maybe [ExperimentTargetFilter])
experimentTarget_filters = Lens.lens (\ExperimentTarget' {filters} -> filters) (\s@ExperimentTarget' {} a -> s {filters = a} :: ExperimentTarget) Prelude.. Lens.mapping Lens.coerced

-- | The resource type parameters.
experimentTarget_parameters :: Lens.Lens' ExperimentTarget (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
experimentTarget_parameters = Lens.lens (\ExperimentTarget' {parameters} -> parameters) (\s@ExperimentTarget' {} a -> s {parameters = a} :: ExperimentTarget) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Names (ARNs) of the resources.
experimentTarget_resourceArns :: Lens.Lens' ExperimentTarget (Prelude.Maybe [Prelude.Text])
experimentTarget_resourceArns = Lens.lens (\ExperimentTarget' {resourceArns} -> resourceArns) (\s@ExperimentTarget' {} a -> s {resourceArns = a} :: ExperimentTarget) Prelude.. Lens.mapping Lens.coerced

-- | The tags for the target resources.
experimentTarget_resourceTags :: Lens.Lens' ExperimentTarget (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
experimentTarget_resourceTags = Lens.lens (\ExperimentTarget' {resourceTags} -> resourceTags) (\s@ExperimentTarget' {} a -> s {resourceTags = a} :: ExperimentTarget) Prelude.. Lens.mapping Lens.coerced

-- | The resource type.
experimentTarget_resourceType :: Lens.Lens' ExperimentTarget (Prelude.Maybe Prelude.Text)
experimentTarget_resourceType = Lens.lens (\ExperimentTarget' {resourceType} -> resourceType) (\s@ExperimentTarget' {} a -> s {resourceType = a} :: ExperimentTarget)

-- | Scopes the identified resources to a specific count or percentage.
experimentTarget_selectionMode :: Lens.Lens' ExperimentTarget (Prelude.Maybe Prelude.Text)
experimentTarget_selectionMode = Lens.lens (\ExperimentTarget' {selectionMode} -> selectionMode) (\s@ExperimentTarget' {} a -> s {selectionMode = a} :: ExperimentTarget)

instance Data.FromJSON ExperimentTarget where
  parseJSON =
    Data.withObject
      "ExperimentTarget"
      ( \x ->
          ExperimentTarget'
            Prelude.<$> (x Data..:? "filters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "parameters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "resourceArns" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "resourceTags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "resourceType")
            Prelude.<*> (x Data..:? "selectionMode")
      )

instance Prelude.Hashable ExperimentTarget where
  hashWithSalt _salt ExperimentTarget' {..} =
    _salt `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` resourceArns
      `Prelude.hashWithSalt` resourceTags
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` selectionMode

instance Prelude.NFData ExperimentTarget where
  rnf ExperimentTarget' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf resourceArns
      `Prelude.seq` Prelude.rnf resourceTags
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf selectionMode
