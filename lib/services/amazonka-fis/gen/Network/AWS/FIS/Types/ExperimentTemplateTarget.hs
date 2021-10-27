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
-- Module      : Network.AWS.FIS.Types.ExperimentTemplateTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FIS.Types.ExperimentTemplateTarget where

import qualified Network.AWS.Core as Core
import Network.AWS.FIS.Types.ExperimentTemplateTargetFilter
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a target for an experiment template.
--
-- /See:/ 'newExperimentTemplateTarget' smart constructor.
data ExperimentTemplateTarget = ExperimentTemplateTarget'
  { -- | The resource type.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The tags for the target resources.
    resourceTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The filters to apply to identify target resources using specific
    -- attributes.
    filters :: Prelude.Maybe [ExperimentTemplateTargetFilter],
    -- | The Amazon Resource Names (ARNs) of the targets.
    resourceArns :: Prelude.Maybe [Prelude.Text],
    -- | Scopes the identified resources to a specific count or percentage.
    selectionMode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExperimentTemplateTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'experimentTemplateTarget_resourceType' - The resource type.
--
-- 'resourceTags', 'experimentTemplateTarget_resourceTags' - The tags for the target resources.
--
-- 'filters', 'experimentTemplateTarget_filters' - The filters to apply to identify target resources using specific
-- attributes.
--
-- 'resourceArns', 'experimentTemplateTarget_resourceArns' - The Amazon Resource Names (ARNs) of the targets.
--
-- 'selectionMode', 'experimentTemplateTarget_selectionMode' - Scopes the identified resources to a specific count or percentage.
newExperimentTemplateTarget ::
  ExperimentTemplateTarget
newExperimentTemplateTarget =
  ExperimentTemplateTarget'
    { resourceType =
        Prelude.Nothing,
      resourceTags = Prelude.Nothing,
      filters = Prelude.Nothing,
      resourceArns = Prelude.Nothing,
      selectionMode = Prelude.Nothing
    }

-- | The resource type.
experimentTemplateTarget_resourceType :: Lens.Lens' ExperimentTemplateTarget (Prelude.Maybe Prelude.Text)
experimentTemplateTarget_resourceType = Lens.lens (\ExperimentTemplateTarget' {resourceType} -> resourceType) (\s@ExperimentTemplateTarget' {} a -> s {resourceType = a} :: ExperimentTemplateTarget)

-- | The tags for the target resources.
experimentTemplateTarget_resourceTags :: Lens.Lens' ExperimentTemplateTarget (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
experimentTemplateTarget_resourceTags = Lens.lens (\ExperimentTemplateTarget' {resourceTags} -> resourceTags) (\s@ExperimentTemplateTarget' {} a -> s {resourceTags = a} :: ExperimentTemplateTarget) Prelude.. Lens.mapping Lens.coerced

-- | The filters to apply to identify target resources using specific
-- attributes.
experimentTemplateTarget_filters :: Lens.Lens' ExperimentTemplateTarget (Prelude.Maybe [ExperimentTemplateTargetFilter])
experimentTemplateTarget_filters = Lens.lens (\ExperimentTemplateTarget' {filters} -> filters) (\s@ExperimentTemplateTarget' {} a -> s {filters = a} :: ExperimentTemplateTarget) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Names (ARNs) of the targets.
experimentTemplateTarget_resourceArns :: Lens.Lens' ExperimentTemplateTarget (Prelude.Maybe [Prelude.Text])
experimentTemplateTarget_resourceArns = Lens.lens (\ExperimentTemplateTarget' {resourceArns} -> resourceArns) (\s@ExperimentTemplateTarget' {} a -> s {resourceArns = a} :: ExperimentTemplateTarget) Prelude.. Lens.mapping Lens.coerced

-- | Scopes the identified resources to a specific count or percentage.
experimentTemplateTarget_selectionMode :: Lens.Lens' ExperimentTemplateTarget (Prelude.Maybe Prelude.Text)
experimentTemplateTarget_selectionMode = Lens.lens (\ExperimentTemplateTarget' {selectionMode} -> selectionMode) (\s@ExperimentTemplateTarget' {} a -> s {selectionMode = a} :: ExperimentTemplateTarget)

instance Core.FromJSON ExperimentTemplateTarget where
  parseJSON =
    Core.withObject
      "ExperimentTemplateTarget"
      ( \x ->
          ExperimentTemplateTarget'
            Prelude.<$> (x Core..:? "resourceType")
            Prelude.<*> (x Core..:? "resourceTags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "filters" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "resourceArns" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "selectionMode")
      )

instance Prelude.Hashable ExperimentTemplateTarget

instance Prelude.NFData ExperimentTemplateTarget
