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
-- Module      : Network.AWS.MediaConvert.Types.MsSmoothAdditionalManifest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MsSmoothAdditionalManifest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specify the details for each additional Microsoft Smooth Streaming
-- manifest that you want the service to generate for this output group.
-- Each manifest can reference a different subset of outputs in the group.
--
-- /See:/ 'newMsSmoothAdditionalManifest' smart constructor.
data MsSmoothAdditionalManifest = MsSmoothAdditionalManifest'
  { -- | Specify a name modifier that the service adds to the name of this
    -- manifest to make it different from the file names of the other main
    -- manifests in the output group. For example, say that the default main
    -- manifest for your Microsoft Smooth group is film-name.ismv. If you enter
    -- \"-no-premium\" for this setting, then the file name the service
    -- generates for this top-level manifest is film-name-no-premium.ismv.
    manifestNameModifier :: Core.Maybe Core.Text,
    -- | Specify the outputs that you want this additional top-level manifest to
    -- reference.
    selectedOutputs :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MsSmoothAdditionalManifest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'manifestNameModifier', 'msSmoothAdditionalManifest_manifestNameModifier' - Specify a name modifier that the service adds to the name of this
-- manifest to make it different from the file names of the other main
-- manifests in the output group. For example, say that the default main
-- manifest for your Microsoft Smooth group is film-name.ismv. If you enter
-- \"-no-premium\" for this setting, then the file name the service
-- generates for this top-level manifest is film-name-no-premium.ismv.
--
-- 'selectedOutputs', 'msSmoothAdditionalManifest_selectedOutputs' - Specify the outputs that you want this additional top-level manifest to
-- reference.
newMsSmoothAdditionalManifest ::
  MsSmoothAdditionalManifest
newMsSmoothAdditionalManifest =
  MsSmoothAdditionalManifest'
    { manifestNameModifier =
        Core.Nothing,
      selectedOutputs = Core.Nothing
    }

-- | Specify a name modifier that the service adds to the name of this
-- manifest to make it different from the file names of the other main
-- manifests in the output group. For example, say that the default main
-- manifest for your Microsoft Smooth group is film-name.ismv. If you enter
-- \"-no-premium\" for this setting, then the file name the service
-- generates for this top-level manifest is film-name-no-premium.ismv.
msSmoothAdditionalManifest_manifestNameModifier :: Lens.Lens' MsSmoothAdditionalManifest (Core.Maybe Core.Text)
msSmoothAdditionalManifest_manifestNameModifier = Lens.lens (\MsSmoothAdditionalManifest' {manifestNameModifier} -> manifestNameModifier) (\s@MsSmoothAdditionalManifest' {} a -> s {manifestNameModifier = a} :: MsSmoothAdditionalManifest)

-- | Specify the outputs that you want this additional top-level manifest to
-- reference.
msSmoothAdditionalManifest_selectedOutputs :: Lens.Lens' MsSmoothAdditionalManifest (Core.Maybe [Core.Text])
msSmoothAdditionalManifest_selectedOutputs = Lens.lens (\MsSmoothAdditionalManifest' {selectedOutputs} -> selectedOutputs) (\s@MsSmoothAdditionalManifest' {} a -> s {selectedOutputs = a} :: MsSmoothAdditionalManifest) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON MsSmoothAdditionalManifest where
  parseJSON =
    Core.withObject
      "MsSmoothAdditionalManifest"
      ( \x ->
          MsSmoothAdditionalManifest'
            Core.<$> (x Core..:? "manifestNameModifier")
            Core.<*> (x Core..:? "selectedOutputs" Core..!= Core.mempty)
      )

instance Core.Hashable MsSmoothAdditionalManifest

instance Core.NFData MsSmoothAdditionalManifest

instance Core.ToJSON MsSmoothAdditionalManifest where
  toJSON MsSmoothAdditionalManifest' {..} =
    Core.object
      ( Core.catMaybes
          [ ("manifestNameModifier" Core..=)
              Core.<$> manifestNameModifier,
            ("selectedOutputs" Core..=)
              Core.<$> selectedOutputs
          ]
      )
