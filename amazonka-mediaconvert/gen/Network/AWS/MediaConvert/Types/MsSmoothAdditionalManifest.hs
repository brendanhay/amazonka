{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    manifestNameModifier :: Prelude.Maybe Prelude.Text,
    -- | Specify the outputs that you want this additional top-level manifest to
    -- reference.
    selectedOutputs :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      selectedOutputs = Prelude.Nothing
    }

-- | Specify a name modifier that the service adds to the name of this
-- manifest to make it different from the file names of the other main
-- manifests in the output group. For example, say that the default main
-- manifest for your Microsoft Smooth group is film-name.ismv. If you enter
-- \"-no-premium\" for this setting, then the file name the service
-- generates for this top-level manifest is film-name-no-premium.ismv.
msSmoothAdditionalManifest_manifestNameModifier :: Lens.Lens' MsSmoothAdditionalManifest (Prelude.Maybe Prelude.Text)
msSmoothAdditionalManifest_manifestNameModifier = Lens.lens (\MsSmoothAdditionalManifest' {manifestNameModifier} -> manifestNameModifier) (\s@MsSmoothAdditionalManifest' {} a -> s {manifestNameModifier = a} :: MsSmoothAdditionalManifest)

-- | Specify the outputs that you want this additional top-level manifest to
-- reference.
msSmoothAdditionalManifest_selectedOutputs :: Lens.Lens' MsSmoothAdditionalManifest (Prelude.Maybe [Prelude.Text])
msSmoothAdditionalManifest_selectedOutputs = Lens.lens (\MsSmoothAdditionalManifest' {selectedOutputs} -> selectedOutputs) (\s@MsSmoothAdditionalManifest' {} a -> s {selectedOutputs = a} :: MsSmoothAdditionalManifest) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON MsSmoothAdditionalManifest where
  parseJSON =
    Prelude.withObject
      "MsSmoothAdditionalManifest"
      ( \x ->
          MsSmoothAdditionalManifest'
            Prelude.<$> (x Prelude..:? "manifestNameModifier")
            Prelude.<*> ( x Prelude..:? "selectedOutputs"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable MsSmoothAdditionalManifest

instance Prelude.NFData MsSmoothAdditionalManifest

instance Prelude.ToJSON MsSmoothAdditionalManifest where
  toJSON MsSmoothAdditionalManifest' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("manifestNameModifier" Prelude..=)
              Prelude.<$> manifestNameModifier,
            ("selectedOutputs" Prelude..=)
              Prelude.<$> selectedOutputs
          ]
      )
