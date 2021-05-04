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
-- Module      : Network.AWS.MediaConvert.Types.DashAdditionalManifest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DashAdditionalManifest where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specify the details for each additional DASH manifest that you want the
-- service to generate for this output group. Each manifest can reference a
-- different subset of outputs in the group.
--
-- /See:/ 'newDashAdditionalManifest' smart constructor.
data DashAdditionalManifest = DashAdditionalManifest'
  { -- | Specify a name modifier that the service adds to the name of this
    -- manifest to make it different from the file names of the other main
    -- manifests in the output group. For example, say that the default main
    -- manifest for your DASH group is film-name.mpd. If you enter
    -- \"-no-premium\" for this setting, then the file name the service
    -- generates for this top-level manifest is film-name-no-premium.mpd.
    manifestNameModifier :: Prelude.Maybe Prelude.Text,
    -- | Specify the outputs that you want this additional top-level manifest to
    -- reference.
    selectedOutputs :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DashAdditionalManifest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'manifestNameModifier', 'dashAdditionalManifest_manifestNameModifier' - Specify a name modifier that the service adds to the name of this
-- manifest to make it different from the file names of the other main
-- manifests in the output group. For example, say that the default main
-- manifest for your DASH group is film-name.mpd. If you enter
-- \"-no-premium\" for this setting, then the file name the service
-- generates for this top-level manifest is film-name-no-premium.mpd.
--
-- 'selectedOutputs', 'dashAdditionalManifest_selectedOutputs' - Specify the outputs that you want this additional top-level manifest to
-- reference.
newDashAdditionalManifest ::
  DashAdditionalManifest
newDashAdditionalManifest =
  DashAdditionalManifest'
    { manifestNameModifier =
        Prelude.Nothing,
      selectedOutputs = Prelude.Nothing
    }

-- | Specify a name modifier that the service adds to the name of this
-- manifest to make it different from the file names of the other main
-- manifests in the output group. For example, say that the default main
-- manifest for your DASH group is film-name.mpd. If you enter
-- \"-no-premium\" for this setting, then the file name the service
-- generates for this top-level manifest is film-name-no-premium.mpd.
dashAdditionalManifest_manifestNameModifier :: Lens.Lens' DashAdditionalManifest (Prelude.Maybe Prelude.Text)
dashAdditionalManifest_manifestNameModifier = Lens.lens (\DashAdditionalManifest' {manifestNameModifier} -> manifestNameModifier) (\s@DashAdditionalManifest' {} a -> s {manifestNameModifier = a} :: DashAdditionalManifest)

-- | Specify the outputs that you want this additional top-level manifest to
-- reference.
dashAdditionalManifest_selectedOutputs :: Lens.Lens' DashAdditionalManifest (Prelude.Maybe [Prelude.Text])
dashAdditionalManifest_selectedOutputs = Lens.lens (\DashAdditionalManifest' {selectedOutputs} -> selectedOutputs) (\s@DashAdditionalManifest' {} a -> s {selectedOutputs = a} :: DashAdditionalManifest) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON DashAdditionalManifest where
  parseJSON =
    Prelude.withObject
      "DashAdditionalManifest"
      ( \x ->
          DashAdditionalManifest'
            Prelude.<$> (x Prelude..:? "manifestNameModifier")
            Prelude.<*> ( x Prelude..:? "selectedOutputs"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable DashAdditionalManifest

instance Prelude.NFData DashAdditionalManifest

instance Prelude.ToJSON DashAdditionalManifest where
  toJSON DashAdditionalManifest' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("manifestNameModifier" Prelude..=)
              Prelude.<$> manifestNameModifier,
            ("selectedOutputs" Prelude..=)
              Prelude.<$> selectedOutputs
          ]
      )
