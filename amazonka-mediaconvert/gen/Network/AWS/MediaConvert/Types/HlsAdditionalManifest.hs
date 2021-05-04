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
-- Module      : Network.AWS.MediaConvert.Types.HlsAdditionalManifest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsAdditionalManifest where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specify the details for each additional HLS manifest that you want the
-- service to generate for this output group. Each manifest can reference a
-- different subset of outputs in the group.
--
-- /See:/ 'newHlsAdditionalManifest' smart constructor.
data HlsAdditionalManifest = HlsAdditionalManifest'
  { -- | Specify a name modifier that the service adds to the name of this
    -- manifest to make it different from the file names of the other main
    -- manifests in the output group. For example, say that the default main
    -- manifest for your HLS group is film-name.m3u8. If you enter
    -- \"-no-premium\" for this setting, then the file name the service
    -- generates for this top-level manifest is film-name-no-premium.m3u8. For
    -- HLS output groups, specify a manifestNameModifier that is different from
    -- the nameModifier of the output. The service uses the output name
    -- modifier to create unique names for the individual variant manifests.
    manifestNameModifier :: Prelude.Maybe Prelude.Text,
    -- | Specify the outputs that you want this additional top-level manifest to
    -- reference.
    selectedOutputs :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HlsAdditionalManifest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'manifestNameModifier', 'hlsAdditionalManifest_manifestNameModifier' - Specify a name modifier that the service adds to the name of this
-- manifest to make it different from the file names of the other main
-- manifests in the output group. For example, say that the default main
-- manifest for your HLS group is film-name.m3u8. If you enter
-- \"-no-premium\" for this setting, then the file name the service
-- generates for this top-level manifest is film-name-no-premium.m3u8. For
-- HLS output groups, specify a manifestNameModifier that is different from
-- the nameModifier of the output. The service uses the output name
-- modifier to create unique names for the individual variant manifests.
--
-- 'selectedOutputs', 'hlsAdditionalManifest_selectedOutputs' - Specify the outputs that you want this additional top-level manifest to
-- reference.
newHlsAdditionalManifest ::
  HlsAdditionalManifest
newHlsAdditionalManifest =
  HlsAdditionalManifest'
    { manifestNameModifier =
        Prelude.Nothing,
      selectedOutputs = Prelude.Nothing
    }

-- | Specify a name modifier that the service adds to the name of this
-- manifest to make it different from the file names of the other main
-- manifests in the output group. For example, say that the default main
-- manifest for your HLS group is film-name.m3u8. If you enter
-- \"-no-premium\" for this setting, then the file name the service
-- generates for this top-level manifest is film-name-no-premium.m3u8. For
-- HLS output groups, specify a manifestNameModifier that is different from
-- the nameModifier of the output. The service uses the output name
-- modifier to create unique names for the individual variant manifests.
hlsAdditionalManifest_manifestNameModifier :: Lens.Lens' HlsAdditionalManifest (Prelude.Maybe Prelude.Text)
hlsAdditionalManifest_manifestNameModifier = Lens.lens (\HlsAdditionalManifest' {manifestNameModifier} -> manifestNameModifier) (\s@HlsAdditionalManifest' {} a -> s {manifestNameModifier = a} :: HlsAdditionalManifest)

-- | Specify the outputs that you want this additional top-level manifest to
-- reference.
hlsAdditionalManifest_selectedOutputs :: Lens.Lens' HlsAdditionalManifest (Prelude.Maybe [Prelude.Text])
hlsAdditionalManifest_selectedOutputs = Lens.lens (\HlsAdditionalManifest' {selectedOutputs} -> selectedOutputs) (\s@HlsAdditionalManifest' {} a -> s {selectedOutputs = a} :: HlsAdditionalManifest) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON HlsAdditionalManifest where
  parseJSON =
    Prelude.withObject
      "HlsAdditionalManifest"
      ( \x ->
          HlsAdditionalManifest'
            Prelude.<$> (x Prelude..:? "manifestNameModifier")
            Prelude.<*> ( x Prelude..:? "selectedOutputs"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable HlsAdditionalManifest

instance Prelude.NFData HlsAdditionalManifest

instance Prelude.ToJSON HlsAdditionalManifest where
  toJSON HlsAdditionalManifest' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("manifestNameModifier" Prelude..=)
              Prelude.<$> manifestNameModifier,
            ("selectedOutputs" Prelude..=)
              Prelude.<$> selectedOutputs
          ]
      )
