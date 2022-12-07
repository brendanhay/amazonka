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
-- Module      : Amazonka.MediaConvert.Types.HlsAdditionalManifest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.HlsAdditionalManifest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify the details for each additional HLS manifest that you want the
-- service to generate for this output group. Each manifest can reference a
-- different subset of outputs in the group.
--
-- /See:/ 'newHlsAdditionalManifest' smart constructor.
data HlsAdditionalManifest = HlsAdditionalManifest'
  { -- | Specify the outputs that you want this additional top-level manifest to
    -- reference.
    selectedOutputs :: Prelude.Maybe [Prelude.Text],
    -- | Specify a name modifier that the service adds to the name of this
    -- manifest to make it different from the file names of the other main
    -- manifests in the output group. For example, say that the default main
    -- manifest for your HLS group is film-name.m3u8. If you enter
    -- \"-no-premium\" for this setting, then the file name the service
    -- generates for this top-level manifest is film-name-no-premium.m3u8. For
    -- HLS output groups, specify a manifestNameModifier that is different from
    -- the nameModifier of the output. The service uses the output name
    -- modifier to create unique names for the individual variant manifests.
    manifestNameModifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HlsAdditionalManifest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'selectedOutputs', 'hlsAdditionalManifest_selectedOutputs' - Specify the outputs that you want this additional top-level manifest to
-- reference.
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
newHlsAdditionalManifest ::
  HlsAdditionalManifest
newHlsAdditionalManifest =
  HlsAdditionalManifest'
    { selectedOutputs =
        Prelude.Nothing,
      manifestNameModifier = Prelude.Nothing
    }

-- | Specify the outputs that you want this additional top-level manifest to
-- reference.
hlsAdditionalManifest_selectedOutputs :: Lens.Lens' HlsAdditionalManifest (Prelude.Maybe [Prelude.Text])
hlsAdditionalManifest_selectedOutputs = Lens.lens (\HlsAdditionalManifest' {selectedOutputs} -> selectedOutputs) (\s@HlsAdditionalManifest' {} a -> s {selectedOutputs = a} :: HlsAdditionalManifest) Prelude.. Lens.mapping Lens.coerced

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

instance Data.FromJSON HlsAdditionalManifest where
  parseJSON =
    Data.withObject
      "HlsAdditionalManifest"
      ( \x ->
          HlsAdditionalManifest'
            Prelude.<$> ( x Data..:? "selectedOutputs"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "manifestNameModifier")
      )

instance Prelude.Hashable HlsAdditionalManifest where
  hashWithSalt _salt HlsAdditionalManifest' {..} =
    _salt `Prelude.hashWithSalt` selectedOutputs
      `Prelude.hashWithSalt` manifestNameModifier

instance Prelude.NFData HlsAdditionalManifest where
  rnf HlsAdditionalManifest' {..} =
    Prelude.rnf selectedOutputs
      `Prelude.seq` Prelude.rnf manifestNameModifier

instance Data.ToJSON HlsAdditionalManifest where
  toJSON HlsAdditionalManifest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("selectedOutputs" Data..=)
              Prelude.<$> selectedOutputs,
            ("manifestNameModifier" Data..=)
              Prelude.<$> manifestNameModifier
          ]
      )
