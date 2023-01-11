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
-- Module      : Amazonka.ElasticTranscoder.Types.Captions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticTranscoder.Types.Captions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticTranscoder.Types.CaptionFormat
import Amazonka.ElasticTranscoder.Types.CaptionSource
import qualified Amazonka.Prelude as Prelude

-- | The captions to be created, if any.
--
-- /See:/ 'newCaptions' smart constructor.
data Captions = Captions'
  { -- | The array of file formats for the output captions. If you leave this
    -- value blank, Elastic Transcoder returns an error.
    captionFormats :: Prelude.Maybe [CaptionFormat],
    -- | Source files for the input sidecar captions used during the transcoding
    -- process. To omit all sidecar captions, leave @CaptionSources@ blank.
    captionSources :: Prelude.Maybe [CaptionSource],
    -- | A policy that determines how Elastic Transcoder handles the existence of
    -- multiple captions.
    --
    -- -   __MergeOverride:__ Elastic Transcoder transcodes both embedded and
    --     sidecar captions into outputs. If captions for a language are
    --     embedded in the input file and also appear in a sidecar file,
    --     Elastic Transcoder uses the sidecar captions and ignores the
    --     embedded captions for that language.
    --
    -- -   __MergeRetain:__ Elastic Transcoder transcodes both embedded and
    --     sidecar captions into outputs. If captions for a language are
    --     embedded in the input file and also appear in a sidecar file,
    --     Elastic Transcoder uses the embedded captions and ignores the
    --     sidecar captions for that language. If @CaptionSources@ is empty,
    --     Elastic Transcoder omits all sidecar captions from the output files.
    --
    -- -   __Override:__ Elastic Transcoder transcodes only the sidecar
    --     captions that you specify in @CaptionSources@.
    --
    -- @MergePolicy@ cannot be null.
    mergePolicy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Captions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'captionFormats', 'captions_captionFormats' - The array of file formats for the output captions. If you leave this
-- value blank, Elastic Transcoder returns an error.
--
-- 'captionSources', 'captions_captionSources' - Source files for the input sidecar captions used during the transcoding
-- process. To omit all sidecar captions, leave @CaptionSources@ blank.
--
-- 'mergePolicy', 'captions_mergePolicy' - A policy that determines how Elastic Transcoder handles the existence of
-- multiple captions.
--
-- -   __MergeOverride:__ Elastic Transcoder transcodes both embedded and
--     sidecar captions into outputs. If captions for a language are
--     embedded in the input file and also appear in a sidecar file,
--     Elastic Transcoder uses the sidecar captions and ignores the
--     embedded captions for that language.
--
-- -   __MergeRetain:__ Elastic Transcoder transcodes both embedded and
--     sidecar captions into outputs. If captions for a language are
--     embedded in the input file and also appear in a sidecar file,
--     Elastic Transcoder uses the embedded captions and ignores the
--     sidecar captions for that language. If @CaptionSources@ is empty,
--     Elastic Transcoder omits all sidecar captions from the output files.
--
-- -   __Override:__ Elastic Transcoder transcodes only the sidecar
--     captions that you specify in @CaptionSources@.
--
-- @MergePolicy@ cannot be null.
newCaptions ::
  Captions
newCaptions =
  Captions'
    { captionFormats = Prelude.Nothing,
      captionSources = Prelude.Nothing,
      mergePolicy = Prelude.Nothing
    }

-- | The array of file formats for the output captions. If you leave this
-- value blank, Elastic Transcoder returns an error.
captions_captionFormats :: Lens.Lens' Captions (Prelude.Maybe [CaptionFormat])
captions_captionFormats = Lens.lens (\Captions' {captionFormats} -> captionFormats) (\s@Captions' {} a -> s {captionFormats = a} :: Captions) Prelude.. Lens.mapping Lens.coerced

-- | Source files for the input sidecar captions used during the transcoding
-- process. To omit all sidecar captions, leave @CaptionSources@ blank.
captions_captionSources :: Lens.Lens' Captions (Prelude.Maybe [CaptionSource])
captions_captionSources = Lens.lens (\Captions' {captionSources} -> captionSources) (\s@Captions' {} a -> s {captionSources = a} :: Captions) Prelude.. Lens.mapping Lens.coerced

-- | A policy that determines how Elastic Transcoder handles the existence of
-- multiple captions.
--
-- -   __MergeOverride:__ Elastic Transcoder transcodes both embedded and
--     sidecar captions into outputs. If captions for a language are
--     embedded in the input file and also appear in a sidecar file,
--     Elastic Transcoder uses the sidecar captions and ignores the
--     embedded captions for that language.
--
-- -   __MergeRetain:__ Elastic Transcoder transcodes both embedded and
--     sidecar captions into outputs. If captions for a language are
--     embedded in the input file and also appear in a sidecar file,
--     Elastic Transcoder uses the embedded captions and ignores the
--     sidecar captions for that language. If @CaptionSources@ is empty,
--     Elastic Transcoder omits all sidecar captions from the output files.
--
-- -   __Override:__ Elastic Transcoder transcodes only the sidecar
--     captions that you specify in @CaptionSources@.
--
-- @MergePolicy@ cannot be null.
captions_mergePolicy :: Lens.Lens' Captions (Prelude.Maybe Prelude.Text)
captions_mergePolicy = Lens.lens (\Captions' {mergePolicy} -> mergePolicy) (\s@Captions' {} a -> s {mergePolicy = a} :: Captions)

instance Data.FromJSON Captions where
  parseJSON =
    Data.withObject
      "Captions"
      ( \x ->
          Captions'
            Prelude.<$> (x Data..:? "CaptionFormats" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "CaptionSources" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "MergePolicy")
      )

instance Prelude.Hashable Captions where
  hashWithSalt _salt Captions' {..} =
    _salt `Prelude.hashWithSalt` captionFormats
      `Prelude.hashWithSalt` captionSources
      `Prelude.hashWithSalt` mergePolicy

instance Prelude.NFData Captions where
  rnf Captions' {..} =
    Prelude.rnf captionFormats
      `Prelude.seq` Prelude.rnf captionSources
      `Prelude.seq` Prelude.rnf mergePolicy

instance Data.ToJSON Captions where
  toJSON Captions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CaptionFormats" Data..=)
              Prelude.<$> captionFormats,
            ("CaptionSources" Data..=)
              Prelude.<$> captionSources,
            ("MergePolicy" Data..=) Prelude.<$> mergePolicy
          ]
      )
