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
-- Module      : Network.AWS.ElasticTranscoder.Types.Captions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.Captions where

import Network.AWS.ElasticTranscoder.Types.CaptionFormat
import Network.AWS.ElasticTranscoder.Types.CaptionSource
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The captions to be created, if any.
--
-- /See:/ 'newCaptions' smart constructor.
data Captions = Captions'
  { -- | Source files for the input sidecar captions used during the transcoding
    -- process. To omit all sidecar captions, leave @CaptionSources@ blank.
    captionSources :: Prelude.Maybe [CaptionSource],
    -- | The array of file formats for the output captions. If you leave this
    -- value blank, Elastic Transcoder returns an error.
    captionFormats :: Prelude.Maybe [CaptionFormat],
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Captions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'captionSources', 'captions_captionSources' - Source files for the input sidecar captions used during the transcoding
-- process. To omit all sidecar captions, leave @CaptionSources@ blank.
--
-- 'captionFormats', 'captions_captionFormats' - The array of file formats for the output captions. If you leave this
-- value blank, Elastic Transcoder returns an error.
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
    { captionSources = Prelude.Nothing,
      captionFormats = Prelude.Nothing,
      mergePolicy = Prelude.Nothing
    }

-- | Source files for the input sidecar captions used during the transcoding
-- process. To omit all sidecar captions, leave @CaptionSources@ blank.
captions_captionSources :: Lens.Lens' Captions (Prelude.Maybe [CaptionSource])
captions_captionSources = Lens.lens (\Captions' {captionSources} -> captionSources) (\s@Captions' {} a -> s {captionSources = a} :: Captions) Prelude.. Lens.mapping Prelude._Coerce

-- | The array of file formats for the output captions. If you leave this
-- value blank, Elastic Transcoder returns an error.
captions_captionFormats :: Lens.Lens' Captions (Prelude.Maybe [CaptionFormat])
captions_captionFormats = Lens.lens (\Captions' {captionFormats} -> captionFormats) (\s@Captions' {} a -> s {captionFormats = a} :: Captions) Prelude.. Lens.mapping Prelude._Coerce

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

instance Prelude.FromJSON Captions where
  parseJSON =
    Prelude.withObject
      "Captions"
      ( \x ->
          Captions'
            Prelude.<$> ( x Prelude..:? "CaptionSources"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "CaptionFormats"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "MergePolicy")
      )

instance Prelude.Hashable Captions

instance Prelude.NFData Captions

instance Prelude.ToJSON Captions where
  toJSON Captions' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CaptionSources" Prelude..=)
              Prelude.<$> captionSources,
            ("CaptionFormats" Prelude..=)
              Prelude.<$> captionFormats,
            ("MergePolicy" Prelude..=) Prelude.<$> mergePolicy
          ]
      )
