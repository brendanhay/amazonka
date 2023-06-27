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
-- Module      : Amazonka.ElasticTranscoder.Types.InputCaptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticTranscoder.Types.InputCaptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticTranscoder.Types.CaptionSource
import qualified Amazonka.Prelude as Prelude

-- | The captions to be created, if any.
--
-- /See:/ 'newInputCaptions' smart constructor.
data InputCaptions = InputCaptions'
  { -- | Source files for the input sidecar captions used during the transcoding
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
-- Create a value of 'InputCaptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'captionSources', 'inputCaptions_captionSources' - Source files for the input sidecar captions used during the transcoding
-- process. To omit all sidecar captions, leave @CaptionSources@ blank.
--
-- 'mergePolicy', 'inputCaptions_mergePolicy' - A policy that determines how Elastic Transcoder handles the existence of
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
newInputCaptions ::
  InputCaptions
newInputCaptions =
  InputCaptions'
    { captionSources = Prelude.Nothing,
      mergePolicy = Prelude.Nothing
    }

-- | Source files for the input sidecar captions used during the transcoding
-- process. To omit all sidecar captions, leave @CaptionSources@ blank.
inputCaptions_captionSources :: Lens.Lens' InputCaptions (Prelude.Maybe [CaptionSource])
inputCaptions_captionSources = Lens.lens (\InputCaptions' {captionSources} -> captionSources) (\s@InputCaptions' {} a -> s {captionSources = a} :: InputCaptions) Prelude.. Lens.mapping Lens.coerced

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
inputCaptions_mergePolicy :: Lens.Lens' InputCaptions (Prelude.Maybe Prelude.Text)
inputCaptions_mergePolicy = Lens.lens (\InputCaptions' {mergePolicy} -> mergePolicy) (\s@InputCaptions' {} a -> s {mergePolicy = a} :: InputCaptions)

instance Data.FromJSON InputCaptions where
  parseJSON =
    Data.withObject
      "InputCaptions"
      ( \x ->
          InputCaptions'
            Prelude.<$> (x Data..:? "CaptionSources" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "MergePolicy")
      )

instance Prelude.Hashable InputCaptions where
  hashWithSalt _salt InputCaptions' {..} =
    _salt
      `Prelude.hashWithSalt` captionSources
      `Prelude.hashWithSalt` mergePolicy

instance Prelude.NFData InputCaptions where
  rnf InputCaptions' {..} =
    Prelude.rnf captionSources
      `Prelude.seq` Prelude.rnf mergePolicy

instance Data.ToJSON InputCaptions where
  toJSON InputCaptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CaptionSources" Data..=)
              Prelude.<$> captionSources,
            ("MergePolicy" Data..=) Prelude.<$> mergePolicy
          ]
      )
