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
-- Module      : Network.AWS.ElasticTranscoder.Types.InputCaptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.InputCaptions where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticTranscoder.Types.CaptionSource
import qualified Network.AWS.Lens as Lens

-- | The captions to be created, if any.
--
-- /See:/ 'newInputCaptions' smart constructor.
data InputCaptions = InputCaptions'
  { -- | Source files for the input sidecar captions used during the transcoding
    -- process. To omit all sidecar captions, leave @CaptionSources@ blank.
    captionSources :: Core.Maybe [CaptionSource],
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
    mergePolicy :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { captionSources = Core.Nothing,
      mergePolicy = Core.Nothing
    }

-- | Source files for the input sidecar captions used during the transcoding
-- process. To omit all sidecar captions, leave @CaptionSources@ blank.
inputCaptions_captionSources :: Lens.Lens' InputCaptions (Core.Maybe [CaptionSource])
inputCaptions_captionSources = Lens.lens (\InputCaptions' {captionSources} -> captionSources) (\s@InputCaptions' {} a -> s {captionSources = a} :: InputCaptions) Core.. Lens.mapping Lens._Coerce

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
inputCaptions_mergePolicy :: Lens.Lens' InputCaptions (Core.Maybe Core.Text)
inputCaptions_mergePolicy = Lens.lens (\InputCaptions' {mergePolicy} -> mergePolicy) (\s@InputCaptions' {} a -> s {mergePolicy = a} :: InputCaptions)

instance Core.FromJSON InputCaptions where
  parseJSON =
    Core.withObject
      "InputCaptions"
      ( \x ->
          InputCaptions'
            Core.<$> (x Core..:? "CaptionSources" Core..!= Core.mempty)
            Core.<*> (x Core..:? "MergePolicy")
      )

instance Core.Hashable InputCaptions

instance Core.NFData InputCaptions

instance Core.ToJSON InputCaptions where
  toJSON InputCaptions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CaptionSources" Core..=) Core.<$> captionSources,
            ("MergePolicy" Core..=) Core.<$> mergePolicy
          ]
      )
