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
-- Module      : Amazonka.MediaPackageV2.Types.Segment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageV2.Types.Segment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageV2.Types.Encryption
import Amazonka.MediaPackageV2.Types.Scte
import qualified Amazonka.Prelude as Prelude

-- | The segment configuration, including the segment name, duration, and
-- other configuration values.
--
-- /See:/ 'newSegment' smart constructor.
data Segment = Segment'
  { encryption :: Prelude.Maybe Encryption,
    -- | When selected, the stream set includes an additional I-frame only
    -- stream, along with the other tracks. If false, this extra stream is not
    -- included. MediaPackage generates an I-frame only stream from the first
    -- rendition in the manifest. The service inserts EXT-I-FRAMES-ONLY tags in
    -- the output manifest, and then generates and includes an I-frames only
    -- playlist in the stream. This playlist permits player functionality like
    -- fast forward and rewind.
    includeIframeOnlyStreams :: Prelude.Maybe Prelude.Bool,
    -- | The SCTE configuration options in the segment settings.
    scte :: Prelude.Maybe Scte,
    -- | The duration (in seconds) of each segment. Enter a value equal to, or a
    -- multiple of, the input segment duration. If the value that you enter is
    -- different from the input segment duration, MediaPackage rounds segments
    -- to the nearest multiple of the input segment duration.
    segmentDurationSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The name that describes the segment. The name is the base name of the
    -- segment used in all content manifests inside of the endpoint. You can\'t
    -- use spaces in the name.
    segmentName :: Prelude.Maybe Prelude.Text,
    -- | By default, MediaPackage excludes all digital video broadcasting (DVB)
    -- subtitles from the output. When selected, MediaPackage passes through
    -- DVB subtitles into the output.
    tsIncludeDvbSubtitles :: Prelude.Maybe Prelude.Bool,
    -- | When selected, MediaPackage bundles all audio tracks in a rendition
    -- group. All other tracks in the stream can be used with any audio
    -- rendition from the group.
    tsUseAudioRenditionGroup :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Segment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryption', 'segment_encryption' - Undocumented member.
--
-- 'includeIframeOnlyStreams', 'segment_includeIframeOnlyStreams' - When selected, the stream set includes an additional I-frame only
-- stream, along with the other tracks. If false, this extra stream is not
-- included. MediaPackage generates an I-frame only stream from the first
-- rendition in the manifest. The service inserts EXT-I-FRAMES-ONLY tags in
-- the output manifest, and then generates and includes an I-frames only
-- playlist in the stream. This playlist permits player functionality like
-- fast forward and rewind.
--
-- 'scte', 'segment_scte' - The SCTE configuration options in the segment settings.
--
-- 'segmentDurationSeconds', 'segment_segmentDurationSeconds' - The duration (in seconds) of each segment. Enter a value equal to, or a
-- multiple of, the input segment duration. If the value that you enter is
-- different from the input segment duration, MediaPackage rounds segments
-- to the nearest multiple of the input segment duration.
--
-- 'segmentName', 'segment_segmentName' - The name that describes the segment. The name is the base name of the
-- segment used in all content manifests inside of the endpoint. You can\'t
-- use spaces in the name.
--
-- 'tsIncludeDvbSubtitles', 'segment_tsIncludeDvbSubtitles' - By default, MediaPackage excludes all digital video broadcasting (DVB)
-- subtitles from the output. When selected, MediaPackage passes through
-- DVB subtitles into the output.
--
-- 'tsUseAudioRenditionGroup', 'segment_tsUseAudioRenditionGroup' - When selected, MediaPackage bundles all audio tracks in a rendition
-- group. All other tracks in the stream can be used with any audio
-- rendition from the group.
newSegment ::
  Segment
newSegment =
  Segment'
    { encryption = Prelude.Nothing,
      includeIframeOnlyStreams = Prelude.Nothing,
      scte = Prelude.Nothing,
      segmentDurationSeconds = Prelude.Nothing,
      segmentName = Prelude.Nothing,
      tsIncludeDvbSubtitles = Prelude.Nothing,
      tsUseAudioRenditionGroup = Prelude.Nothing
    }

-- | Undocumented member.
segment_encryption :: Lens.Lens' Segment (Prelude.Maybe Encryption)
segment_encryption = Lens.lens (\Segment' {encryption} -> encryption) (\s@Segment' {} a -> s {encryption = a} :: Segment)

-- | When selected, the stream set includes an additional I-frame only
-- stream, along with the other tracks. If false, this extra stream is not
-- included. MediaPackage generates an I-frame only stream from the first
-- rendition in the manifest. The service inserts EXT-I-FRAMES-ONLY tags in
-- the output manifest, and then generates and includes an I-frames only
-- playlist in the stream. This playlist permits player functionality like
-- fast forward and rewind.
segment_includeIframeOnlyStreams :: Lens.Lens' Segment (Prelude.Maybe Prelude.Bool)
segment_includeIframeOnlyStreams = Lens.lens (\Segment' {includeIframeOnlyStreams} -> includeIframeOnlyStreams) (\s@Segment' {} a -> s {includeIframeOnlyStreams = a} :: Segment)

-- | The SCTE configuration options in the segment settings.
segment_scte :: Lens.Lens' Segment (Prelude.Maybe Scte)
segment_scte = Lens.lens (\Segment' {scte} -> scte) (\s@Segment' {} a -> s {scte = a} :: Segment)

-- | The duration (in seconds) of each segment. Enter a value equal to, or a
-- multiple of, the input segment duration. If the value that you enter is
-- different from the input segment duration, MediaPackage rounds segments
-- to the nearest multiple of the input segment duration.
segment_segmentDurationSeconds :: Lens.Lens' Segment (Prelude.Maybe Prelude.Natural)
segment_segmentDurationSeconds = Lens.lens (\Segment' {segmentDurationSeconds} -> segmentDurationSeconds) (\s@Segment' {} a -> s {segmentDurationSeconds = a} :: Segment)

-- | The name that describes the segment. The name is the base name of the
-- segment used in all content manifests inside of the endpoint. You can\'t
-- use spaces in the name.
segment_segmentName :: Lens.Lens' Segment (Prelude.Maybe Prelude.Text)
segment_segmentName = Lens.lens (\Segment' {segmentName} -> segmentName) (\s@Segment' {} a -> s {segmentName = a} :: Segment)

-- | By default, MediaPackage excludes all digital video broadcasting (DVB)
-- subtitles from the output. When selected, MediaPackage passes through
-- DVB subtitles into the output.
segment_tsIncludeDvbSubtitles :: Lens.Lens' Segment (Prelude.Maybe Prelude.Bool)
segment_tsIncludeDvbSubtitles = Lens.lens (\Segment' {tsIncludeDvbSubtitles} -> tsIncludeDvbSubtitles) (\s@Segment' {} a -> s {tsIncludeDvbSubtitles = a} :: Segment)

-- | When selected, MediaPackage bundles all audio tracks in a rendition
-- group. All other tracks in the stream can be used with any audio
-- rendition from the group.
segment_tsUseAudioRenditionGroup :: Lens.Lens' Segment (Prelude.Maybe Prelude.Bool)
segment_tsUseAudioRenditionGroup = Lens.lens (\Segment' {tsUseAudioRenditionGroup} -> tsUseAudioRenditionGroup) (\s@Segment' {} a -> s {tsUseAudioRenditionGroup = a} :: Segment)

instance Data.FromJSON Segment where
  parseJSON =
    Data.withObject
      "Segment"
      ( \x ->
          Segment'
            Prelude.<$> (x Data..:? "Encryption")
            Prelude.<*> (x Data..:? "IncludeIframeOnlyStreams")
            Prelude.<*> (x Data..:? "Scte")
            Prelude.<*> (x Data..:? "SegmentDurationSeconds")
            Prelude.<*> (x Data..:? "SegmentName")
            Prelude.<*> (x Data..:? "TsIncludeDvbSubtitles")
            Prelude.<*> (x Data..:? "TsUseAudioRenditionGroup")
      )

instance Prelude.Hashable Segment where
  hashWithSalt _salt Segment' {..} =
    _salt
      `Prelude.hashWithSalt` encryption
      `Prelude.hashWithSalt` includeIframeOnlyStreams
      `Prelude.hashWithSalt` scte
      `Prelude.hashWithSalt` segmentDurationSeconds
      `Prelude.hashWithSalt` segmentName
      `Prelude.hashWithSalt` tsIncludeDvbSubtitles
      `Prelude.hashWithSalt` tsUseAudioRenditionGroup

instance Prelude.NFData Segment where
  rnf Segment' {..} =
    Prelude.rnf encryption
      `Prelude.seq` Prelude.rnf includeIframeOnlyStreams
      `Prelude.seq` Prelude.rnf scte
      `Prelude.seq` Prelude.rnf segmentDurationSeconds
      `Prelude.seq` Prelude.rnf segmentName
      `Prelude.seq` Prelude.rnf tsIncludeDvbSubtitles
      `Prelude.seq` Prelude.rnf tsUseAudioRenditionGroup

instance Data.ToJSON Segment where
  toJSON Segment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Encryption" Data..=) Prelude.<$> encryption,
            ("IncludeIframeOnlyStreams" Data..=)
              Prelude.<$> includeIframeOnlyStreams,
            ("Scte" Data..=) Prelude.<$> scte,
            ("SegmentDurationSeconds" Data..=)
              Prelude.<$> segmentDurationSeconds,
            ("SegmentName" Data..=) Prelude.<$> segmentName,
            ("TsIncludeDvbSubtitles" Data..=)
              Prelude.<$> tsIncludeDvbSubtitles,
            ("TsUseAudioRenditionGroup" Data..=)
              Prelude.<$> tsUseAudioRenditionGroup
          ]
      )
