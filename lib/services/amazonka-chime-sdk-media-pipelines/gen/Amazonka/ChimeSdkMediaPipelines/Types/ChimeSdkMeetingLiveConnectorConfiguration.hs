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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.ChimeSdkMeetingLiveConnectorConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.ChimeSdkMeetingLiveConnectorConfiguration where

import Amazonka.ChimeSdkMediaPipelines.Types.CompositedVideoArtifactsConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.LiveConnectorMuxType
import Amazonka.ChimeSdkMediaPipelines.Types.SourceConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The media pipeline\'s configuration object.
--
-- /See:/ 'newChimeSdkMeetingLiveConnectorConfiguration' smart constructor.
data ChimeSdkMeetingLiveConnectorConfiguration = ChimeSdkMeetingLiveConnectorConfiguration'
  { -- | The source configuration settings of the media pipeline\'s configuration
    -- object.
    sourceConfiguration :: Prelude.Maybe SourceConfiguration,
    -- | The media pipeline\'s composited video.
    compositedVideo :: Prelude.Maybe CompositedVideoArtifactsConfiguration,
    -- | The configuration object\'s Chime SDK meeting ARN.
    arn :: Core.Sensitive Prelude.Text,
    -- | The configuration object\'s multiplex type.
    muxType :: LiveConnectorMuxType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChimeSdkMeetingLiveConnectorConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceConfiguration', 'chimeSdkMeetingLiveConnectorConfiguration_sourceConfiguration' - The source configuration settings of the media pipeline\'s configuration
-- object.
--
-- 'compositedVideo', 'chimeSdkMeetingLiveConnectorConfiguration_compositedVideo' - The media pipeline\'s composited video.
--
-- 'arn', 'chimeSdkMeetingLiveConnectorConfiguration_arn' - The configuration object\'s Chime SDK meeting ARN.
--
-- 'muxType', 'chimeSdkMeetingLiveConnectorConfiguration_muxType' - The configuration object\'s multiplex type.
newChimeSdkMeetingLiveConnectorConfiguration ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'muxType'
  LiveConnectorMuxType ->
  ChimeSdkMeetingLiveConnectorConfiguration
newChimeSdkMeetingLiveConnectorConfiguration
  pArn_
  pMuxType_ =
    ChimeSdkMeetingLiveConnectorConfiguration'
      { sourceConfiguration =
          Prelude.Nothing,
        compositedVideo =
          Prelude.Nothing,
        arn =
          Core._Sensitive Lens.# pArn_,
        muxType = pMuxType_
      }

-- | The source configuration settings of the media pipeline\'s configuration
-- object.
chimeSdkMeetingLiveConnectorConfiguration_sourceConfiguration :: Lens.Lens' ChimeSdkMeetingLiveConnectorConfiguration (Prelude.Maybe SourceConfiguration)
chimeSdkMeetingLiveConnectorConfiguration_sourceConfiguration = Lens.lens (\ChimeSdkMeetingLiveConnectorConfiguration' {sourceConfiguration} -> sourceConfiguration) (\s@ChimeSdkMeetingLiveConnectorConfiguration' {} a -> s {sourceConfiguration = a} :: ChimeSdkMeetingLiveConnectorConfiguration)

-- | The media pipeline\'s composited video.
chimeSdkMeetingLiveConnectorConfiguration_compositedVideo :: Lens.Lens' ChimeSdkMeetingLiveConnectorConfiguration (Prelude.Maybe CompositedVideoArtifactsConfiguration)
chimeSdkMeetingLiveConnectorConfiguration_compositedVideo = Lens.lens (\ChimeSdkMeetingLiveConnectorConfiguration' {compositedVideo} -> compositedVideo) (\s@ChimeSdkMeetingLiveConnectorConfiguration' {} a -> s {compositedVideo = a} :: ChimeSdkMeetingLiveConnectorConfiguration)

-- | The configuration object\'s Chime SDK meeting ARN.
chimeSdkMeetingLiveConnectorConfiguration_arn :: Lens.Lens' ChimeSdkMeetingLiveConnectorConfiguration Prelude.Text
chimeSdkMeetingLiveConnectorConfiguration_arn = Lens.lens (\ChimeSdkMeetingLiveConnectorConfiguration' {arn} -> arn) (\s@ChimeSdkMeetingLiveConnectorConfiguration' {} a -> s {arn = a} :: ChimeSdkMeetingLiveConnectorConfiguration) Prelude.. Core._Sensitive

-- | The configuration object\'s multiplex type.
chimeSdkMeetingLiveConnectorConfiguration_muxType :: Lens.Lens' ChimeSdkMeetingLiveConnectorConfiguration LiveConnectorMuxType
chimeSdkMeetingLiveConnectorConfiguration_muxType = Lens.lens (\ChimeSdkMeetingLiveConnectorConfiguration' {muxType} -> muxType) (\s@ChimeSdkMeetingLiveConnectorConfiguration' {} a -> s {muxType = a} :: ChimeSdkMeetingLiveConnectorConfiguration)

instance
  Core.FromJSON
    ChimeSdkMeetingLiveConnectorConfiguration
  where
  parseJSON =
    Core.withObject
      "ChimeSdkMeetingLiveConnectorConfiguration"
      ( \x ->
          ChimeSdkMeetingLiveConnectorConfiguration'
            Prelude.<$> (x Core..:? "SourceConfiguration")
              Prelude.<*> (x Core..:? "CompositedVideo")
              Prelude.<*> (x Core..: "Arn")
              Prelude.<*> (x Core..: "MuxType")
      )

instance
  Prelude.Hashable
    ChimeSdkMeetingLiveConnectorConfiguration
  where
  hashWithSalt
    _salt
    ChimeSdkMeetingLiveConnectorConfiguration' {..} =
      _salt `Prelude.hashWithSalt` sourceConfiguration
        `Prelude.hashWithSalt` compositedVideo
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` muxType

instance
  Prelude.NFData
    ChimeSdkMeetingLiveConnectorConfiguration
  where
  rnf ChimeSdkMeetingLiveConnectorConfiguration' {..} =
    Prelude.rnf sourceConfiguration
      `Prelude.seq` Prelude.rnf compositedVideo
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf muxType

instance
  Core.ToJSON
    ChimeSdkMeetingLiveConnectorConfiguration
  where
  toJSON ChimeSdkMeetingLiveConnectorConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SourceConfiguration" Core..=)
              Prelude.<$> sourceConfiguration,
            ("CompositedVideo" Core..=)
              Prelude.<$> compositedVideo,
            Prelude.Just ("Arn" Core..= arn),
            Prelude.Just ("MuxType" Core..= muxType)
          ]
      )
