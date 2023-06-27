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
-- Module      : Amazonka.ChimeSdkVoice.Types.StreamingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.StreamingConfiguration where

import Amazonka.ChimeSdkVoice.Types.MediaInsightsConfiguration
import Amazonka.ChimeSdkVoice.Types.StreamingNotificationTarget
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The streaming configuration associated with an Amazon Chime SDK Voice
-- Connector. Specifies whether media streaming is enabled for sending to
-- Amazon Kinesis, and shows the retention period for the Amazon Kinesis
-- data, in hours.
--
-- /See:/ 'newStreamingConfiguration' smart constructor.
data StreamingConfiguration = StreamingConfiguration'
  { -- | The call analytics configuration.
    mediaInsightsConfiguration :: Prelude.Maybe MediaInsightsConfiguration,
    -- | The streaming notification targets.
    streamingNotificationTargets :: Prelude.Maybe (Prelude.NonEmpty StreamingNotificationTarget),
    -- | The amount of time, in hours, to the Kinesis data.
    dataRetentionInHours :: Prelude.Natural,
    -- | When true, streaming to Kinesis is off.
    disabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mediaInsightsConfiguration', 'streamingConfiguration_mediaInsightsConfiguration' - The call analytics configuration.
--
-- 'streamingNotificationTargets', 'streamingConfiguration_streamingNotificationTargets' - The streaming notification targets.
--
-- 'dataRetentionInHours', 'streamingConfiguration_dataRetentionInHours' - The amount of time, in hours, to the Kinesis data.
--
-- 'disabled', 'streamingConfiguration_disabled' - When true, streaming to Kinesis is off.
newStreamingConfiguration ::
  -- | 'dataRetentionInHours'
  Prelude.Natural ->
  -- | 'disabled'
  Prelude.Bool ->
  StreamingConfiguration
newStreamingConfiguration
  pDataRetentionInHours_
  pDisabled_ =
    StreamingConfiguration'
      { mediaInsightsConfiguration =
          Prelude.Nothing,
        streamingNotificationTargets = Prelude.Nothing,
        dataRetentionInHours = pDataRetentionInHours_,
        disabled = pDisabled_
      }

-- | The call analytics configuration.
streamingConfiguration_mediaInsightsConfiguration :: Lens.Lens' StreamingConfiguration (Prelude.Maybe MediaInsightsConfiguration)
streamingConfiguration_mediaInsightsConfiguration = Lens.lens (\StreamingConfiguration' {mediaInsightsConfiguration} -> mediaInsightsConfiguration) (\s@StreamingConfiguration' {} a -> s {mediaInsightsConfiguration = a} :: StreamingConfiguration)

-- | The streaming notification targets.
streamingConfiguration_streamingNotificationTargets :: Lens.Lens' StreamingConfiguration (Prelude.Maybe (Prelude.NonEmpty StreamingNotificationTarget))
streamingConfiguration_streamingNotificationTargets = Lens.lens (\StreamingConfiguration' {streamingNotificationTargets} -> streamingNotificationTargets) (\s@StreamingConfiguration' {} a -> s {streamingNotificationTargets = a} :: StreamingConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The amount of time, in hours, to the Kinesis data.
streamingConfiguration_dataRetentionInHours :: Lens.Lens' StreamingConfiguration Prelude.Natural
streamingConfiguration_dataRetentionInHours = Lens.lens (\StreamingConfiguration' {dataRetentionInHours} -> dataRetentionInHours) (\s@StreamingConfiguration' {} a -> s {dataRetentionInHours = a} :: StreamingConfiguration)

-- | When true, streaming to Kinesis is off.
streamingConfiguration_disabled :: Lens.Lens' StreamingConfiguration Prelude.Bool
streamingConfiguration_disabled = Lens.lens (\StreamingConfiguration' {disabled} -> disabled) (\s@StreamingConfiguration' {} a -> s {disabled = a} :: StreamingConfiguration)

instance Data.FromJSON StreamingConfiguration where
  parseJSON =
    Data.withObject
      "StreamingConfiguration"
      ( \x ->
          StreamingConfiguration'
            Prelude.<$> (x Data..:? "MediaInsightsConfiguration")
            Prelude.<*> (x Data..:? "StreamingNotificationTargets")
            Prelude.<*> (x Data..: "DataRetentionInHours")
            Prelude.<*> (x Data..: "Disabled")
      )

instance Prelude.Hashable StreamingConfiguration where
  hashWithSalt _salt StreamingConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` mediaInsightsConfiguration
      `Prelude.hashWithSalt` streamingNotificationTargets
      `Prelude.hashWithSalt` dataRetentionInHours
      `Prelude.hashWithSalt` disabled

instance Prelude.NFData StreamingConfiguration where
  rnf StreamingConfiguration' {..} =
    Prelude.rnf mediaInsightsConfiguration
      `Prelude.seq` Prelude.rnf streamingNotificationTargets
      `Prelude.seq` Prelude.rnf dataRetentionInHours
      `Prelude.seq` Prelude.rnf disabled

instance Data.ToJSON StreamingConfiguration where
  toJSON StreamingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MediaInsightsConfiguration" Data..=)
              Prelude.<$> mediaInsightsConfiguration,
            ("StreamingNotificationTargets" Data..=)
              Prelude.<$> streamingNotificationTargets,
            Prelude.Just
              ( "DataRetentionInHours"
                  Data..= dataRetentionInHours
              ),
            Prelude.Just ("Disabled" Data..= disabled)
          ]
      )
