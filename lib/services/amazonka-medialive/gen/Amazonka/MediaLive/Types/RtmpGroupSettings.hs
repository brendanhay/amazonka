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
-- Module      : Amazonka.MediaLive.Types.RtmpGroupSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.RtmpGroupSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.AuthenticationScheme
import Amazonka.MediaLive.Types.InputLossActionForRtmpOut
import Amazonka.MediaLive.Types.RtmpAdMarkers
import Amazonka.MediaLive.Types.RtmpCacheFullBehavior
import Amazonka.MediaLive.Types.RtmpCaptionData
import qualified Amazonka.Prelude as Prelude

-- | Rtmp Group Settings
--
-- /See:/ 'newRtmpGroupSettings' smart constructor.
data RtmpGroupSettings = RtmpGroupSettings'
  { -- | Choose the ad marker type for this output group. MediaLive will create a
    -- message based on the content of each SCTE-35 message, format it for that
    -- marker type, and insert it in the datastream.
    adMarkers :: Prelude.Maybe [RtmpAdMarkers],
    -- | Authentication scheme to use when connecting with CDN
    authenticationScheme :: Prelude.Maybe AuthenticationScheme,
    -- | Controls behavior when content cache fills up. If remote origin server
    -- stalls the RTMP connection and does not accept content fast enough the
    -- \'Media Cache\' will fill up. When the cache reaches the duration
    -- specified by cacheLength the cache will stop accepting new content. If
    -- set to disconnectImmediately, the RTMP output will force a disconnect.
    -- Clear the media cache, and reconnect after restartDelay seconds. If set
    -- to waitForServer, the RTMP output will wait up to 5 minutes to allow the
    -- origin server to begin accepting data again.
    cacheFullBehavior :: Prelude.Maybe RtmpCacheFullBehavior,
    -- | Cache length, in seconds, is used to calculate buffer size.
    cacheLength :: Prelude.Maybe Prelude.Natural,
    -- | Controls the types of data that passes to onCaptionInfo outputs. If set
    -- to \'all\' then 608 and 708 carried DTVCC data will be passed. If set to
    -- \'field1AndField2608\' then DTVCC data will be stripped out, but 608
    -- data from both fields will be passed. If set to \'field1608\' then only
    -- the data carried in 608 from field 1 video will be passed.
    captionData :: Prelude.Maybe RtmpCaptionData,
    -- | Controls the behavior of this RTMP group if input becomes unavailable. -
    -- emitOutput: Emit a slate until input returns. - pauseOutput: Stop
    -- transmitting data until input returns. This does not close the
    -- underlying RTMP connection.
    inputLossAction :: Prelude.Maybe InputLossActionForRtmpOut,
    -- | If a streaming output fails, number of seconds to wait until a restart
    -- is initiated. A value of 0 means never restart.
    restartDelay :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RtmpGroupSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adMarkers', 'rtmpGroupSettings_adMarkers' - Choose the ad marker type for this output group. MediaLive will create a
-- message based on the content of each SCTE-35 message, format it for that
-- marker type, and insert it in the datastream.
--
-- 'authenticationScheme', 'rtmpGroupSettings_authenticationScheme' - Authentication scheme to use when connecting with CDN
--
-- 'cacheFullBehavior', 'rtmpGroupSettings_cacheFullBehavior' - Controls behavior when content cache fills up. If remote origin server
-- stalls the RTMP connection and does not accept content fast enough the
-- \'Media Cache\' will fill up. When the cache reaches the duration
-- specified by cacheLength the cache will stop accepting new content. If
-- set to disconnectImmediately, the RTMP output will force a disconnect.
-- Clear the media cache, and reconnect after restartDelay seconds. If set
-- to waitForServer, the RTMP output will wait up to 5 minutes to allow the
-- origin server to begin accepting data again.
--
-- 'cacheLength', 'rtmpGroupSettings_cacheLength' - Cache length, in seconds, is used to calculate buffer size.
--
-- 'captionData', 'rtmpGroupSettings_captionData' - Controls the types of data that passes to onCaptionInfo outputs. If set
-- to \'all\' then 608 and 708 carried DTVCC data will be passed. If set to
-- \'field1AndField2608\' then DTVCC data will be stripped out, but 608
-- data from both fields will be passed. If set to \'field1608\' then only
-- the data carried in 608 from field 1 video will be passed.
--
-- 'inputLossAction', 'rtmpGroupSettings_inputLossAction' - Controls the behavior of this RTMP group if input becomes unavailable. -
-- emitOutput: Emit a slate until input returns. - pauseOutput: Stop
-- transmitting data until input returns. This does not close the
-- underlying RTMP connection.
--
-- 'restartDelay', 'rtmpGroupSettings_restartDelay' - If a streaming output fails, number of seconds to wait until a restart
-- is initiated. A value of 0 means never restart.
newRtmpGroupSettings ::
  RtmpGroupSettings
newRtmpGroupSettings =
  RtmpGroupSettings'
    { adMarkers = Prelude.Nothing,
      authenticationScheme = Prelude.Nothing,
      cacheFullBehavior = Prelude.Nothing,
      cacheLength = Prelude.Nothing,
      captionData = Prelude.Nothing,
      inputLossAction = Prelude.Nothing,
      restartDelay = Prelude.Nothing
    }

-- | Choose the ad marker type for this output group. MediaLive will create a
-- message based on the content of each SCTE-35 message, format it for that
-- marker type, and insert it in the datastream.
rtmpGroupSettings_adMarkers :: Lens.Lens' RtmpGroupSettings (Prelude.Maybe [RtmpAdMarkers])
rtmpGroupSettings_adMarkers = Lens.lens (\RtmpGroupSettings' {adMarkers} -> adMarkers) (\s@RtmpGroupSettings' {} a -> s {adMarkers = a} :: RtmpGroupSettings) Prelude.. Lens.mapping Lens.coerced

-- | Authentication scheme to use when connecting with CDN
rtmpGroupSettings_authenticationScheme :: Lens.Lens' RtmpGroupSettings (Prelude.Maybe AuthenticationScheme)
rtmpGroupSettings_authenticationScheme = Lens.lens (\RtmpGroupSettings' {authenticationScheme} -> authenticationScheme) (\s@RtmpGroupSettings' {} a -> s {authenticationScheme = a} :: RtmpGroupSettings)

-- | Controls behavior when content cache fills up. If remote origin server
-- stalls the RTMP connection and does not accept content fast enough the
-- \'Media Cache\' will fill up. When the cache reaches the duration
-- specified by cacheLength the cache will stop accepting new content. If
-- set to disconnectImmediately, the RTMP output will force a disconnect.
-- Clear the media cache, and reconnect after restartDelay seconds. If set
-- to waitForServer, the RTMP output will wait up to 5 minutes to allow the
-- origin server to begin accepting data again.
rtmpGroupSettings_cacheFullBehavior :: Lens.Lens' RtmpGroupSettings (Prelude.Maybe RtmpCacheFullBehavior)
rtmpGroupSettings_cacheFullBehavior = Lens.lens (\RtmpGroupSettings' {cacheFullBehavior} -> cacheFullBehavior) (\s@RtmpGroupSettings' {} a -> s {cacheFullBehavior = a} :: RtmpGroupSettings)

-- | Cache length, in seconds, is used to calculate buffer size.
rtmpGroupSettings_cacheLength :: Lens.Lens' RtmpGroupSettings (Prelude.Maybe Prelude.Natural)
rtmpGroupSettings_cacheLength = Lens.lens (\RtmpGroupSettings' {cacheLength} -> cacheLength) (\s@RtmpGroupSettings' {} a -> s {cacheLength = a} :: RtmpGroupSettings)

-- | Controls the types of data that passes to onCaptionInfo outputs. If set
-- to \'all\' then 608 and 708 carried DTVCC data will be passed. If set to
-- \'field1AndField2608\' then DTVCC data will be stripped out, but 608
-- data from both fields will be passed. If set to \'field1608\' then only
-- the data carried in 608 from field 1 video will be passed.
rtmpGroupSettings_captionData :: Lens.Lens' RtmpGroupSettings (Prelude.Maybe RtmpCaptionData)
rtmpGroupSettings_captionData = Lens.lens (\RtmpGroupSettings' {captionData} -> captionData) (\s@RtmpGroupSettings' {} a -> s {captionData = a} :: RtmpGroupSettings)

-- | Controls the behavior of this RTMP group if input becomes unavailable. -
-- emitOutput: Emit a slate until input returns. - pauseOutput: Stop
-- transmitting data until input returns. This does not close the
-- underlying RTMP connection.
rtmpGroupSettings_inputLossAction :: Lens.Lens' RtmpGroupSettings (Prelude.Maybe InputLossActionForRtmpOut)
rtmpGroupSettings_inputLossAction = Lens.lens (\RtmpGroupSettings' {inputLossAction} -> inputLossAction) (\s@RtmpGroupSettings' {} a -> s {inputLossAction = a} :: RtmpGroupSettings)

-- | If a streaming output fails, number of seconds to wait until a restart
-- is initiated. A value of 0 means never restart.
rtmpGroupSettings_restartDelay :: Lens.Lens' RtmpGroupSettings (Prelude.Maybe Prelude.Natural)
rtmpGroupSettings_restartDelay = Lens.lens (\RtmpGroupSettings' {restartDelay} -> restartDelay) (\s@RtmpGroupSettings' {} a -> s {restartDelay = a} :: RtmpGroupSettings)

instance Data.FromJSON RtmpGroupSettings where
  parseJSON =
    Data.withObject
      "RtmpGroupSettings"
      ( \x ->
          RtmpGroupSettings'
            Prelude.<$> (x Data..:? "adMarkers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "authenticationScheme")
            Prelude.<*> (x Data..:? "cacheFullBehavior")
            Prelude.<*> (x Data..:? "cacheLength")
            Prelude.<*> (x Data..:? "captionData")
            Prelude.<*> (x Data..:? "inputLossAction")
            Prelude.<*> (x Data..:? "restartDelay")
      )

instance Prelude.Hashable RtmpGroupSettings where
  hashWithSalt _salt RtmpGroupSettings' {..} =
    _salt
      `Prelude.hashWithSalt` adMarkers
      `Prelude.hashWithSalt` authenticationScheme
      `Prelude.hashWithSalt` cacheFullBehavior
      `Prelude.hashWithSalt` cacheLength
      `Prelude.hashWithSalt` captionData
      `Prelude.hashWithSalt` inputLossAction
      `Prelude.hashWithSalt` restartDelay

instance Prelude.NFData RtmpGroupSettings where
  rnf RtmpGroupSettings' {..} =
    Prelude.rnf adMarkers `Prelude.seq`
      Prelude.rnf authenticationScheme `Prelude.seq`
        Prelude.rnf cacheFullBehavior `Prelude.seq`
          Prelude.rnf cacheLength `Prelude.seq`
            Prelude.rnf captionData `Prelude.seq`
              Prelude.rnf inputLossAction `Prelude.seq`
                Prelude.rnf restartDelay

instance Data.ToJSON RtmpGroupSettings where
  toJSON RtmpGroupSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("adMarkers" Data..=) Prelude.<$> adMarkers,
            ("authenticationScheme" Data..=)
              Prelude.<$> authenticationScheme,
            ("cacheFullBehavior" Data..=)
              Prelude.<$> cacheFullBehavior,
            ("cacheLength" Data..=) Prelude.<$> cacheLength,
            ("captionData" Data..=) Prelude.<$> captionData,
            ("inputLossAction" Data..=)
              Prelude.<$> inputLossAction,
            ("restartDelay" Data..=) Prelude.<$> restartDelay
          ]
      )
