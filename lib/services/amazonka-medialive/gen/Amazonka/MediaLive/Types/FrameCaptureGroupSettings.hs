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
-- Module      : Amazonka.MediaLive.Types.FrameCaptureGroupSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.FrameCaptureGroupSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.FrameCaptureCdnSettings
import Amazonka.MediaLive.Types.OutputLocationRef
import qualified Amazonka.Prelude as Prelude

-- | Frame Capture Group Settings
--
-- /See:/ 'newFrameCaptureGroupSettings' smart constructor.
data FrameCaptureGroupSettings = FrameCaptureGroupSettings'
  { -- | Parameters that control interactions with the CDN.
    frameCaptureCdnSettings :: Prelude.Maybe FrameCaptureCdnSettings,
    -- | The destination for the frame capture files. Either the URI for an
    -- Amazon S3 bucket and object, plus a file name prefix (for example,
    -- s3ssl:\/\/sportsDelivery\/highlights\/20180820\/curling-) or the URI for
    -- a MediaStore container, plus a file name prefix (for example,
    -- mediastoressl:\/\/sportsDelivery\/20180820\/curling-). The final file
    -- names consist of the prefix from the destination field (for example,
    -- \"curling-\") + name modifier + the counter (5 digits, starting from
    -- 00001) + extension (which is always .jpg). For example,
    -- curling-low.00001.jpg
    destination :: OutputLocationRef
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FrameCaptureGroupSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'frameCaptureCdnSettings', 'frameCaptureGroupSettings_frameCaptureCdnSettings' - Parameters that control interactions with the CDN.
--
-- 'destination', 'frameCaptureGroupSettings_destination' - The destination for the frame capture files. Either the URI for an
-- Amazon S3 bucket and object, plus a file name prefix (for example,
-- s3ssl:\/\/sportsDelivery\/highlights\/20180820\/curling-) or the URI for
-- a MediaStore container, plus a file name prefix (for example,
-- mediastoressl:\/\/sportsDelivery\/20180820\/curling-). The final file
-- names consist of the prefix from the destination field (for example,
-- \"curling-\") + name modifier + the counter (5 digits, starting from
-- 00001) + extension (which is always .jpg). For example,
-- curling-low.00001.jpg
newFrameCaptureGroupSettings ::
  -- | 'destination'
  OutputLocationRef ->
  FrameCaptureGroupSettings
newFrameCaptureGroupSettings pDestination_ =
  FrameCaptureGroupSettings'
    { frameCaptureCdnSettings =
        Prelude.Nothing,
      destination = pDestination_
    }

-- | Parameters that control interactions with the CDN.
frameCaptureGroupSettings_frameCaptureCdnSettings :: Lens.Lens' FrameCaptureGroupSettings (Prelude.Maybe FrameCaptureCdnSettings)
frameCaptureGroupSettings_frameCaptureCdnSettings = Lens.lens (\FrameCaptureGroupSettings' {frameCaptureCdnSettings} -> frameCaptureCdnSettings) (\s@FrameCaptureGroupSettings' {} a -> s {frameCaptureCdnSettings = a} :: FrameCaptureGroupSettings)

-- | The destination for the frame capture files. Either the URI for an
-- Amazon S3 bucket and object, plus a file name prefix (for example,
-- s3ssl:\/\/sportsDelivery\/highlights\/20180820\/curling-) or the URI for
-- a MediaStore container, plus a file name prefix (for example,
-- mediastoressl:\/\/sportsDelivery\/20180820\/curling-). The final file
-- names consist of the prefix from the destination field (for example,
-- \"curling-\") + name modifier + the counter (5 digits, starting from
-- 00001) + extension (which is always .jpg). For example,
-- curling-low.00001.jpg
frameCaptureGroupSettings_destination :: Lens.Lens' FrameCaptureGroupSettings OutputLocationRef
frameCaptureGroupSettings_destination = Lens.lens (\FrameCaptureGroupSettings' {destination} -> destination) (\s@FrameCaptureGroupSettings' {} a -> s {destination = a} :: FrameCaptureGroupSettings)

instance Data.FromJSON FrameCaptureGroupSettings where
  parseJSON =
    Data.withObject
      "FrameCaptureGroupSettings"
      ( \x ->
          FrameCaptureGroupSettings'
            Prelude.<$> (x Data..:? "frameCaptureCdnSettings")
            Prelude.<*> (x Data..: "destination")
      )

instance Prelude.Hashable FrameCaptureGroupSettings where
  hashWithSalt _salt FrameCaptureGroupSettings' {..} =
    _salt
      `Prelude.hashWithSalt` frameCaptureCdnSettings
      `Prelude.hashWithSalt` destination

instance Prelude.NFData FrameCaptureGroupSettings where
  rnf FrameCaptureGroupSettings' {..} =
    Prelude.rnf frameCaptureCdnSettings
      `Prelude.seq` Prelude.rnf destination

instance Data.ToJSON FrameCaptureGroupSettings where
  toJSON FrameCaptureGroupSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("frameCaptureCdnSettings" Data..=)
              Prelude.<$> frameCaptureCdnSettings,
            Prelude.Just ("destination" Data..= destination)
          ]
      )
