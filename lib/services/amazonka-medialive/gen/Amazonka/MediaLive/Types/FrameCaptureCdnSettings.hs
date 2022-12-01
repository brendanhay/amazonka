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
-- Module      : Amazonka.MediaLive.Types.FrameCaptureCdnSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.FrameCaptureCdnSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types.FrameCaptureS3Settings
import qualified Amazonka.Prelude as Prelude

-- | Frame Capture Cdn Settings
--
-- /See:/ 'newFrameCaptureCdnSettings' smart constructor.
data FrameCaptureCdnSettings = FrameCaptureCdnSettings'
  { frameCaptureS3Settings :: Prelude.Maybe FrameCaptureS3Settings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FrameCaptureCdnSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'frameCaptureS3Settings', 'frameCaptureCdnSettings_frameCaptureS3Settings' - Undocumented member.
newFrameCaptureCdnSettings ::
  FrameCaptureCdnSettings
newFrameCaptureCdnSettings =
  FrameCaptureCdnSettings'
    { frameCaptureS3Settings =
        Prelude.Nothing
    }

-- | Undocumented member.
frameCaptureCdnSettings_frameCaptureS3Settings :: Lens.Lens' FrameCaptureCdnSettings (Prelude.Maybe FrameCaptureS3Settings)
frameCaptureCdnSettings_frameCaptureS3Settings = Lens.lens (\FrameCaptureCdnSettings' {frameCaptureS3Settings} -> frameCaptureS3Settings) (\s@FrameCaptureCdnSettings' {} a -> s {frameCaptureS3Settings = a} :: FrameCaptureCdnSettings)

instance Core.FromJSON FrameCaptureCdnSettings where
  parseJSON =
    Core.withObject
      "FrameCaptureCdnSettings"
      ( \x ->
          FrameCaptureCdnSettings'
            Prelude.<$> (x Core..:? "frameCaptureS3Settings")
      )

instance Prelude.Hashable FrameCaptureCdnSettings where
  hashWithSalt _salt FrameCaptureCdnSettings' {..} =
    _salt `Prelude.hashWithSalt` frameCaptureS3Settings

instance Prelude.NFData FrameCaptureCdnSettings where
  rnf FrameCaptureCdnSettings' {..} =
    Prelude.rnf frameCaptureS3Settings

instance Core.ToJSON FrameCaptureCdnSettings where
  toJSON FrameCaptureCdnSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("frameCaptureS3Settings" Core..=)
              Prelude.<$> frameCaptureS3Settings
          ]
      )
