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
-- Module      : Network.AWS.MediaLive.Types.FrameCaptureOutputSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FrameCaptureOutputSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Frame Capture Output Settings
--
-- /See:/ 'newFrameCaptureOutputSettings' smart constructor.
data FrameCaptureOutputSettings = FrameCaptureOutputSettings'
  { -- | Required if the output group contains more than one output. This
    -- modifier forms part of the output file name.
    nameModifier :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FrameCaptureOutputSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nameModifier', 'frameCaptureOutputSettings_nameModifier' - Required if the output group contains more than one output. This
-- modifier forms part of the output file name.
newFrameCaptureOutputSettings ::
  FrameCaptureOutputSettings
newFrameCaptureOutputSettings =
  FrameCaptureOutputSettings'
    { nameModifier =
        Core.Nothing
    }

-- | Required if the output group contains more than one output. This
-- modifier forms part of the output file name.
frameCaptureOutputSettings_nameModifier :: Lens.Lens' FrameCaptureOutputSettings (Core.Maybe Core.Text)
frameCaptureOutputSettings_nameModifier = Lens.lens (\FrameCaptureOutputSettings' {nameModifier} -> nameModifier) (\s@FrameCaptureOutputSettings' {} a -> s {nameModifier = a} :: FrameCaptureOutputSettings)

instance Core.FromJSON FrameCaptureOutputSettings where
  parseJSON =
    Core.withObject
      "FrameCaptureOutputSettings"
      ( \x ->
          FrameCaptureOutputSettings'
            Core.<$> (x Core..:? "nameModifier")
      )

instance Core.Hashable FrameCaptureOutputSettings

instance Core.NFData FrameCaptureOutputSettings

instance Core.ToJSON FrameCaptureOutputSettings where
  toJSON FrameCaptureOutputSettings' {..} =
    Core.object
      ( Core.catMaybes
          [("nameModifier" Core..=) Core.<$> nameModifier]
      )
