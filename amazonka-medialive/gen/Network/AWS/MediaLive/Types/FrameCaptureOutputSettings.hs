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
-- Module      : Network.AWS.MediaLive.Types.FrameCaptureOutputSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FrameCaptureOutputSettings where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Frame Capture Output Settings
--
-- /See:/ 'newFrameCaptureOutputSettings' smart constructor.
data FrameCaptureOutputSettings = FrameCaptureOutputSettings'
  { -- | Required if the output group contains more than one output. This
    -- modifier forms part of the output file name.
    nameModifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | Required if the output group contains more than one output. This
-- modifier forms part of the output file name.
frameCaptureOutputSettings_nameModifier :: Lens.Lens' FrameCaptureOutputSettings (Prelude.Maybe Prelude.Text)
frameCaptureOutputSettings_nameModifier = Lens.lens (\FrameCaptureOutputSettings' {nameModifier} -> nameModifier) (\s@FrameCaptureOutputSettings' {} a -> s {nameModifier = a} :: FrameCaptureOutputSettings)

instance Prelude.FromJSON FrameCaptureOutputSettings where
  parseJSON =
    Prelude.withObject
      "FrameCaptureOutputSettings"
      ( \x ->
          FrameCaptureOutputSettings'
            Prelude.<$> (x Prelude..:? "nameModifier")
      )

instance Prelude.Hashable FrameCaptureOutputSettings

instance Prelude.NFData FrameCaptureOutputSettings

instance Prelude.ToJSON FrameCaptureOutputSettings where
  toJSON FrameCaptureOutputSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("nameModifier" Prelude..=)
              Prelude.<$> nameModifier
          ]
      )
