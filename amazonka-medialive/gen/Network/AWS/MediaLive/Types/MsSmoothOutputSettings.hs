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
-- Module      : Network.AWS.MediaLive.Types.MsSmoothOutputSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MsSmoothOutputSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.MsSmoothH265PackagingType
import qualified Network.AWS.Prelude as Prelude

-- | Ms Smooth Output Settings
--
-- /See:/ 'newMsSmoothOutputSettings' smart constructor.
data MsSmoothOutputSettings = MsSmoothOutputSettings'
  { -- | Only applicable when this output is referencing an H.265 video
    -- description. Specifies whether MP4 segments should be packaged as HEV1
    -- or HVC1.
    h265PackagingType :: Prelude.Maybe MsSmoothH265PackagingType,
    -- | String concatenated to the end of the destination filename. Required for
    -- multiple outputs of the same type.
    nameModifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MsSmoothOutputSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'h265PackagingType', 'msSmoothOutputSettings_h265PackagingType' - Only applicable when this output is referencing an H.265 video
-- description. Specifies whether MP4 segments should be packaged as HEV1
-- or HVC1.
--
-- 'nameModifier', 'msSmoothOutputSettings_nameModifier' - String concatenated to the end of the destination filename. Required for
-- multiple outputs of the same type.
newMsSmoothOutputSettings ::
  MsSmoothOutputSettings
newMsSmoothOutputSettings =
  MsSmoothOutputSettings'
    { h265PackagingType =
        Prelude.Nothing,
      nameModifier = Prelude.Nothing
    }

-- | Only applicable when this output is referencing an H.265 video
-- description. Specifies whether MP4 segments should be packaged as HEV1
-- or HVC1.
msSmoothOutputSettings_h265PackagingType :: Lens.Lens' MsSmoothOutputSettings (Prelude.Maybe MsSmoothH265PackagingType)
msSmoothOutputSettings_h265PackagingType = Lens.lens (\MsSmoothOutputSettings' {h265PackagingType} -> h265PackagingType) (\s@MsSmoothOutputSettings' {} a -> s {h265PackagingType = a} :: MsSmoothOutputSettings)

-- | String concatenated to the end of the destination filename. Required for
-- multiple outputs of the same type.
msSmoothOutputSettings_nameModifier :: Lens.Lens' MsSmoothOutputSettings (Prelude.Maybe Prelude.Text)
msSmoothOutputSettings_nameModifier = Lens.lens (\MsSmoothOutputSettings' {nameModifier} -> nameModifier) (\s@MsSmoothOutputSettings' {} a -> s {nameModifier = a} :: MsSmoothOutputSettings)

instance Prelude.FromJSON MsSmoothOutputSettings where
  parseJSON =
    Prelude.withObject
      "MsSmoothOutputSettings"
      ( \x ->
          MsSmoothOutputSettings'
            Prelude.<$> (x Prelude..:? "h265PackagingType")
            Prelude.<*> (x Prelude..:? "nameModifier")
      )

instance Prelude.Hashable MsSmoothOutputSettings

instance Prelude.NFData MsSmoothOutputSettings

instance Prelude.ToJSON MsSmoothOutputSettings where
  toJSON MsSmoothOutputSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("h265PackagingType" Prelude..=)
              Prelude.<$> h265PackagingType,
            ("nameModifier" Prelude..=)
              Prelude.<$> nameModifier
          ]
      )
