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
-- Module      : Network.AWS.MediaLive.Types.FrameCaptureGroupSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FrameCaptureGroupSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.OutputLocationRef
import qualified Network.AWS.Prelude as Prelude

-- | Frame Capture Group Settings
--
-- /See:/ 'newFrameCaptureGroupSettings' smart constructor.
data FrameCaptureGroupSettings = FrameCaptureGroupSettings'
  { -- | The destination for the frame capture files. Either the URI for an
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FrameCaptureGroupSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
    { destination =
        pDestination_
    }

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

instance Prelude.FromJSON FrameCaptureGroupSettings where
  parseJSON =
    Prelude.withObject
      "FrameCaptureGroupSettings"
      ( \x ->
          FrameCaptureGroupSettings'
            Prelude.<$> (x Prelude..: "destination")
      )

instance Prelude.Hashable FrameCaptureGroupSettings

instance Prelude.NFData FrameCaptureGroupSettings

instance Prelude.ToJSON FrameCaptureGroupSettings where
  toJSON FrameCaptureGroupSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("destination" Prelude..= destination)
          ]
      )
