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
-- Module      : Network.AWS.MediaLive.Types.MediaPackageOutputDestinationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MediaPackageOutputDestinationSettings where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | MediaPackage Output Destination Settings
--
-- /See:/ 'newMediaPackageOutputDestinationSettings' smart constructor.
data MediaPackageOutputDestinationSettings = MediaPackageOutputDestinationSettings'
  { -- | ID of the channel in MediaPackage that is the destination for this
    -- output group. You do not need to specify the individual inputs in
    -- MediaPackage; MediaLive will handle the connection of the two MediaLive
    -- pipelines to the two MediaPackage inputs. The MediaPackage channel and
    -- MediaLive channel must be in the same region.
    channelId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MediaPackageOutputDestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelId', 'mediaPackageOutputDestinationSettings_channelId' - ID of the channel in MediaPackage that is the destination for this
-- output group. You do not need to specify the individual inputs in
-- MediaPackage; MediaLive will handle the connection of the two MediaLive
-- pipelines to the two MediaPackage inputs. The MediaPackage channel and
-- MediaLive channel must be in the same region.
newMediaPackageOutputDestinationSettings ::
  MediaPackageOutputDestinationSettings
newMediaPackageOutputDestinationSettings =
  MediaPackageOutputDestinationSettings'
    { channelId =
        Prelude.Nothing
    }

-- | ID of the channel in MediaPackage that is the destination for this
-- output group. You do not need to specify the individual inputs in
-- MediaPackage; MediaLive will handle the connection of the two MediaLive
-- pipelines to the two MediaPackage inputs. The MediaPackage channel and
-- MediaLive channel must be in the same region.
mediaPackageOutputDestinationSettings_channelId :: Lens.Lens' MediaPackageOutputDestinationSettings (Prelude.Maybe Prelude.Text)
mediaPackageOutputDestinationSettings_channelId = Lens.lens (\MediaPackageOutputDestinationSettings' {channelId} -> channelId) (\s@MediaPackageOutputDestinationSettings' {} a -> s {channelId = a} :: MediaPackageOutputDestinationSettings)

instance
  Prelude.FromJSON
    MediaPackageOutputDestinationSettings
  where
  parseJSON =
    Prelude.withObject
      "MediaPackageOutputDestinationSettings"
      ( \x ->
          MediaPackageOutputDestinationSettings'
            Prelude.<$> (x Prelude..:? "channelId")
      )

instance
  Prelude.Hashable
    MediaPackageOutputDestinationSettings

instance
  Prelude.NFData
    MediaPackageOutputDestinationSettings

instance
  Prelude.ToJSON
    MediaPackageOutputDestinationSettings
  where
  toJSON MediaPackageOutputDestinationSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("channelId" Prelude..=) Prelude.<$> channelId]
      )
