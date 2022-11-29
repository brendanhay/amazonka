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
-- Module      : Amazonka.MediaLive.Types.MediaPackageOutputDestinationSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.MediaPackageOutputDestinationSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Core.FromJSON
    MediaPackageOutputDestinationSettings
  where
  parseJSON =
    Core.withObject
      "MediaPackageOutputDestinationSettings"
      ( \x ->
          MediaPackageOutputDestinationSettings'
            Prelude.<$> (x Core..:? "channelId")
      )

instance
  Prelude.Hashable
    MediaPackageOutputDestinationSettings
  where
  hashWithSalt
    _salt
    MediaPackageOutputDestinationSettings' {..} =
      _salt `Prelude.hashWithSalt` channelId

instance
  Prelude.NFData
    MediaPackageOutputDestinationSettings
  where
  rnf MediaPackageOutputDestinationSettings' {..} =
    Prelude.rnf channelId

instance
  Core.ToJSON
    MediaPackageOutputDestinationSettings
  where
  toJSON MediaPackageOutputDestinationSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [("channelId" Core..=) Prelude.<$> channelId]
      )
