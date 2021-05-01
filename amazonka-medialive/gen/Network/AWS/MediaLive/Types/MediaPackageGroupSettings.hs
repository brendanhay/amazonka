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
-- Module      : Network.AWS.MediaLive.Types.MediaPackageGroupSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MediaPackageGroupSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.OutputLocationRef
import qualified Network.AWS.Prelude as Prelude

-- | Media Package Group Settings
--
-- /See:/ 'newMediaPackageGroupSettings' smart constructor.
data MediaPackageGroupSettings = MediaPackageGroupSettings'
  { -- | MediaPackage channel destination.
    destination :: OutputLocationRef
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MediaPackageGroupSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'mediaPackageGroupSettings_destination' - MediaPackage channel destination.
newMediaPackageGroupSettings ::
  -- | 'destination'
  OutputLocationRef ->
  MediaPackageGroupSettings
newMediaPackageGroupSettings pDestination_ =
  MediaPackageGroupSettings'
    { destination =
        pDestination_
    }

-- | MediaPackage channel destination.
mediaPackageGroupSettings_destination :: Lens.Lens' MediaPackageGroupSettings OutputLocationRef
mediaPackageGroupSettings_destination = Lens.lens (\MediaPackageGroupSettings' {destination} -> destination) (\s@MediaPackageGroupSettings' {} a -> s {destination = a} :: MediaPackageGroupSettings)

instance Prelude.FromJSON MediaPackageGroupSettings where
  parseJSON =
    Prelude.withObject
      "MediaPackageGroupSettings"
      ( \x ->
          MediaPackageGroupSettings'
            Prelude.<$> (x Prelude..: "destination")
      )

instance Prelude.Hashable MediaPackageGroupSettings

instance Prelude.NFData MediaPackageGroupSettings

instance Prelude.ToJSON MediaPackageGroupSettings where
  toJSON MediaPackageGroupSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("destination" Prelude..= destination)
          ]
      )
