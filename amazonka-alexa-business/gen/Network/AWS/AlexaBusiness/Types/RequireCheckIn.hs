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
-- Module      : Network.AWS.AlexaBusiness.Types.RequireCheckIn
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.RequireCheckIn where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Settings for the require check in feature that are applied to a room
-- profile. Require check in allows a meeting room’s Alexa or AVS device to
-- prompt the user to check in; otherwise, the room will be released.
--
-- /See:/ 'newRequireCheckIn' smart constructor.
data RequireCheckIn = RequireCheckIn'
  { -- | Duration between 5 and 20 minutes to determine when to release the room
    -- if it\'s not checked into.
    releaseAfterMinutes :: Prelude.Maybe Prelude.Int,
    -- | Whether require check in is enabled or not.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RequireCheckIn' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'releaseAfterMinutes', 'requireCheckIn_releaseAfterMinutes' - Duration between 5 and 20 minutes to determine when to release the room
-- if it\'s not checked into.
--
-- 'enabled', 'requireCheckIn_enabled' - Whether require check in is enabled or not.
newRequireCheckIn ::
  RequireCheckIn
newRequireCheckIn =
  RequireCheckIn'
    { releaseAfterMinutes =
        Prelude.Nothing,
      enabled = Prelude.Nothing
    }

-- | Duration between 5 and 20 minutes to determine when to release the room
-- if it\'s not checked into.
requireCheckIn_releaseAfterMinutes :: Lens.Lens' RequireCheckIn (Prelude.Maybe Prelude.Int)
requireCheckIn_releaseAfterMinutes = Lens.lens (\RequireCheckIn' {releaseAfterMinutes} -> releaseAfterMinutes) (\s@RequireCheckIn' {} a -> s {releaseAfterMinutes = a} :: RequireCheckIn)

-- | Whether require check in is enabled or not.
requireCheckIn_enabled :: Lens.Lens' RequireCheckIn (Prelude.Maybe Prelude.Bool)
requireCheckIn_enabled = Lens.lens (\RequireCheckIn' {enabled} -> enabled) (\s@RequireCheckIn' {} a -> s {enabled = a} :: RequireCheckIn)

instance Prelude.FromJSON RequireCheckIn where
  parseJSON =
    Prelude.withObject
      "RequireCheckIn"
      ( \x ->
          RequireCheckIn'
            Prelude.<$> (x Prelude..:? "ReleaseAfterMinutes")
            Prelude.<*> (x Prelude..:? "Enabled")
      )

instance Prelude.Hashable RequireCheckIn

instance Prelude.NFData RequireCheckIn
