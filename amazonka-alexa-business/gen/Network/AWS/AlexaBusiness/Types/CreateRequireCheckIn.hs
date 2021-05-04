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
-- Module      : Network.AWS.AlexaBusiness.Types.CreateRequireCheckIn
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.CreateRequireCheckIn where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Creates settings for the require check in feature that are applied to a
-- room profile. Require check in allows a meeting room’s Alexa or AVS
-- device to prompt the user to check in; otherwise, the room will be
-- released.
--
-- /See:/ 'newCreateRequireCheckIn' smart constructor.
data CreateRequireCheckIn = CreateRequireCheckIn'
  { -- | Duration between 5 and 20 minutes to determine when to release the room
    -- if it\'s not checked into.
    releaseAfterMinutes :: Prelude.Int,
    -- | Whether require check in is enabled or not.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateRequireCheckIn' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'releaseAfterMinutes', 'createRequireCheckIn_releaseAfterMinutes' - Duration between 5 and 20 minutes to determine when to release the room
-- if it\'s not checked into.
--
-- 'enabled', 'createRequireCheckIn_enabled' - Whether require check in is enabled or not.
newCreateRequireCheckIn ::
  -- | 'releaseAfterMinutes'
  Prelude.Int ->
  -- | 'enabled'
  Prelude.Bool ->
  CreateRequireCheckIn
newCreateRequireCheckIn
  pReleaseAfterMinutes_
  pEnabled_ =
    CreateRequireCheckIn'
      { releaseAfterMinutes =
          pReleaseAfterMinutes_,
        enabled = pEnabled_
      }

-- | Duration between 5 and 20 minutes to determine when to release the room
-- if it\'s not checked into.
createRequireCheckIn_releaseAfterMinutes :: Lens.Lens' CreateRequireCheckIn Prelude.Int
createRequireCheckIn_releaseAfterMinutes = Lens.lens (\CreateRequireCheckIn' {releaseAfterMinutes} -> releaseAfterMinutes) (\s@CreateRequireCheckIn' {} a -> s {releaseAfterMinutes = a} :: CreateRequireCheckIn)

-- | Whether require check in is enabled or not.
createRequireCheckIn_enabled :: Lens.Lens' CreateRequireCheckIn Prelude.Bool
createRequireCheckIn_enabled = Lens.lens (\CreateRequireCheckIn' {enabled} -> enabled) (\s@CreateRequireCheckIn' {} a -> s {enabled = a} :: CreateRequireCheckIn)

instance Prelude.Hashable CreateRequireCheckIn

instance Prelude.NFData CreateRequireCheckIn

instance Prelude.ToJSON CreateRequireCheckIn where
  toJSON CreateRequireCheckIn' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ReleaseAfterMinutes"
                  Prelude..= releaseAfterMinutes
              ),
            Prelude.Just ("Enabled" Prelude..= enabled)
          ]
      )
