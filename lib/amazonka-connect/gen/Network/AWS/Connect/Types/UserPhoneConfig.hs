{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.UserPhoneConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.UserPhoneConfig
  ( UserPhoneConfig (..),

    -- * Smart constructor
    mkUserPhoneConfig,

    -- * Lenses
    upcPhoneType,
    upcAfterContactWorkTimeLimit,
    upcAutoAccept,
    upcDeskPhoneNumber,
  )
where

import qualified Network.AWS.Connect.Types.DeskPhoneNumber as Types
import qualified Network.AWS.Connect.Types.PhoneType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the phone configuration settings for a user.
--
-- /See:/ 'mkUserPhoneConfig' smart constructor.
data UserPhoneConfig = UserPhoneConfig'
  { -- | The phone type.
    phoneType :: Types.PhoneType,
    -- | The After Call Work (ACW) timeout setting, in seconds.
    afterContactWorkTimeLimit :: Core.Maybe Core.Natural,
    -- | The Auto accept setting.
    autoAccept :: Core.Maybe Core.Bool,
    -- | The phone number for the user's desk phone.
    deskPhoneNumber :: Core.Maybe Types.DeskPhoneNumber
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UserPhoneConfig' value with any optional fields omitted.
mkUserPhoneConfig ::
  -- | 'phoneType'
  Types.PhoneType ->
  UserPhoneConfig
mkUserPhoneConfig phoneType =
  UserPhoneConfig'
    { phoneType,
      afterContactWorkTimeLimit = Core.Nothing,
      autoAccept = Core.Nothing,
      deskPhoneNumber = Core.Nothing
    }

-- | The phone type.
--
-- /Note:/ Consider using 'phoneType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcPhoneType :: Lens.Lens' UserPhoneConfig Types.PhoneType
upcPhoneType = Lens.field @"phoneType"
{-# DEPRECATED upcPhoneType "Use generic-lens or generic-optics with 'phoneType' instead." #-}

-- | The After Call Work (ACW) timeout setting, in seconds.
--
-- /Note:/ Consider using 'afterContactWorkTimeLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcAfterContactWorkTimeLimit :: Lens.Lens' UserPhoneConfig (Core.Maybe Core.Natural)
upcAfterContactWorkTimeLimit = Lens.field @"afterContactWorkTimeLimit"
{-# DEPRECATED upcAfterContactWorkTimeLimit "Use generic-lens or generic-optics with 'afterContactWorkTimeLimit' instead." #-}

-- | The Auto accept setting.
--
-- /Note:/ Consider using 'autoAccept' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcAutoAccept :: Lens.Lens' UserPhoneConfig (Core.Maybe Core.Bool)
upcAutoAccept = Lens.field @"autoAccept"
{-# DEPRECATED upcAutoAccept "Use generic-lens or generic-optics with 'autoAccept' instead." #-}

-- | The phone number for the user's desk phone.
--
-- /Note:/ Consider using 'deskPhoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcDeskPhoneNumber :: Lens.Lens' UserPhoneConfig (Core.Maybe Types.DeskPhoneNumber)
upcDeskPhoneNumber = Lens.field @"deskPhoneNumber"
{-# DEPRECATED upcDeskPhoneNumber "Use generic-lens or generic-optics with 'deskPhoneNumber' instead." #-}

instance Core.FromJSON UserPhoneConfig where
  toJSON UserPhoneConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("PhoneType" Core..= phoneType),
            ("AfterContactWorkTimeLimit" Core..=)
              Core.<$> afterContactWorkTimeLimit,
            ("AutoAccept" Core..=) Core.<$> autoAccept,
            ("DeskPhoneNumber" Core..=) Core.<$> deskPhoneNumber
          ]
      )

instance Core.FromJSON UserPhoneConfig where
  parseJSON =
    Core.withObject "UserPhoneConfig" Core.$
      \x ->
        UserPhoneConfig'
          Core.<$> (x Core..: "PhoneType")
          Core.<*> (x Core..:? "AfterContactWorkTimeLimit")
          Core.<*> (x Core..:? "AutoAccept")
          Core.<*> (x Core..:? "DeskPhoneNumber")
