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
    upcAutoAccept,
    upcAfterContactWorkTimeLimit,
    upcPhoneType,
    upcDeskPhoneNumber,
  )
where

import Network.AWS.Connect.Types.PhoneType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the phone configuration settings for a user.
--
-- /See:/ 'mkUserPhoneConfig' smart constructor.
data UserPhoneConfig = UserPhoneConfig'
  { -- | The Auto accept setting.
    autoAccept :: Lude.Maybe Lude.Bool,
    -- | The After Call Work (ACW) timeout setting, in seconds.
    afterContactWorkTimeLimit :: Lude.Maybe Lude.Natural,
    -- | The phone type.
    phoneType :: PhoneType,
    -- | The phone number for the user's desk phone.
    deskPhoneNumber :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserPhoneConfig' with the minimum fields required to make a request.
--
-- * 'autoAccept' - The Auto accept setting.
-- * 'afterContactWorkTimeLimit' - The After Call Work (ACW) timeout setting, in seconds.
-- * 'phoneType' - The phone type.
-- * 'deskPhoneNumber' - The phone number for the user's desk phone.
mkUserPhoneConfig ::
  -- | 'phoneType'
  PhoneType ->
  UserPhoneConfig
mkUserPhoneConfig pPhoneType_ =
  UserPhoneConfig'
    { autoAccept = Lude.Nothing,
      afterContactWorkTimeLimit = Lude.Nothing,
      phoneType = pPhoneType_,
      deskPhoneNumber = Lude.Nothing
    }

-- | The Auto accept setting.
--
-- /Note:/ Consider using 'autoAccept' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcAutoAccept :: Lens.Lens' UserPhoneConfig (Lude.Maybe Lude.Bool)
upcAutoAccept = Lens.lens (autoAccept :: UserPhoneConfig -> Lude.Maybe Lude.Bool) (\s a -> s {autoAccept = a} :: UserPhoneConfig)
{-# DEPRECATED upcAutoAccept "Use generic-lens or generic-optics with 'autoAccept' instead." #-}

-- | The After Call Work (ACW) timeout setting, in seconds.
--
-- /Note:/ Consider using 'afterContactWorkTimeLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcAfterContactWorkTimeLimit :: Lens.Lens' UserPhoneConfig (Lude.Maybe Lude.Natural)
upcAfterContactWorkTimeLimit = Lens.lens (afterContactWorkTimeLimit :: UserPhoneConfig -> Lude.Maybe Lude.Natural) (\s a -> s {afterContactWorkTimeLimit = a} :: UserPhoneConfig)
{-# DEPRECATED upcAfterContactWorkTimeLimit "Use generic-lens or generic-optics with 'afterContactWorkTimeLimit' instead." #-}

-- | The phone type.
--
-- /Note:/ Consider using 'phoneType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcPhoneType :: Lens.Lens' UserPhoneConfig PhoneType
upcPhoneType = Lens.lens (phoneType :: UserPhoneConfig -> PhoneType) (\s a -> s {phoneType = a} :: UserPhoneConfig)
{-# DEPRECATED upcPhoneType "Use generic-lens or generic-optics with 'phoneType' instead." #-}

-- | The phone number for the user's desk phone.
--
-- /Note:/ Consider using 'deskPhoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcDeskPhoneNumber :: Lens.Lens' UserPhoneConfig (Lude.Maybe Lude.Text)
upcDeskPhoneNumber = Lens.lens (deskPhoneNumber :: UserPhoneConfig -> Lude.Maybe Lude.Text) (\s a -> s {deskPhoneNumber = a} :: UserPhoneConfig)
{-# DEPRECATED upcDeskPhoneNumber "Use generic-lens or generic-optics with 'deskPhoneNumber' instead." #-}

instance Lude.FromJSON UserPhoneConfig where
  parseJSON =
    Lude.withObject
      "UserPhoneConfig"
      ( \x ->
          UserPhoneConfig'
            Lude.<$> (x Lude..:? "AutoAccept")
            Lude.<*> (x Lude..:? "AfterContactWorkTimeLimit")
            Lude.<*> (x Lude..: "PhoneType")
            Lude.<*> (x Lude..:? "DeskPhoneNumber")
      )

instance Lude.ToJSON UserPhoneConfig where
  toJSON UserPhoneConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AutoAccept" Lude..=) Lude.<$> autoAccept,
            ("AfterContactWorkTimeLimit" Lude..=)
              Lude.<$> afterContactWorkTimeLimit,
            Lude.Just ("PhoneType" Lude..= phoneType),
            ("DeskPhoneNumber" Lude..=) Lude.<$> deskPhoneNumber
          ]
      )
