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
    upcDeskPhoneNumber,
    upcPhoneType,
  )
where

import Network.AWS.Connect.Types.PhoneType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the phone configuration settings for a user.
--
-- /See:/ 'mkUserPhoneConfig' smart constructor.
data UserPhoneConfig = UserPhoneConfig'
  { autoAccept ::
      Lude.Maybe Lude.Bool,
    afterContactWorkTimeLimit :: Lude.Maybe Lude.Natural,
    deskPhoneNumber :: Lude.Maybe Lude.Text,
    phoneType :: PhoneType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserPhoneConfig' with the minimum fields required to make a request.
--
-- * 'afterContactWorkTimeLimit' - The After Call Work (ACW) timeout setting, in seconds.
-- * 'autoAccept' - The Auto accept setting.
-- * 'deskPhoneNumber' - The phone number for the user's desk phone.
-- * 'phoneType' - The phone type.
mkUserPhoneConfig ::
  -- | 'phoneType'
  PhoneType ->
  UserPhoneConfig
mkUserPhoneConfig pPhoneType_ =
  UserPhoneConfig'
    { autoAccept = Lude.Nothing,
      afterContactWorkTimeLimit = Lude.Nothing,
      deskPhoneNumber = Lude.Nothing,
      phoneType = pPhoneType_
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

-- | The phone number for the user's desk phone.
--
-- /Note:/ Consider using 'deskPhoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcDeskPhoneNumber :: Lens.Lens' UserPhoneConfig (Lude.Maybe Lude.Text)
upcDeskPhoneNumber = Lens.lens (deskPhoneNumber :: UserPhoneConfig -> Lude.Maybe Lude.Text) (\s a -> s {deskPhoneNumber = a} :: UserPhoneConfig)
{-# DEPRECATED upcDeskPhoneNumber "Use generic-lens or generic-optics with 'deskPhoneNumber' instead." #-}

-- | The phone type.
--
-- /Note:/ Consider using 'phoneType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcPhoneType :: Lens.Lens' UserPhoneConfig PhoneType
upcPhoneType = Lens.lens (phoneType :: UserPhoneConfig -> PhoneType) (\s a -> s {phoneType = a} :: UserPhoneConfig)
{-# DEPRECATED upcPhoneType "Use generic-lens or generic-optics with 'phoneType' instead." #-}

instance Lude.FromJSON UserPhoneConfig where
  parseJSON =
    Lude.withObject
      "UserPhoneConfig"
      ( \x ->
          UserPhoneConfig'
            Lude.<$> (x Lude..:? "AutoAccept")
            Lude.<*> (x Lude..:? "AfterContactWorkTimeLimit")
            Lude.<*> (x Lude..:? "DeskPhoneNumber")
            Lude.<*> (x Lude..: "PhoneType")
      )

instance Lude.ToJSON UserPhoneConfig where
  toJSON UserPhoneConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AutoAccept" Lude..=) Lude.<$> autoAccept,
            ("AfterContactWorkTimeLimit" Lude..=)
              Lude.<$> afterContactWorkTimeLimit,
            ("DeskPhoneNumber" Lude..=) Lude.<$> deskPhoneNumber,
            Lude.Just ("PhoneType" Lude..= phoneType)
          ]
      )
