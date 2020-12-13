{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.IAMInstanceProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.IAMInstanceProfile
  ( IAMInstanceProfile (..),

    -- * Smart constructor
    mkIAMInstanceProfile,

    -- * Lenses
    iapARN,
    iapId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the EC2 instance profile.
--
-- /See:/ 'mkIAMInstanceProfile' smart constructor.
data IAMInstanceProfile = IAMInstanceProfile'
  { -- | The profile ARN of the EC2 instance.
    arn :: Lude.Maybe Lude.Text,
    -- | The profile ID of the EC2 instance.
    id :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IAMInstanceProfile' with the minimum fields required to make a request.
--
-- * 'arn' - The profile ARN of the EC2 instance.
-- * 'id' - The profile ID of the EC2 instance.
mkIAMInstanceProfile ::
  IAMInstanceProfile
mkIAMInstanceProfile =
  IAMInstanceProfile' {arn = Lude.Nothing, id = Lude.Nothing}

-- | The profile ARN of the EC2 instance.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iapARN :: Lens.Lens' IAMInstanceProfile (Lude.Maybe Lude.Text)
iapARN = Lens.lens (arn :: IAMInstanceProfile -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: IAMInstanceProfile)
{-# DEPRECATED iapARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The profile ID of the EC2 instance.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iapId :: Lens.Lens' IAMInstanceProfile (Lude.Maybe Lude.Text)
iapId = Lens.lens (id :: IAMInstanceProfile -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: IAMInstanceProfile)
{-# DEPRECATED iapId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON IAMInstanceProfile where
  parseJSON =
    Lude.withObject
      "IAMInstanceProfile"
      ( \x ->
          IAMInstanceProfile'
            Lude.<$> (x Lude..:? "arn") Lude.<*> (x Lude..:? "id")
      )
