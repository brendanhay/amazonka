{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IAMInstanceProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IAMInstanceProfile
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

-- | Describes an IAM instance profile.
--
-- /See:/ 'mkIAMInstanceProfile' smart constructor.
data IAMInstanceProfile = IAMInstanceProfile'
  { -- | The Amazon Resource Name (ARN) of the instance profile.
    arn :: Lude.Maybe Lude.Text,
    -- | The ID of the instance profile.
    id :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IAMInstanceProfile' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the instance profile.
-- * 'id' - The ID of the instance profile.
mkIAMInstanceProfile ::
  IAMInstanceProfile
mkIAMInstanceProfile =
  IAMInstanceProfile' {arn = Lude.Nothing, id = Lude.Nothing}

-- | The Amazon Resource Name (ARN) of the instance profile.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iapARN :: Lens.Lens' IAMInstanceProfile (Lude.Maybe Lude.Text)
iapARN = Lens.lens (arn :: IAMInstanceProfile -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: IAMInstanceProfile)
{-# DEPRECATED iapARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The ID of the instance profile.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iapId :: Lens.Lens' IAMInstanceProfile (Lude.Maybe Lude.Text)
iapId = Lens.lens (id :: IAMInstanceProfile -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: IAMInstanceProfile)
{-# DEPRECATED iapId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromXML IAMInstanceProfile where
  parseXML x =
    IAMInstanceProfile'
      Lude.<$> (x Lude..@? "arn") Lude.<*> (x Lude..@? "id")
