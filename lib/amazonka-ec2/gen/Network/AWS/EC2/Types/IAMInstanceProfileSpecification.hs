{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IAMInstanceProfileSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IAMInstanceProfileSpecification
  ( IAMInstanceProfileSpecification (..),

    -- * Smart constructor
    mkIAMInstanceProfileSpecification,

    -- * Lenses
    iapsARN,
    iapsName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an IAM instance profile.
--
-- /See:/ 'mkIAMInstanceProfileSpecification' smart constructor.
data IAMInstanceProfileSpecification = IAMInstanceProfileSpecification'
  { arn ::
      Lude.Maybe Lude.Text,
    name ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IAMInstanceProfileSpecification' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the instance profile.
-- * 'name' - The name of the instance profile.
mkIAMInstanceProfileSpecification ::
  IAMInstanceProfileSpecification
mkIAMInstanceProfileSpecification =
  IAMInstanceProfileSpecification'
    { arn = Lude.Nothing,
      name = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the instance profile.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iapsARN :: Lens.Lens' IAMInstanceProfileSpecification (Lude.Maybe Lude.Text)
iapsARN = Lens.lens (arn :: IAMInstanceProfileSpecification -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: IAMInstanceProfileSpecification)
{-# DEPRECATED iapsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the instance profile.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iapsName :: Lens.Lens' IAMInstanceProfileSpecification (Lude.Maybe Lude.Text)
iapsName = Lens.lens (name :: IAMInstanceProfileSpecification -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: IAMInstanceProfileSpecification)
{-# DEPRECATED iapsName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromXML IAMInstanceProfileSpecification where
  parseXML x =
    IAMInstanceProfileSpecification'
      Lude.<$> (x Lude..@? "arn") Lude.<*> (x Lude..@? "name")

instance Lude.ToQuery IAMInstanceProfileSpecification where
  toQuery IAMInstanceProfileSpecification' {..} =
    Lude.mconcat ["Arn" Lude.=: arn, "Name" Lude.=: name]
