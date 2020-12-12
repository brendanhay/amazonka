{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateIAMInstanceProfileSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateIAMInstanceProfileSpecification
  ( LaunchTemplateIAMInstanceProfileSpecification (..),

    -- * Smart constructor
    mkLaunchTemplateIAMInstanceProfileSpecification,

    -- * Lenses
    ltiapsARN,
    ltiapsName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an IAM instance profile.
--
-- /See:/ 'mkLaunchTemplateIAMInstanceProfileSpecification' smart constructor.
data LaunchTemplateIAMInstanceProfileSpecification = LaunchTemplateIAMInstanceProfileSpecification'
  { arn ::
      Lude.Maybe
        Lude.Text,
    name ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'LaunchTemplateIAMInstanceProfileSpecification' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the instance profile.
-- * 'name' - The name of the instance profile.
mkLaunchTemplateIAMInstanceProfileSpecification ::
  LaunchTemplateIAMInstanceProfileSpecification
mkLaunchTemplateIAMInstanceProfileSpecification =
  LaunchTemplateIAMInstanceProfileSpecification'
    { arn =
        Lude.Nothing,
      name = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the instance profile.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltiapsARN :: Lens.Lens' LaunchTemplateIAMInstanceProfileSpecification (Lude.Maybe Lude.Text)
ltiapsARN = Lens.lens (arn :: LaunchTemplateIAMInstanceProfileSpecification -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: LaunchTemplateIAMInstanceProfileSpecification)
{-# DEPRECATED ltiapsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the instance profile.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltiapsName :: Lens.Lens' LaunchTemplateIAMInstanceProfileSpecification (Lude.Maybe Lude.Text)
ltiapsName = Lens.lens (name :: LaunchTemplateIAMInstanceProfileSpecification -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: LaunchTemplateIAMInstanceProfileSpecification)
{-# DEPRECATED ltiapsName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromXML LaunchTemplateIAMInstanceProfileSpecification where
  parseXML x =
    LaunchTemplateIAMInstanceProfileSpecification'
      Lude.<$> (x Lude..@? "arn") Lude.<*> (x Lude..@? "name")
