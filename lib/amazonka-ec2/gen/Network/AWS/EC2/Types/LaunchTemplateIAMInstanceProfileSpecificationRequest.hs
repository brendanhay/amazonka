-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateIAMInstanceProfileSpecificationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateIAMInstanceProfileSpecificationRequest
  ( LaunchTemplateIAMInstanceProfileSpecificationRequest (..),

    -- * Smart constructor
    mkLaunchTemplateIAMInstanceProfileSpecificationRequest,

    -- * Lenses
    ltiapsrARN,
    ltiapsrName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An IAM instance profile.
--
-- /See:/ 'mkLaunchTemplateIAMInstanceProfileSpecificationRequest' smart constructor.
data LaunchTemplateIAMInstanceProfileSpecificationRequest = LaunchTemplateIAMInstanceProfileSpecificationRequest'
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

-- | Creates a value of 'LaunchTemplateIAMInstanceProfileSpecificationRequest' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the instance profile.
-- * 'name' - The name of the instance profile.
mkLaunchTemplateIAMInstanceProfileSpecificationRequest ::
  LaunchTemplateIAMInstanceProfileSpecificationRequest
mkLaunchTemplateIAMInstanceProfileSpecificationRequest =
  LaunchTemplateIAMInstanceProfileSpecificationRequest'
    { arn =
        Lude.Nothing,
      name = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the instance profile.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltiapsrARN :: Lens.Lens' LaunchTemplateIAMInstanceProfileSpecificationRequest (Lude.Maybe Lude.Text)
ltiapsrARN = Lens.lens (arn :: LaunchTemplateIAMInstanceProfileSpecificationRequest -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: LaunchTemplateIAMInstanceProfileSpecificationRequest)
{-# DEPRECATED ltiapsrARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the instance profile.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltiapsrName :: Lens.Lens' LaunchTemplateIAMInstanceProfileSpecificationRequest (Lude.Maybe Lude.Text)
ltiapsrName = Lens.lens (name :: LaunchTemplateIAMInstanceProfileSpecificationRequest -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: LaunchTemplateIAMInstanceProfileSpecificationRequest)
{-# DEPRECATED ltiapsrName "Use generic-lens or generic-optics with 'name' instead." #-}

instance
  Lude.ToQuery
    LaunchTemplateIAMInstanceProfileSpecificationRequest
  where
  toQuery LaunchTemplateIAMInstanceProfileSpecificationRequest' {..} =
    Lude.mconcat ["Arn" Lude.=: arn, "Name" Lude.=: name]
