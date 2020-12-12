{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstancesIAMInstanceProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstancesIAMInstanceProfile
  ( ScheduledInstancesIAMInstanceProfile (..),

    -- * Smart constructor
    mkScheduledInstancesIAMInstanceProfile,

    -- * Lenses
    siiapARN,
    siiapName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an IAM instance profile for a Scheduled Instance.
--
-- /See:/ 'mkScheduledInstancesIAMInstanceProfile' smart constructor.
data ScheduledInstancesIAMInstanceProfile = ScheduledInstancesIAMInstanceProfile'
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
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScheduledInstancesIAMInstanceProfile' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN).
-- * 'name' - The name.
mkScheduledInstancesIAMInstanceProfile ::
  ScheduledInstancesIAMInstanceProfile
mkScheduledInstancesIAMInstanceProfile =
  ScheduledInstancesIAMInstanceProfile'
    { arn = Lude.Nothing,
      name = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siiapARN :: Lens.Lens' ScheduledInstancesIAMInstanceProfile (Lude.Maybe Lude.Text)
siiapARN = Lens.lens (arn :: ScheduledInstancesIAMInstanceProfile -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: ScheduledInstancesIAMInstanceProfile)
{-# DEPRECATED siiapARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siiapName :: Lens.Lens' ScheduledInstancesIAMInstanceProfile (Lude.Maybe Lude.Text)
siiapName = Lens.lens (name :: ScheduledInstancesIAMInstanceProfile -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ScheduledInstancesIAMInstanceProfile)
{-# DEPRECATED siiapName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.ToQuery ScheduledInstancesIAMInstanceProfile where
  toQuery ScheduledInstancesIAMInstanceProfile' {..} =
    Lude.mconcat ["Arn" Lude.=: arn, "Name" Lude.=: name]
