{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateIamInstanceProfileSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LaunchTemplateIamInstanceProfileSpecification
  ( LaunchTemplateIamInstanceProfileSpecification (..)
  -- * Smart constructor
  , mkLaunchTemplateIamInstanceProfileSpecification
  -- * Lenses
  , ltiipsArn
  , ltiipsName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an IAM instance profile.
--
-- /See:/ 'mkLaunchTemplateIamInstanceProfileSpecification' smart constructor.
data LaunchTemplateIamInstanceProfileSpecification = LaunchTemplateIamInstanceProfileSpecification'
  { arn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the instance profile.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the instance profile.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplateIamInstanceProfileSpecification' value with any optional fields omitted.
mkLaunchTemplateIamInstanceProfileSpecification
    :: LaunchTemplateIamInstanceProfileSpecification
mkLaunchTemplateIamInstanceProfileSpecification
  = LaunchTemplateIamInstanceProfileSpecification'{arn =
                                                     Core.Nothing,
                                                   name = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the instance profile.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltiipsArn :: Lens.Lens' LaunchTemplateIamInstanceProfileSpecification (Core.Maybe Core.Text)
ltiipsArn = Lens.field @"arn"
{-# INLINEABLE ltiipsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The name of the instance profile.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltiipsName :: Lens.Lens' LaunchTemplateIamInstanceProfileSpecification (Core.Maybe Core.Text)
ltiipsName = Lens.field @"name"
{-# INLINEABLE ltiipsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromXML LaunchTemplateIamInstanceProfileSpecification
         where
        parseXML x
          = LaunchTemplateIamInstanceProfileSpecification' Core.<$>
              (x Core..@? "arn") Core.<*> x Core..@? "name"
