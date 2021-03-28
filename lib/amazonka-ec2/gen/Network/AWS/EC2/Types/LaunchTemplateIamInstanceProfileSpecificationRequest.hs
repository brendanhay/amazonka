{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateIamInstanceProfileSpecificationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LaunchTemplateIamInstanceProfileSpecificationRequest
  ( LaunchTemplateIamInstanceProfileSpecificationRequest (..)
  -- * Smart constructor
  , mkLaunchTemplateIamInstanceProfileSpecificationRequest
  -- * Lenses
  , ltiipsrArn
  , ltiipsrName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An IAM instance profile.
--
-- /See:/ 'mkLaunchTemplateIamInstanceProfileSpecificationRequest' smart constructor.
data LaunchTemplateIamInstanceProfileSpecificationRequest = LaunchTemplateIamInstanceProfileSpecificationRequest'
  { arn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the instance profile.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the instance profile.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplateIamInstanceProfileSpecificationRequest' value with any optional fields omitted.
mkLaunchTemplateIamInstanceProfileSpecificationRequest
    :: LaunchTemplateIamInstanceProfileSpecificationRequest
mkLaunchTemplateIamInstanceProfileSpecificationRequest
  = LaunchTemplateIamInstanceProfileSpecificationRequest'{arn =
                                                            Core.Nothing,
                                                          name = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the instance profile.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltiipsrArn :: Lens.Lens' LaunchTemplateIamInstanceProfileSpecificationRequest (Core.Maybe Core.Text)
ltiipsrArn = Lens.field @"arn"
{-# INLINEABLE ltiipsrArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The name of the instance profile.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltiipsrName :: Lens.Lens' LaunchTemplateIamInstanceProfileSpecificationRequest (Core.Maybe Core.Text)
ltiipsrName = Lens.field @"name"
{-# INLINEABLE ltiipsrName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery
           LaunchTemplateIamInstanceProfileSpecificationRequest
         where
        toQuery LaunchTemplateIamInstanceProfileSpecificationRequest{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Arn") arn Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Name") name
