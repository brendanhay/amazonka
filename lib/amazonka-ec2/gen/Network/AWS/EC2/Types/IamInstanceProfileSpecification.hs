{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IamInstanceProfileSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.IamInstanceProfileSpecification
  ( IamInstanceProfileSpecification (..)
  -- * Smart constructor
  , mkIamInstanceProfileSpecification
  -- * Lenses
  , iipsArn
  , iipsName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an IAM instance profile.
--
-- /See:/ 'mkIamInstanceProfileSpecification' smart constructor.
data IamInstanceProfileSpecification = IamInstanceProfileSpecification'
  { arn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the instance profile.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the instance profile.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IamInstanceProfileSpecification' value with any optional fields omitted.
mkIamInstanceProfileSpecification
    :: IamInstanceProfileSpecification
mkIamInstanceProfileSpecification
  = IamInstanceProfileSpecification'{arn = Core.Nothing,
                                     name = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the instance profile.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iipsArn :: Lens.Lens' IamInstanceProfileSpecification (Core.Maybe Core.Text)
iipsArn = Lens.field @"arn"
{-# INLINEABLE iipsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The name of the instance profile.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iipsName :: Lens.Lens' IamInstanceProfileSpecification (Core.Maybe Core.Text)
iipsName = Lens.field @"name"
{-# INLINEABLE iipsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery IamInstanceProfileSpecification where
        toQuery IamInstanceProfileSpecification{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Arn") arn Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Name") name

instance Core.FromXML IamInstanceProfileSpecification where
        parseXML x
          = IamInstanceProfileSpecification' Core.<$>
              (x Core..@? "arn") Core.<*> x Core..@? "name"
