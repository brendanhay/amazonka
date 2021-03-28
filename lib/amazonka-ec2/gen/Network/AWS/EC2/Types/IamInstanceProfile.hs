{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IamInstanceProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.IamInstanceProfile
  ( IamInstanceProfile (..)
  -- * Smart constructor
  , mkIamInstanceProfile
  -- * Lenses
  , iipArn
  , iipId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an IAM instance profile.
--
-- /See:/ 'mkIamInstanceProfile' smart constructor.
data IamInstanceProfile = IamInstanceProfile'
  { arn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the instance profile.
  , id :: Core.Maybe Core.Text
    -- ^ The ID of the instance profile.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IamInstanceProfile' value with any optional fields omitted.
mkIamInstanceProfile
    :: IamInstanceProfile
mkIamInstanceProfile
  = IamInstanceProfile'{arn = Core.Nothing, id = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the instance profile.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iipArn :: Lens.Lens' IamInstanceProfile (Core.Maybe Core.Text)
iipArn = Lens.field @"arn"
{-# INLINEABLE iipArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The ID of the instance profile.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iipId :: Lens.Lens' IamInstanceProfile (Core.Maybe Core.Text)
iipId = Lens.field @"id"
{-# INLINEABLE iipId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.FromXML IamInstanceProfile where
        parseXML x
          = IamInstanceProfile' Core.<$>
              (x Core..@? "arn") Core.<*> x Core..@? "id"
