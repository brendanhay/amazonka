{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.AccountLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.AccountLimit
  ( AccountLimit (..)
  -- * Smart constructor
  , mkAccountLimit
  -- * Lenses
  , alName
  , alValue
  ) where

import qualified Network.AWS.CloudFormation.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The AccountLimit data type. 
--
-- CloudFormation has the following limits per account:
--
--     * Number of concurrent resources
--
--
--     * Number of stacks
--
--
--     * Number of stack outputs
--
--
-- For more information about these account limits, and other CloudFormation limits, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/cloudformation-limits.html AWS CloudFormation Limits> in the /AWS CloudFormation User Guide/ .
--
-- /See:/ 'mkAccountLimit' smart constructor.
data AccountLimit = AccountLimit'
  { name :: Core.Maybe Types.Name
    -- ^ The name of the account limit.
--
-- Values: @ConcurrentResourcesLimit@ | @StackLimit@ | @StackOutputsLimit@ 
  , value :: Core.Maybe Core.Int
    -- ^ The value that is associated with the account limit name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AccountLimit' value with any optional fields omitted.
mkAccountLimit
    :: AccountLimit
mkAccountLimit
  = AccountLimit'{name = Core.Nothing, value = Core.Nothing}

-- | The name of the account limit.
--
-- Values: @ConcurrentResourcesLimit@ | @StackLimit@ | @StackOutputsLimit@ 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alName :: Lens.Lens' AccountLimit (Core.Maybe Types.Name)
alName = Lens.field @"name"
{-# INLINEABLE alName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The value that is associated with the account limit name.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alValue :: Lens.Lens' AccountLimit (Core.Maybe Core.Int)
alValue = Lens.field @"value"
{-# INLINEABLE alValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromXML AccountLimit where
        parseXML x
          = AccountLimit' Core.<$>
              (x Core..@? "Name") Core.<*> x Core..@? "Value"
