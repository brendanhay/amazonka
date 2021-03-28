{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.InstanceState
  ( InstanceState (..)
  -- * Smart constructor
  , mkInstanceState
  -- * Lenses
  , isCode
  , isName
  ) where

import qualified Network.AWS.EC2.Types.InstanceStateName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the current state of an instance.
--
-- /See:/ 'mkInstanceState' smart constructor.
data InstanceState = InstanceState'
  { code :: Core.Int
    -- ^ The state of the instance as a 16-bit unsigned integer. 
--
-- The high byte is all of the bits between 2^8 and (2^16)-1, which equals decimal values between 256 and 65,535. These numerical values are used for internal purposes and should be ignored.
-- The low byte is all of the bits between 2^0 and (2^8)-1, which equals decimal values between 0 and 255. 
-- The valid values for instance-state-code will all be in the range of the low byte and they are:
--
--     * @0@ : @pending@ 
--
--
--     * @16@ : @running@ 
--
--
--     * @32@ : @shutting-down@ 
--
--
--     * @48@ : @terminated@ 
--
--
--     * @64@ : @stopping@ 
--
--
--     * @80@ : @stopped@ 
--
--
-- You can ignore the high byte value by zeroing out all of the bits above 2^8 or 256 in decimal.
  , name :: Types.InstanceStateName
    -- ^ The current state of the instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceState' value with any optional fields omitted.
mkInstanceState
    :: Core.Int -- ^ 'code'
    -> Types.InstanceStateName -- ^ 'name'
    -> InstanceState
mkInstanceState code name = InstanceState'{code, name}

-- | The state of the instance as a 16-bit unsigned integer. 
--
-- The high byte is all of the bits between 2^8 and (2^16)-1, which equals decimal values between 256 and 65,535. These numerical values are used for internal purposes and should be ignored.
-- The low byte is all of the bits between 2^0 and (2^8)-1, which equals decimal values between 0 and 255. 
-- The valid values for instance-state-code will all be in the range of the low byte and they are:
--
--     * @0@ : @pending@ 
--
--
--     * @16@ : @running@ 
--
--
--     * @32@ : @shutting-down@ 
--
--
--     * @48@ : @terminated@ 
--
--
--     * @64@ : @stopping@ 
--
--
--     * @80@ : @stopped@ 
--
--
-- You can ignore the high byte value by zeroing out all of the bits above 2^8 or 256 in decimal.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isCode :: Lens.Lens' InstanceState Core.Int
isCode = Lens.field @"code"
{-# INLINEABLE isCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | The current state of the instance.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isName :: Lens.Lens' InstanceState Types.InstanceStateName
isName = Lens.field @"name"
{-# INLINEABLE isName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromXML InstanceState where
        parseXML x
          = InstanceState' Core.<$>
              (x Core..@ "code") Core.<*> x Core..@ "name"
