{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IcmpTypeCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.IcmpTypeCode
  ( IcmpTypeCode (..)
  -- * Smart constructor
  , mkIcmpTypeCode
  -- * Lenses
  , itcCode
  , itcType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the ICMP type and code.
--
-- /See:/ 'mkIcmpTypeCode' smart constructor.
data IcmpTypeCode = IcmpTypeCode'
  { code :: Core.Maybe Core.Int
    -- ^ The ICMP code. A value of -1 means all codes for the specified ICMP type.
  , type' :: Core.Maybe Core.Int
    -- ^ The ICMP type. A value of -1 means all types.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IcmpTypeCode' value with any optional fields omitted.
mkIcmpTypeCode
    :: IcmpTypeCode
mkIcmpTypeCode
  = IcmpTypeCode'{code = Core.Nothing, type' = Core.Nothing}

-- | The ICMP code. A value of -1 means all codes for the specified ICMP type.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itcCode :: Lens.Lens' IcmpTypeCode (Core.Maybe Core.Int)
itcCode = Lens.field @"code"
{-# INLINEABLE itcCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | The ICMP type. A value of -1 means all types.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itcType :: Lens.Lens' IcmpTypeCode (Core.Maybe Core.Int)
itcType = Lens.field @"type'"
{-# INLINEABLE itcType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.ToQuery IcmpTypeCode where
        toQuery IcmpTypeCode{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Code") code Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Type") type'

instance Core.FromXML IcmpTypeCode where
        parseXML x
          = IcmpTypeCode' Core.<$>
              (x Core..@? "code") Core.<*> x Core..@? "type"
