{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CidrBlock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.CidrBlock
  ( CidrBlock (..)
  -- * Smart constructor
  , mkCidrBlock
  -- * Lenses
  , cbCidrBlock
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an IPv4 CIDR block.
--
-- /See:/ 'mkCidrBlock' smart constructor.
newtype CidrBlock = CidrBlock'
  { cidrBlock :: Core.Maybe Core.Text
    -- ^ The IPv4 CIDR block.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CidrBlock' value with any optional fields omitted.
mkCidrBlock
    :: CidrBlock
mkCidrBlock = CidrBlock'{cidrBlock = Core.Nothing}

-- | The IPv4 CIDR block.
--
-- /Note:/ Consider using 'cidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbCidrBlock :: Lens.Lens' CidrBlock (Core.Maybe Core.Text)
cbCidrBlock = Lens.field @"cidrBlock"
{-# INLINEABLE cbCidrBlock #-}
{-# DEPRECATED cidrBlock "Use generic-lens or generic-optics with 'cidrBlock' instead"  #-}

instance Core.FromXML CidrBlock where
        parseXML x = CidrBlock' Core.<$> (x Core..@? "cidrBlock")
