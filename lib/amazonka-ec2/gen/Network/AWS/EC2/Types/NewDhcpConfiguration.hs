{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NewDhcpConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.NewDhcpConfiguration
  ( NewDhcpConfiguration (..)
  -- * Smart constructor
  , mkNewDhcpConfiguration
  -- * Lenses
  , ndcKey
  , ndcValues
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /See:/ 'mkNewDhcpConfiguration' smart constructor.
data NewDhcpConfiguration = NewDhcpConfiguration'
  { key :: Core.Maybe Core.Text
  , values :: Core.Maybe [Core.Text]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NewDhcpConfiguration' value with any optional fields omitted.
mkNewDhcpConfiguration
    :: NewDhcpConfiguration
mkNewDhcpConfiguration
  = NewDhcpConfiguration'{key = Core.Nothing, values = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ndcKey :: Lens.Lens' NewDhcpConfiguration (Core.Maybe Core.Text)
ndcKey = Lens.field @"key"
{-# INLINEABLE ndcKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ndcValues :: Lens.Lens' NewDhcpConfiguration (Core.Maybe [Core.Text])
ndcValues = Lens.field @"values"
{-# INLINEABLE ndcValues #-}
{-# DEPRECATED values "Use generic-lens or generic-optics with 'values' instead"  #-}

instance Core.ToQuery NewDhcpConfiguration where
        toQuery NewDhcpConfiguration{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Key") key Core.<>
              Core.maybe Core.mempty (Core.toQueryList "Value") values
