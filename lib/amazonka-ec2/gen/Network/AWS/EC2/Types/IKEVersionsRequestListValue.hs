{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IKEVersionsRequestListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.IKEVersionsRequestListValue
  ( IKEVersionsRequestListValue (..)
  -- * Smart constructor
  , mkIKEVersionsRequestListValue
  -- * Lenses
  , ikevrlvValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The IKE version that is permitted for the VPN tunnel.
--
-- /See:/ 'mkIKEVersionsRequestListValue' smart constructor.
newtype IKEVersionsRequestListValue = IKEVersionsRequestListValue'
  { value :: Core.Maybe Core.Text
    -- ^ The IKE version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'IKEVersionsRequestListValue' value with any optional fields omitted.
mkIKEVersionsRequestListValue
    :: IKEVersionsRequestListValue
mkIKEVersionsRequestListValue
  = IKEVersionsRequestListValue'{value = Core.Nothing}

-- | The IKE version.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikevrlvValue :: Lens.Lens' IKEVersionsRequestListValue (Core.Maybe Core.Text)
ikevrlvValue = Lens.field @"value"
{-# INLINEABLE ikevrlvValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.ToQuery IKEVersionsRequestListValue where
        toQuery IKEVersionsRequestListValue{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Value") value
