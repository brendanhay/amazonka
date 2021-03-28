{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IKEVersionsListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.IKEVersionsListValue
  ( IKEVersionsListValue (..)
  -- * Smart constructor
  , mkIKEVersionsListValue
  -- * Lenses
  , ikevlvValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The internet key exchange (IKE) version permitted for the VPN tunnel.
--
-- /See:/ 'mkIKEVersionsListValue' smart constructor.
newtype IKEVersionsListValue = IKEVersionsListValue'
  { value :: Core.Maybe Core.Text
    -- ^ The IKE version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'IKEVersionsListValue' value with any optional fields omitted.
mkIKEVersionsListValue
    :: IKEVersionsListValue
mkIKEVersionsListValue
  = IKEVersionsListValue'{value = Core.Nothing}

-- | The IKE version.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikevlvValue :: Lens.Lens' IKEVersionsListValue (Core.Maybe Core.Text)
ikevlvValue = Lens.field @"value"
{-# INLINEABLE ikevlvValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromXML IKEVersionsListValue where
        parseXML x = IKEVersionsListValue' Core.<$> (x Core..@? "value")
