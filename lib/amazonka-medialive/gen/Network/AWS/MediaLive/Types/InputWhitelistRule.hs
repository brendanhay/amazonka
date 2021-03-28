{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputWhitelistRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.InputWhitelistRule
  ( InputWhitelistRule (..)
  -- * Smart constructor
  , mkInputWhitelistRule
  -- * Lenses
  , iwrCidr
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Whitelist rule
--
-- /See:/ 'mkInputWhitelistRule' smart constructor.
newtype InputWhitelistRule = InputWhitelistRule'
  { cidr :: Core.Maybe Core.Text
    -- ^ The IPv4 CIDR that's whitelisted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'InputWhitelistRule' value with any optional fields omitted.
mkInputWhitelistRule
    :: InputWhitelistRule
mkInputWhitelistRule = InputWhitelistRule'{cidr = Core.Nothing}

-- | The IPv4 CIDR that's whitelisted.
--
-- /Note:/ Consider using 'cidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iwrCidr :: Lens.Lens' InputWhitelistRule (Core.Maybe Core.Text)
iwrCidr = Lens.field @"cidr"
{-# INLINEABLE iwrCidr #-}
{-# DEPRECATED cidr "Use generic-lens or generic-optics with 'cidr' instead"  #-}

instance Core.FromJSON InputWhitelistRule where
        parseJSON
          = Core.withObject "InputWhitelistRule" Core.$
              \ x -> InputWhitelistRule' Core.<$> (x Core..:? "cidr")
