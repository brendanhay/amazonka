{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputWhitelistRuleCidr
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputWhitelistRuleCidr
  ( InputWhitelistRuleCidr (..),

    -- * Smart constructor
    mkInputWhitelistRuleCidr,

    -- * Lenses
    iwrcCidr,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An IPv4 CIDR to whitelist.
--
-- /See:/ 'mkInputWhitelistRuleCidr' smart constructor.
newtype InputWhitelistRuleCidr = InputWhitelistRuleCidr'
  { -- | The IPv4 CIDR to whitelist.
    cidr :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'InputWhitelistRuleCidr' value with any optional fields omitted.
mkInputWhitelistRuleCidr ::
  InputWhitelistRuleCidr
mkInputWhitelistRuleCidr =
  InputWhitelistRuleCidr' {cidr = Core.Nothing}

-- | The IPv4 CIDR to whitelist.
--
-- /Note:/ Consider using 'cidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iwrcCidr :: Lens.Lens' InputWhitelistRuleCidr (Core.Maybe Core.Text)
iwrcCidr = Lens.field @"cidr"
{-# DEPRECATED iwrcCidr "Use generic-lens or generic-optics with 'cidr' instead." #-}

instance Core.FromJSON InputWhitelistRuleCidr where
  toJSON InputWhitelistRuleCidr {..} =
    Core.object (Core.catMaybes [("cidr" Core..=) Core.<$> cidr])
