{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.ListenerDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.ListenerDescription
  ( ListenerDescription (..),

    -- * Smart constructor
    mkListenerDescription,

    -- * Lenses
    ldListener,
    ldPolicyNames,
  )
where

import qualified Network.AWS.ELB.Internal as Types
import qualified Network.AWS.ELB.Types.Listener as Types
import qualified Network.AWS.ELB.Types.PolicyName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The policies enabled for a listener.
--
-- /See:/ 'mkListenerDescription' smart constructor.
data ListenerDescription = ListenerDescription'
  { -- | The listener.
    listener :: Core.Maybe Types.Listener,
    -- | The policies. If there are no policies enabled, the list is empty.
    policyNames :: Core.Maybe [Types.PolicyName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListenerDescription' value with any optional fields omitted.
mkListenerDescription ::
  ListenerDescription
mkListenerDescription =
  ListenerDescription'
    { listener = Core.Nothing,
      policyNames = Core.Nothing
    }

-- | The listener.
--
-- /Note:/ Consider using 'listener' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldListener :: Lens.Lens' ListenerDescription (Core.Maybe Types.Listener)
ldListener = Lens.field @"listener"
{-# DEPRECATED ldListener "Use generic-lens or generic-optics with 'listener' instead." #-}

-- | The policies. If there are no policies enabled, the list is empty.
--
-- /Note:/ Consider using 'policyNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldPolicyNames :: Lens.Lens' ListenerDescription (Core.Maybe [Types.PolicyName])
ldPolicyNames = Lens.field @"policyNames"
{-# DEPRECATED ldPolicyNames "Use generic-lens or generic-optics with 'policyNames' instead." #-}

instance Core.FromXML ListenerDescription where
  parseXML x =
    ListenerDescription'
      Core.<$> (x Core..@? "Listener")
      Core.<*> (x Core..@? "PolicyNames" Core..<@> Core.parseXMLList "member")
