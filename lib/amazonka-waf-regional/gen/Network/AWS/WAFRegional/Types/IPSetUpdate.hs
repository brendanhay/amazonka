{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.IPSetUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAFRegional.Types.IPSetUpdate
  ( IPSetUpdate (..)
  -- * Smart constructor
  , mkIPSetUpdate
  -- * Lenses
  , ipsuAction
  , ipsuIPSetDescriptor
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAFRegional.Types.ChangeAction as Types
import qualified Network.AWS.WAFRegional.Types.IPSetDescriptor as Types

-- | Specifies the type of update to perform to an 'IPSet' with 'UpdateIPSet' .
--
-- /See:/ 'mkIPSetUpdate' smart constructor.
data IPSetUpdate = IPSetUpdate'
  { action :: Types.ChangeAction
    -- ^ Specifies whether to insert or delete an IP address with 'UpdateIPSet' .
  , iPSetDescriptor :: Types.IPSetDescriptor
    -- ^ The IP address type (@IPV4@ or @IPV6@ ) and the IP address range (in CIDR notation) that web requests originate from.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IPSetUpdate' value with any optional fields omitted.
mkIPSetUpdate
    :: Types.ChangeAction -- ^ 'action'
    -> Types.IPSetDescriptor -- ^ 'iPSetDescriptor'
    -> IPSetUpdate
mkIPSetUpdate action iPSetDescriptor
  = IPSetUpdate'{action, iPSetDescriptor}

-- | Specifies whether to insert or delete an IP address with 'UpdateIPSet' .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsuAction :: Lens.Lens' IPSetUpdate Types.ChangeAction
ipsuAction = Lens.field @"action"
{-# INLINEABLE ipsuAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

-- | The IP address type (@IPV4@ or @IPV6@ ) and the IP address range (in CIDR notation) that web requests originate from.
--
-- /Note:/ Consider using 'iPSetDescriptor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsuIPSetDescriptor :: Lens.Lens' IPSetUpdate Types.IPSetDescriptor
ipsuIPSetDescriptor = Lens.field @"iPSetDescriptor"
{-# INLINEABLE ipsuIPSetDescriptor #-}
{-# DEPRECATED iPSetDescriptor "Use generic-lens or generic-optics with 'iPSetDescriptor' instead"  #-}

instance Core.FromJSON IPSetUpdate where
        toJSON IPSetUpdate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Action" Core..= action),
                  Core.Just ("IPSetDescriptor" Core..= iPSetDescriptor)])
