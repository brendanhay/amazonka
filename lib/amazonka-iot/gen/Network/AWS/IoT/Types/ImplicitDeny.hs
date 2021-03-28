{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ImplicitDeny
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.ImplicitDeny
  ( ImplicitDeny (..)
  -- * Smart constructor
  , mkImplicitDeny
  -- * Lenses
  , idPolicies
  ) where

import qualified Network.AWS.IoT.Types.Policy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information that implicitly denies authorization. When policy doesn't explicitly deny or allow an action on a resource it is considered an implicit deny.
--
-- /See:/ 'mkImplicitDeny' smart constructor.
newtype ImplicitDeny = ImplicitDeny'
  { policies :: Core.Maybe [Types.Policy]
    -- ^ Policies that don't contain a matching allow or deny statement for the specified action on the specified resource. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ImplicitDeny' value with any optional fields omitted.
mkImplicitDeny
    :: ImplicitDeny
mkImplicitDeny = ImplicitDeny'{policies = Core.Nothing}

-- | Policies that don't contain a matching allow or deny statement for the specified action on the specified resource. 
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idPolicies :: Lens.Lens' ImplicitDeny (Core.Maybe [Types.Policy])
idPolicies = Lens.field @"policies"
{-# INLINEABLE idPolicies #-}
{-# DEPRECATED policies "Use generic-lens or generic-optics with 'policies' instead"  #-}

instance Core.FromJSON ImplicitDeny where
        parseJSON
          = Core.withObject "ImplicitDeny" Core.$
              \ x -> ImplicitDeny' Core.<$> (x Core..:? "policies")
