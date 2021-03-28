{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ExplicitDeny
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.ExplicitDeny
  ( ExplicitDeny (..)
  -- * Smart constructor
  , mkExplicitDeny
  -- * Lenses
  , edPolicies
  ) where

import qualified Network.AWS.IoT.Types.Policy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information that explicitly denies authorization.
--
-- /See:/ 'mkExplicitDeny' smart constructor.
newtype ExplicitDeny = ExplicitDeny'
  { policies :: Core.Maybe [Types.Policy]
    -- ^ The policies that denied the authorization.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ExplicitDeny' value with any optional fields omitted.
mkExplicitDeny
    :: ExplicitDeny
mkExplicitDeny = ExplicitDeny'{policies = Core.Nothing}

-- | The policies that denied the authorization.
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edPolicies :: Lens.Lens' ExplicitDeny (Core.Maybe [Types.Policy])
edPolicies = Lens.field @"policies"
{-# INLINEABLE edPolicies #-}
{-# DEPRECATED policies "Use generic-lens or generic-optics with 'policies' instead"  #-}

instance Core.FromJSON ExplicitDeny where
        parseJSON
          = Core.withObject "ExplicitDeny" Core.$
              \ x -> ExplicitDeny' Core.<$> (x Core..:? "policies")
