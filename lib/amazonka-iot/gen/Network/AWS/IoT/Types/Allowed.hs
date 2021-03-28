{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Allowed
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.Allowed
  ( Allowed (..)
  -- * Smart constructor
  , mkAllowed
  -- * Lenses
  , aPolicies
  ) where

import qualified Network.AWS.IoT.Types.Policy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information that allowed the authorization.
--
-- /See:/ 'mkAllowed' smart constructor.
newtype Allowed = Allowed'
  { policies :: Core.Maybe [Types.Policy]
    -- ^ A list of policies that allowed the authentication.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Allowed' value with any optional fields omitted.
mkAllowed
    :: Allowed
mkAllowed = Allowed'{policies = Core.Nothing}

-- | A list of policies that allowed the authentication.
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPolicies :: Lens.Lens' Allowed (Core.Maybe [Types.Policy])
aPolicies = Lens.field @"policies"
{-# INLINEABLE aPolicies #-}
{-# DEPRECATED policies "Use generic-lens or generic-optics with 'policies' instead"  #-}

instance Core.FromJSON Allowed where
        parseJSON
          = Core.withObject "Allowed" Core.$
              \ x -> Allowed' Core.<$> (x Core..:? "policies")
