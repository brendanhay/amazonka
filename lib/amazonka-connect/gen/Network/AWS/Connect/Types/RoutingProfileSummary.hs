{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.RoutingProfileSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.RoutingProfileSummary
  ( RoutingProfileSummary (..)
  -- * Smart constructor
  , mkRoutingProfileSummary
  -- * Lenses
  , rpsArn
  , rpsId
  , rpsName
  ) where

import qualified Network.AWS.Connect.Types.ARN as Types
import qualified Network.AWS.Connect.Types.RoutingProfileId as Types
import qualified Network.AWS.Connect.Types.RoutingProfileName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains summary information about a routing profile.
--
-- /See:/ 'mkRoutingProfileSummary' smart constructor.
data RoutingProfileSummary = RoutingProfileSummary'
  { arn :: Core.Maybe Types.ARN
    -- ^ The Amazon Resource Name (ARN) of the routing profile.
  , id :: Core.Maybe Types.RoutingProfileId
    -- ^ The identifier of the routing profile.
  , name :: Core.Maybe Types.RoutingProfileName
    -- ^ The name of the routing profile.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RoutingProfileSummary' value with any optional fields omitted.
mkRoutingProfileSummary
    :: RoutingProfileSummary
mkRoutingProfileSummary
  = RoutingProfileSummary'{arn = Core.Nothing, id = Core.Nothing,
                           name = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the routing profile.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpsArn :: Lens.Lens' RoutingProfileSummary (Core.Maybe Types.ARN)
rpsArn = Lens.field @"arn"
{-# INLINEABLE rpsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The identifier of the routing profile.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpsId :: Lens.Lens' RoutingProfileSummary (Core.Maybe Types.RoutingProfileId)
rpsId = Lens.field @"id"
{-# INLINEABLE rpsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The name of the routing profile.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpsName :: Lens.Lens' RoutingProfileSummary (Core.Maybe Types.RoutingProfileName)
rpsName = Lens.field @"name"
{-# INLINEABLE rpsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON RoutingProfileSummary where
        parseJSON
          = Core.withObject "RoutingProfileSummary" Core.$
              \ x ->
                RoutingProfileSummary' Core.<$>
                  (x Core..:? "Arn") Core.<*> x Core..:? "Id" Core.<*>
                    x Core..:? "Name"
