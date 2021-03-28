{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.AliasRoutingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types.AliasRoutingConfiguration
  ( AliasRoutingConfiguration (..)
  -- * Smart constructor
  , mkAliasRoutingConfiguration
  -- * Lenses
  , arcAdditionalVersionWeights
  ) where

import qualified Network.AWS.Lambda.Types.AdditionalVersion as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The <https://docs.aws.amazon.com/lambda/latest/dg/lambda-traffic-shifting-using-aliases.html traffic-shifting> configuration of a Lambda function alias.
--
-- /See:/ 'mkAliasRoutingConfiguration' smart constructor.
newtype AliasRoutingConfiguration = AliasRoutingConfiguration'
  { additionalVersionWeights :: Core.Maybe (Core.HashMap Types.AdditionalVersion Core.Double)
    -- ^ The second version, and the percentage of traffic that's routed to it.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AliasRoutingConfiguration' value with any optional fields omitted.
mkAliasRoutingConfiguration
    :: AliasRoutingConfiguration
mkAliasRoutingConfiguration
  = AliasRoutingConfiguration'{additionalVersionWeights =
                                 Core.Nothing}

-- | The second version, and the percentage of traffic that's routed to it.
--
-- /Note:/ Consider using 'additionalVersionWeights' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arcAdditionalVersionWeights :: Lens.Lens' AliasRoutingConfiguration (Core.Maybe (Core.HashMap Types.AdditionalVersion Core.Double))
arcAdditionalVersionWeights = Lens.field @"additionalVersionWeights"
{-# INLINEABLE arcAdditionalVersionWeights #-}
{-# DEPRECATED additionalVersionWeights "Use generic-lens or generic-optics with 'additionalVersionWeights' instead"  #-}

instance Core.FromJSON AliasRoutingConfiguration where
        toJSON AliasRoutingConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("AdditionalVersionWeights" Core..=) Core.<$>
                    additionalVersionWeights])

instance Core.FromJSON AliasRoutingConfiguration where
        parseJSON
          = Core.withObject "AliasRoutingConfiguration" Core.$
              \ x ->
                AliasRoutingConfiguration' Core.<$>
                  (x Core..:? "AdditionalVersionWeights")
