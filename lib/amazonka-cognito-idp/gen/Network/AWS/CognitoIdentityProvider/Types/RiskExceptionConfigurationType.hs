{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.RiskExceptionConfigurationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.RiskExceptionConfigurationType
  ( RiskExceptionConfigurationType (..)
  -- * Smart constructor
  , mkRiskExceptionConfigurationType
  -- * Lenses
  , rectBlockedIPRangeList
  , rectSkippedIPRangeList
  ) where

import qualified Network.AWS.CognitoIdentityProvider.Types.StringType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The type of the configuration to override the risk decision.
--
-- /See:/ 'mkRiskExceptionConfigurationType' smart constructor.
data RiskExceptionConfigurationType = RiskExceptionConfigurationType'
  { blockedIPRangeList :: Core.Maybe [Types.StringType]
    -- ^ Overrides the risk decision to always block the pre-authentication requests. The IP range is in CIDR notation: a compact representation of an IP address and its associated routing prefix.
  , skippedIPRangeList :: Core.Maybe [Types.StringType]
    -- ^ Risk detection is not performed on the IP addresses in the range list. The IP range is in CIDR notation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RiskExceptionConfigurationType' value with any optional fields omitted.
mkRiskExceptionConfigurationType
    :: RiskExceptionConfigurationType
mkRiskExceptionConfigurationType
  = RiskExceptionConfigurationType'{blockedIPRangeList =
                                      Core.Nothing,
                                    skippedIPRangeList = Core.Nothing}

-- | Overrides the risk decision to always block the pre-authentication requests. The IP range is in CIDR notation: a compact representation of an IP address and its associated routing prefix.
--
-- /Note:/ Consider using 'blockedIPRangeList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rectBlockedIPRangeList :: Lens.Lens' RiskExceptionConfigurationType (Core.Maybe [Types.StringType])
rectBlockedIPRangeList = Lens.field @"blockedIPRangeList"
{-# INLINEABLE rectBlockedIPRangeList #-}
{-# DEPRECATED blockedIPRangeList "Use generic-lens or generic-optics with 'blockedIPRangeList' instead"  #-}

-- | Risk detection is not performed on the IP addresses in the range list. The IP range is in CIDR notation.
--
-- /Note:/ Consider using 'skippedIPRangeList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rectSkippedIPRangeList :: Lens.Lens' RiskExceptionConfigurationType (Core.Maybe [Types.StringType])
rectSkippedIPRangeList = Lens.field @"skippedIPRangeList"
{-# INLINEABLE rectSkippedIPRangeList #-}
{-# DEPRECATED skippedIPRangeList "Use generic-lens or generic-optics with 'skippedIPRangeList' instead"  #-}

instance Core.FromJSON RiskExceptionConfigurationType where
        toJSON RiskExceptionConfigurationType{..}
          = Core.object
              (Core.catMaybes
                 [("BlockedIPRangeList" Core..=) Core.<$> blockedIPRangeList,
                  ("SkippedIPRangeList" Core..=) Core.<$> skippedIPRangeList])

instance Core.FromJSON RiskExceptionConfigurationType where
        parseJSON
          = Core.withObject "RiskExceptionConfigurationType" Core.$
              \ x ->
                RiskExceptionConfigurationType' Core.<$>
                  (x Core..:? "BlockedIPRangeList") Core.<*>
                    x Core..:? "SkippedIPRangeList"
