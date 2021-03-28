{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.AutomaticTapeCreationPolicyInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StorageGateway.Types.AutomaticTapeCreationPolicyInfo
  ( AutomaticTapeCreationPolicyInfo (..)
  -- * Smart constructor
  , mkAutomaticTapeCreationPolicyInfo
  -- * Lenses
  , atcpiAutomaticTapeCreationRules
  , atcpiGatewayARN
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StorageGateway.Types.AutomaticTapeCreationRule as Types
import qualified Network.AWS.StorageGateway.Types.GatewayARN as Types

-- | Information about the gateway's automatic tape creation policies, including the automatic tape creation rules and the gateway that is using the policies.
--
-- /See:/ 'mkAutomaticTapeCreationPolicyInfo' smart constructor.
data AutomaticTapeCreationPolicyInfo = AutomaticTapeCreationPolicyInfo'
  { automaticTapeCreationRules :: Core.Maybe (Core.NonEmpty Types.AutomaticTapeCreationRule)
    -- ^ An automatic tape creation policy consists of a list of automatic tape creation rules. This returns the rules that determine when and how to automatically create new tapes.
  , gatewayARN :: Core.Maybe Types.GatewayARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AutomaticTapeCreationPolicyInfo' value with any optional fields omitted.
mkAutomaticTapeCreationPolicyInfo
    :: AutomaticTapeCreationPolicyInfo
mkAutomaticTapeCreationPolicyInfo
  = AutomaticTapeCreationPolicyInfo'{automaticTapeCreationRules =
                                       Core.Nothing,
                                     gatewayARN = Core.Nothing}

-- | An automatic tape creation policy consists of a list of automatic tape creation rules. This returns the rules that determine when and how to automatically create new tapes.
--
-- /Note:/ Consider using 'automaticTapeCreationRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcpiAutomaticTapeCreationRules :: Lens.Lens' AutomaticTapeCreationPolicyInfo (Core.Maybe (Core.NonEmpty Types.AutomaticTapeCreationRule))
atcpiAutomaticTapeCreationRules = Lens.field @"automaticTapeCreationRules"
{-# INLINEABLE atcpiAutomaticTapeCreationRules #-}
{-# DEPRECATED automaticTapeCreationRules "Use generic-lens or generic-optics with 'automaticTapeCreationRules' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcpiGatewayARN :: Lens.Lens' AutomaticTapeCreationPolicyInfo (Core.Maybe Types.GatewayARN)
atcpiGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE atcpiGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

instance Core.FromJSON AutomaticTapeCreationPolicyInfo where
        parseJSON
          = Core.withObject "AutomaticTapeCreationPolicyInfo" Core.$
              \ x ->
                AutomaticTapeCreationPolicyInfo' Core.<$>
                  (x Core..:? "AutomaticTapeCreationRules") Core.<*>
                    x Core..:? "GatewayARN"
