{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.GroupConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ResourceGroups.Types.GroupConfiguration
  ( GroupConfiguration (..)
  -- * Smart constructor
  , mkGroupConfiguration
  -- * Lenses
  , gcConfiguration
  , gcFailureReason
  , gcProposedConfiguration
  , gcStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ResourceGroups.Types.FailureReason as Types
import qualified Network.AWS.ResourceGroups.Types.GroupConfigurationItem as Types
import qualified Network.AWS.ResourceGroups.Types.GroupConfigurationStatus as Types

-- | A service configuration associated with a resource group. The configuration options are determined by the AWS service that defines the @Type@ , and specifies which resources can be included in the group. You can add a service configuration when you create the group.
--
-- /See:/ 'mkGroupConfiguration' smart constructor.
data GroupConfiguration = GroupConfiguration'
  { configuration :: Core.Maybe [Types.GroupConfigurationItem]
    -- ^ The configuration currently associated with the group and in effect.
  , failureReason :: Core.Maybe Types.FailureReason
    -- ^ If present, the reason why a request to update the group configuration failed.
  , proposedConfiguration :: Core.Maybe [Types.GroupConfigurationItem]
    -- ^ If present, the new configuration that is in the process of being applied to the group.
  , status :: Core.Maybe Types.GroupConfigurationStatus
    -- ^ The current status of an attempt to update the group configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GroupConfiguration' value with any optional fields omitted.
mkGroupConfiguration
    :: GroupConfiguration
mkGroupConfiguration
  = GroupConfiguration'{configuration = Core.Nothing,
                        failureReason = Core.Nothing, proposedConfiguration = Core.Nothing,
                        status = Core.Nothing}

-- | The configuration currently associated with the group and in effect.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcConfiguration :: Lens.Lens' GroupConfiguration (Core.Maybe [Types.GroupConfigurationItem])
gcConfiguration = Lens.field @"configuration"
{-# INLINEABLE gcConfiguration #-}
{-# DEPRECATED configuration "Use generic-lens or generic-optics with 'configuration' instead"  #-}

-- | If present, the reason why a request to update the group configuration failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcFailureReason :: Lens.Lens' GroupConfiguration (Core.Maybe Types.FailureReason)
gcFailureReason = Lens.field @"failureReason"
{-# INLINEABLE gcFailureReason #-}
{-# DEPRECATED failureReason "Use generic-lens or generic-optics with 'failureReason' instead"  #-}

-- | If present, the new configuration that is in the process of being applied to the group.
--
-- /Note:/ Consider using 'proposedConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcProposedConfiguration :: Lens.Lens' GroupConfiguration (Core.Maybe [Types.GroupConfigurationItem])
gcProposedConfiguration = Lens.field @"proposedConfiguration"
{-# INLINEABLE gcProposedConfiguration #-}
{-# DEPRECATED proposedConfiguration "Use generic-lens or generic-optics with 'proposedConfiguration' instead"  #-}

-- | The current status of an attempt to update the group configuration.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcStatus :: Lens.Lens' GroupConfiguration (Core.Maybe Types.GroupConfigurationStatus)
gcStatus = Lens.field @"status"
{-# INLINEABLE gcStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON GroupConfiguration where
        parseJSON
          = Core.withObject "GroupConfiguration" Core.$
              \ x ->
                GroupConfiguration' Core.<$>
                  (x Core..:? "Configuration") Core.<*> x Core..:? "FailureReason"
                    Core.<*> x Core..:? "ProposedConfiguration"
                    Core.<*> x Core..:? "Status"
