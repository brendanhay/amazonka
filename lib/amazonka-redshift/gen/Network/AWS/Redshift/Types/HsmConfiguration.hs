{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.HsmConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.HsmConfiguration
  ( HsmConfiguration (..)
  -- * Smart constructor
  , mkHsmConfiguration
  -- * Lenses
  , hcDescription
  , hcHsmConfigurationIdentifier
  , hcHsmIpAddress
  , hcHsmPartitionName
  , hcTags
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.Tag as Types

-- | Returns information about an HSM configuration, which is an object that describes to Amazon Redshift clusters the information they require to connect to an HSM where they can store database encryption keys.
--
-- /See:/ 'mkHsmConfiguration' smart constructor.
data HsmConfiguration = HsmConfiguration'
  { description :: Core.Maybe Core.Text
    -- ^ A text description of the HSM configuration.
  , hsmConfigurationIdentifier :: Core.Maybe Core.Text
    -- ^ The name of the Amazon Redshift HSM configuration.
  , hsmIpAddress :: Core.Maybe Core.Text
    -- ^ The IP address that the Amazon Redshift cluster must use to access the HSM.
  , hsmPartitionName :: Core.Maybe Core.Text
    -- ^ The name of the partition in the HSM where the Amazon Redshift clusters will store their database encryption keys.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The list of tags for the HSM configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HsmConfiguration' value with any optional fields omitted.
mkHsmConfiguration
    :: HsmConfiguration
mkHsmConfiguration
  = HsmConfiguration'{description = Core.Nothing,
                      hsmConfigurationIdentifier = Core.Nothing,
                      hsmIpAddress = Core.Nothing, hsmPartitionName = Core.Nothing,
                      tags = Core.Nothing}

-- | A text description of the HSM configuration.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcDescription :: Lens.Lens' HsmConfiguration (Core.Maybe Core.Text)
hcDescription = Lens.field @"description"
{-# INLINEABLE hcDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The name of the Amazon Redshift HSM configuration.
--
-- /Note:/ Consider using 'hsmConfigurationIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcHsmConfigurationIdentifier :: Lens.Lens' HsmConfiguration (Core.Maybe Core.Text)
hcHsmConfigurationIdentifier = Lens.field @"hsmConfigurationIdentifier"
{-# INLINEABLE hcHsmConfigurationIdentifier #-}
{-# DEPRECATED hsmConfigurationIdentifier "Use generic-lens or generic-optics with 'hsmConfigurationIdentifier' instead"  #-}

-- | The IP address that the Amazon Redshift cluster must use to access the HSM.
--
-- /Note:/ Consider using 'hsmIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcHsmIpAddress :: Lens.Lens' HsmConfiguration (Core.Maybe Core.Text)
hcHsmIpAddress = Lens.field @"hsmIpAddress"
{-# INLINEABLE hcHsmIpAddress #-}
{-# DEPRECATED hsmIpAddress "Use generic-lens or generic-optics with 'hsmIpAddress' instead"  #-}

-- | The name of the partition in the HSM where the Amazon Redshift clusters will store their database encryption keys.
--
-- /Note:/ Consider using 'hsmPartitionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcHsmPartitionName :: Lens.Lens' HsmConfiguration (Core.Maybe Core.Text)
hcHsmPartitionName = Lens.field @"hsmPartitionName"
{-# INLINEABLE hcHsmPartitionName #-}
{-# DEPRECATED hsmPartitionName "Use generic-lens or generic-optics with 'hsmPartitionName' instead"  #-}

-- | The list of tags for the HSM configuration.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcTags :: Lens.Lens' HsmConfiguration (Core.Maybe [Types.Tag])
hcTags = Lens.field @"tags"
{-# INLINEABLE hcTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromXML HsmConfiguration where
        parseXML x
          = HsmConfiguration' Core.<$>
              (x Core..@? "Description") Core.<*>
                x Core..@? "HsmConfigurationIdentifier"
                Core.<*> x Core..@? "HsmIpAddress"
                Core.<*> x Core..@? "HsmPartitionName"
                Core.<*> x Core..@? "Tags" Core..<@> Core.parseXMLList "Tag"
