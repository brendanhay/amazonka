{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigurationAggregator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.ConfigurationAggregator
  ( ConfigurationAggregator (..)
  -- * Smart constructor
  , mkConfigurationAggregator
  -- * Lenses
  , caAccountAggregationSources
  , caConfigurationAggregatorArn
  , caConfigurationAggregatorName
  , caCreatedBy
  , caCreationTime
  , caLastUpdatedTime
  , caOrganizationAggregationSource
  ) where

import qualified Network.AWS.Config.Types.AccountAggregationSource as Types
import qualified Network.AWS.Config.Types.ConfigurationAggregatorArn as Types
import qualified Network.AWS.Config.Types.ConfigurationAggregatorName as Types
import qualified Network.AWS.Config.Types.OrganizationAggregationSource as Types
import qualified Network.AWS.Config.Types.StringWithCharLimit256 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The details about the configuration aggregator, including information about source accounts, regions, and metadata of the aggregator. 
--
-- /See:/ 'mkConfigurationAggregator' smart constructor.
data ConfigurationAggregator = ConfigurationAggregator'
  { accountAggregationSources :: Core.Maybe [Types.AccountAggregationSource]
    -- ^ Provides a list of source accounts and regions to be aggregated.
  , configurationAggregatorArn :: Core.Maybe Types.ConfigurationAggregatorArn
    -- ^ The Amazon Resource Name (ARN) of the aggregator.
  , configurationAggregatorName :: Core.Maybe Types.ConfigurationAggregatorName
    -- ^ The name of the aggregator.
  , createdBy :: Core.Maybe Types.StringWithCharLimit256
    -- ^ AWS service that created the configuration aggregator.
  , creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time stamp when the configuration aggregator was created.
  , lastUpdatedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time of the last update.
  , organizationAggregationSource :: Core.Maybe Types.OrganizationAggregationSource
    -- ^ Provides an organization and list of regions to be aggregated.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ConfigurationAggregator' value with any optional fields omitted.
mkConfigurationAggregator
    :: ConfigurationAggregator
mkConfigurationAggregator
  = ConfigurationAggregator'{accountAggregationSources =
                               Core.Nothing,
                             configurationAggregatorArn = Core.Nothing,
                             configurationAggregatorName = Core.Nothing,
                             createdBy = Core.Nothing, creationTime = Core.Nothing,
                             lastUpdatedTime = Core.Nothing,
                             organizationAggregationSource = Core.Nothing}

-- | Provides a list of source accounts and regions to be aggregated.
--
-- /Note:/ Consider using 'accountAggregationSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAccountAggregationSources :: Lens.Lens' ConfigurationAggregator (Core.Maybe [Types.AccountAggregationSource])
caAccountAggregationSources = Lens.field @"accountAggregationSources"
{-# INLINEABLE caAccountAggregationSources #-}
{-# DEPRECATED accountAggregationSources "Use generic-lens or generic-optics with 'accountAggregationSources' instead"  #-}

-- | The Amazon Resource Name (ARN) of the aggregator.
--
-- /Note:/ Consider using 'configurationAggregatorArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caConfigurationAggregatorArn :: Lens.Lens' ConfigurationAggregator (Core.Maybe Types.ConfigurationAggregatorArn)
caConfigurationAggregatorArn = Lens.field @"configurationAggregatorArn"
{-# INLINEABLE caConfigurationAggregatorArn #-}
{-# DEPRECATED configurationAggregatorArn "Use generic-lens or generic-optics with 'configurationAggregatorArn' instead"  #-}

-- | The name of the aggregator.
--
-- /Note:/ Consider using 'configurationAggregatorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caConfigurationAggregatorName :: Lens.Lens' ConfigurationAggregator (Core.Maybe Types.ConfigurationAggregatorName)
caConfigurationAggregatorName = Lens.field @"configurationAggregatorName"
{-# INLINEABLE caConfigurationAggregatorName #-}
{-# DEPRECATED configurationAggregatorName "Use generic-lens or generic-optics with 'configurationAggregatorName' instead"  #-}

-- | AWS service that created the configuration aggregator.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caCreatedBy :: Lens.Lens' ConfigurationAggregator (Core.Maybe Types.StringWithCharLimit256)
caCreatedBy = Lens.field @"createdBy"
{-# INLINEABLE caCreatedBy #-}
{-# DEPRECATED createdBy "Use generic-lens or generic-optics with 'createdBy' instead"  #-}

-- | The time stamp when the configuration aggregator was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caCreationTime :: Lens.Lens' ConfigurationAggregator (Core.Maybe Core.NominalDiffTime)
caCreationTime = Lens.field @"creationTime"
{-# INLINEABLE caCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The time of the last update.
--
-- /Note:/ Consider using 'lastUpdatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caLastUpdatedTime :: Lens.Lens' ConfigurationAggregator (Core.Maybe Core.NominalDiffTime)
caLastUpdatedTime = Lens.field @"lastUpdatedTime"
{-# INLINEABLE caLastUpdatedTime #-}
{-# DEPRECATED lastUpdatedTime "Use generic-lens or generic-optics with 'lastUpdatedTime' instead"  #-}

-- | Provides an organization and list of regions to be aggregated.
--
-- /Note:/ Consider using 'organizationAggregationSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caOrganizationAggregationSource :: Lens.Lens' ConfigurationAggregator (Core.Maybe Types.OrganizationAggregationSource)
caOrganizationAggregationSource = Lens.field @"organizationAggregationSource"
{-# INLINEABLE caOrganizationAggregationSource #-}
{-# DEPRECATED organizationAggregationSource "Use generic-lens or generic-optics with 'organizationAggregationSource' instead"  #-}

instance Core.FromJSON ConfigurationAggregator where
        parseJSON
          = Core.withObject "ConfigurationAggregator" Core.$
              \ x ->
                ConfigurationAggregator' Core.<$>
                  (x Core..:? "AccountAggregationSources") Core.<*>
                    x Core..:? "ConfigurationAggregatorArn"
                    Core.<*> x Core..:? "ConfigurationAggregatorName"
                    Core.<*> x Core..:? "CreatedBy"
                    Core.<*> x Core..:? "CreationTime"
                    Core.<*> x Core..:? "LastUpdatedTime"
                    Core.<*> x Core..:? "OrganizationAggregationSource"
