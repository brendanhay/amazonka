-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigurationAggregator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigurationAggregator
  ( ConfigurationAggregator (..),

    -- * Smart constructor
    mkConfigurationAggregator,

    -- * Lenses
    caConfigurationAggregatorARN,
    caCreationTime,
    caOrganizationAggregationSource,
    caLastUpdatedTime,
    caAccountAggregationSources,
    caCreatedBy,
    caConfigurationAggregatorName,
  )
where

import Network.AWS.Config.Types.AccountAggregationSource
import Network.AWS.Config.Types.OrganizationAggregationSource
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The details about the configuration aggregator, including information about source accounts, regions, and metadata of the aggregator.
--
-- /See:/ 'mkConfigurationAggregator' smart constructor.
data ConfigurationAggregator = ConfigurationAggregator'
  { configurationAggregatorARN ::
      Lude.Maybe Lude.Text,
    creationTime :: Lude.Maybe Lude.Timestamp,
    organizationAggregationSource ::
      Lude.Maybe OrganizationAggregationSource,
    lastUpdatedTime ::
      Lude.Maybe Lude.Timestamp,
    accountAggregationSources ::
      Lude.Maybe [AccountAggregationSource],
    createdBy :: Lude.Maybe Lude.Text,
    configurationAggregatorName ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConfigurationAggregator' with the minimum fields required to make a request.
--
-- * 'accountAggregationSources' - Provides a list of source accounts and regions to be aggregated.
-- * 'configurationAggregatorARN' - The Amazon Resource Name (ARN) of the aggregator.
-- * 'configurationAggregatorName' - The name of the aggregator.
-- * 'createdBy' - AWS service that created the configuration aggregator.
-- * 'creationTime' - The time stamp when the configuration aggregator was created.
-- * 'lastUpdatedTime' - The time of the last update.
-- * 'organizationAggregationSource' - Provides an organization and list of regions to be aggregated.
mkConfigurationAggregator ::
  ConfigurationAggregator
mkConfigurationAggregator =
  ConfigurationAggregator'
    { configurationAggregatorARN =
        Lude.Nothing,
      creationTime = Lude.Nothing,
      organizationAggregationSource = Lude.Nothing,
      lastUpdatedTime = Lude.Nothing,
      accountAggregationSources = Lude.Nothing,
      createdBy = Lude.Nothing,
      configurationAggregatorName = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the aggregator.
--
-- /Note:/ Consider using 'configurationAggregatorARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caConfigurationAggregatorARN :: Lens.Lens' ConfigurationAggregator (Lude.Maybe Lude.Text)
caConfigurationAggregatorARN = Lens.lens (configurationAggregatorARN :: ConfigurationAggregator -> Lude.Maybe Lude.Text) (\s a -> s {configurationAggregatorARN = a} :: ConfigurationAggregator)
{-# DEPRECATED caConfigurationAggregatorARN "Use generic-lens or generic-optics with 'configurationAggregatorARN' instead." #-}

-- | The time stamp when the configuration aggregator was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caCreationTime :: Lens.Lens' ConfigurationAggregator (Lude.Maybe Lude.Timestamp)
caCreationTime = Lens.lens (creationTime :: ConfigurationAggregator -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: ConfigurationAggregator)
{-# DEPRECATED caCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Provides an organization and list of regions to be aggregated.
--
-- /Note:/ Consider using 'organizationAggregationSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caOrganizationAggregationSource :: Lens.Lens' ConfigurationAggregator (Lude.Maybe OrganizationAggregationSource)
caOrganizationAggregationSource = Lens.lens (organizationAggregationSource :: ConfigurationAggregator -> Lude.Maybe OrganizationAggregationSource) (\s a -> s {organizationAggregationSource = a} :: ConfigurationAggregator)
{-# DEPRECATED caOrganizationAggregationSource "Use generic-lens or generic-optics with 'organizationAggregationSource' instead." #-}

-- | The time of the last update.
--
-- /Note:/ Consider using 'lastUpdatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caLastUpdatedTime :: Lens.Lens' ConfigurationAggregator (Lude.Maybe Lude.Timestamp)
caLastUpdatedTime = Lens.lens (lastUpdatedTime :: ConfigurationAggregator -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedTime = a} :: ConfigurationAggregator)
{-# DEPRECATED caLastUpdatedTime "Use generic-lens or generic-optics with 'lastUpdatedTime' instead." #-}

-- | Provides a list of source accounts and regions to be aggregated.
--
-- /Note:/ Consider using 'accountAggregationSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAccountAggregationSources :: Lens.Lens' ConfigurationAggregator (Lude.Maybe [AccountAggregationSource])
caAccountAggregationSources = Lens.lens (accountAggregationSources :: ConfigurationAggregator -> Lude.Maybe [AccountAggregationSource]) (\s a -> s {accountAggregationSources = a} :: ConfigurationAggregator)
{-# DEPRECATED caAccountAggregationSources "Use generic-lens or generic-optics with 'accountAggregationSources' instead." #-}

-- | AWS service that created the configuration aggregator.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caCreatedBy :: Lens.Lens' ConfigurationAggregator (Lude.Maybe Lude.Text)
caCreatedBy = Lens.lens (createdBy :: ConfigurationAggregator -> Lude.Maybe Lude.Text) (\s a -> s {createdBy = a} :: ConfigurationAggregator)
{-# DEPRECATED caCreatedBy "Use generic-lens or generic-optics with 'createdBy' instead." #-}

-- | The name of the aggregator.
--
-- /Note:/ Consider using 'configurationAggregatorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caConfigurationAggregatorName :: Lens.Lens' ConfigurationAggregator (Lude.Maybe Lude.Text)
caConfigurationAggregatorName = Lens.lens (configurationAggregatorName :: ConfigurationAggregator -> Lude.Maybe Lude.Text) (\s a -> s {configurationAggregatorName = a} :: ConfigurationAggregator)
{-# DEPRECATED caConfigurationAggregatorName "Use generic-lens or generic-optics with 'configurationAggregatorName' instead." #-}

instance Lude.FromJSON ConfigurationAggregator where
  parseJSON =
    Lude.withObject
      "ConfigurationAggregator"
      ( \x ->
          ConfigurationAggregator'
            Lude.<$> (x Lude..:? "ConfigurationAggregatorArn")
            Lude.<*> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "OrganizationAggregationSource")
            Lude.<*> (x Lude..:? "LastUpdatedTime")
            Lude.<*> (x Lude..:? "AccountAggregationSources" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "CreatedBy")
            Lude.<*> (x Lude..:? "ConfigurationAggregatorName")
      )
