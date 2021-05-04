{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigurationAggregator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigurationAggregator where

import Network.AWS.Config.Types.AccountAggregationSource
import Network.AWS.Config.Types.OrganizationAggregationSource
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The details about the configuration aggregator, including information
-- about source accounts, regions, and metadata of the aggregator.
--
-- /See:/ 'newConfigurationAggregator' smart constructor.
data ConfigurationAggregator = ConfigurationAggregator'
  { -- | The time stamp when the configuration aggregator was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The Amazon Resource Name (ARN) of the aggregator.
    configurationAggregatorArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the aggregator.
    configurationAggregatorName :: Prelude.Maybe Prelude.Text,
    -- | Provides a list of source accounts and regions to be aggregated.
    accountAggregationSources :: Prelude.Maybe [AccountAggregationSource],
    -- | AWS service that created the configuration aggregator.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The time of the last update.
    lastUpdatedTime :: Prelude.Maybe Prelude.POSIX,
    -- | Provides an organization and list of regions to be aggregated.
    organizationAggregationSource :: Prelude.Maybe OrganizationAggregationSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ConfigurationAggregator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'configurationAggregator_creationTime' - The time stamp when the configuration aggregator was created.
--
-- 'configurationAggregatorArn', 'configurationAggregator_configurationAggregatorArn' - The Amazon Resource Name (ARN) of the aggregator.
--
-- 'configurationAggregatorName', 'configurationAggregator_configurationAggregatorName' - The name of the aggregator.
--
-- 'accountAggregationSources', 'configurationAggregator_accountAggregationSources' - Provides a list of source accounts and regions to be aggregated.
--
-- 'createdBy', 'configurationAggregator_createdBy' - AWS service that created the configuration aggregator.
--
-- 'lastUpdatedTime', 'configurationAggregator_lastUpdatedTime' - The time of the last update.
--
-- 'organizationAggregationSource', 'configurationAggregator_organizationAggregationSource' - Provides an organization and list of regions to be aggregated.
newConfigurationAggregator ::
  ConfigurationAggregator
newConfigurationAggregator =
  ConfigurationAggregator'
    { creationTime =
        Prelude.Nothing,
      configurationAggregatorArn = Prelude.Nothing,
      configurationAggregatorName = Prelude.Nothing,
      accountAggregationSources = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      organizationAggregationSource = Prelude.Nothing
    }

-- | The time stamp when the configuration aggregator was created.
configurationAggregator_creationTime :: Lens.Lens' ConfigurationAggregator (Prelude.Maybe Prelude.UTCTime)
configurationAggregator_creationTime = Lens.lens (\ConfigurationAggregator' {creationTime} -> creationTime) (\s@ConfigurationAggregator' {} a -> s {creationTime = a} :: ConfigurationAggregator) Prelude.. Lens.mapping Prelude._Time

-- | The Amazon Resource Name (ARN) of the aggregator.
configurationAggregator_configurationAggregatorArn :: Lens.Lens' ConfigurationAggregator (Prelude.Maybe Prelude.Text)
configurationAggregator_configurationAggregatorArn = Lens.lens (\ConfigurationAggregator' {configurationAggregatorArn} -> configurationAggregatorArn) (\s@ConfigurationAggregator' {} a -> s {configurationAggregatorArn = a} :: ConfigurationAggregator)

-- | The name of the aggregator.
configurationAggregator_configurationAggregatorName :: Lens.Lens' ConfigurationAggregator (Prelude.Maybe Prelude.Text)
configurationAggregator_configurationAggregatorName = Lens.lens (\ConfigurationAggregator' {configurationAggregatorName} -> configurationAggregatorName) (\s@ConfigurationAggregator' {} a -> s {configurationAggregatorName = a} :: ConfigurationAggregator)

-- | Provides a list of source accounts and regions to be aggregated.
configurationAggregator_accountAggregationSources :: Lens.Lens' ConfigurationAggregator (Prelude.Maybe [AccountAggregationSource])
configurationAggregator_accountAggregationSources = Lens.lens (\ConfigurationAggregator' {accountAggregationSources} -> accountAggregationSources) (\s@ConfigurationAggregator' {} a -> s {accountAggregationSources = a} :: ConfigurationAggregator) Prelude.. Lens.mapping Prelude._Coerce

-- | AWS service that created the configuration aggregator.
configurationAggregator_createdBy :: Lens.Lens' ConfigurationAggregator (Prelude.Maybe Prelude.Text)
configurationAggregator_createdBy = Lens.lens (\ConfigurationAggregator' {createdBy} -> createdBy) (\s@ConfigurationAggregator' {} a -> s {createdBy = a} :: ConfigurationAggregator)

-- | The time of the last update.
configurationAggregator_lastUpdatedTime :: Lens.Lens' ConfigurationAggregator (Prelude.Maybe Prelude.UTCTime)
configurationAggregator_lastUpdatedTime = Lens.lens (\ConfigurationAggregator' {lastUpdatedTime} -> lastUpdatedTime) (\s@ConfigurationAggregator' {} a -> s {lastUpdatedTime = a} :: ConfigurationAggregator) Prelude.. Lens.mapping Prelude._Time

-- | Provides an organization and list of regions to be aggregated.
configurationAggregator_organizationAggregationSource :: Lens.Lens' ConfigurationAggregator (Prelude.Maybe OrganizationAggregationSource)
configurationAggregator_organizationAggregationSource = Lens.lens (\ConfigurationAggregator' {organizationAggregationSource} -> organizationAggregationSource) (\s@ConfigurationAggregator' {} a -> s {organizationAggregationSource = a} :: ConfigurationAggregator)

instance Prelude.FromJSON ConfigurationAggregator where
  parseJSON =
    Prelude.withObject
      "ConfigurationAggregator"
      ( \x ->
          ConfigurationAggregator'
            Prelude.<$> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "ConfigurationAggregatorArn")
            Prelude.<*> (x Prelude..:? "ConfigurationAggregatorName")
            Prelude.<*> ( x Prelude..:? "AccountAggregationSources"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "CreatedBy")
            Prelude.<*> (x Prelude..:? "LastUpdatedTime")
            Prelude.<*> (x Prelude..:? "OrganizationAggregationSource")
      )

instance Prelude.Hashable ConfigurationAggregator

instance Prelude.NFData ConfigurationAggregator
