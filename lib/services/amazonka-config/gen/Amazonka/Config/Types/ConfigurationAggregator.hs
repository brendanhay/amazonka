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
-- Module      : Amazonka.Config.Types.ConfigurationAggregator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ConfigurationAggregator where

import Amazonka.Config.Types.AccountAggregationSource
import Amazonka.Config.Types.OrganizationAggregationSource
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details about the configuration aggregator, including information
-- about source accounts, regions, and metadata of the aggregator.
--
-- /See:/ 'newConfigurationAggregator' smart constructor.
data ConfigurationAggregator = ConfigurationAggregator'
  { -- | Provides a list of source accounts and regions to be aggregated.
    accountAggregationSources :: Prelude.Maybe [AccountAggregationSource],
    -- | The Amazon Resource Name (ARN) of the aggregator.
    configurationAggregatorArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the aggregator.
    configurationAggregatorName :: Prelude.Maybe Prelude.Text,
    -- | Amazon Web Services service that created the configuration aggregator.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The time stamp when the configuration aggregator was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The time of the last update.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | Provides an organization and list of regions to be aggregated.
    organizationAggregationSource :: Prelude.Maybe OrganizationAggregationSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigurationAggregator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountAggregationSources', 'configurationAggregator_accountAggregationSources' - Provides a list of source accounts and regions to be aggregated.
--
-- 'configurationAggregatorArn', 'configurationAggregator_configurationAggregatorArn' - The Amazon Resource Name (ARN) of the aggregator.
--
-- 'configurationAggregatorName', 'configurationAggregator_configurationAggregatorName' - The name of the aggregator.
--
-- 'createdBy', 'configurationAggregator_createdBy' - Amazon Web Services service that created the configuration aggregator.
--
-- 'creationTime', 'configurationAggregator_creationTime' - The time stamp when the configuration aggregator was created.
--
-- 'lastUpdatedTime', 'configurationAggregator_lastUpdatedTime' - The time of the last update.
--
-- 'organizationAggregationSource', 'configurationAggregator_organizationAggregationSource' - Provides an organization and list of regions to be aggregated.
newConfigurationAggregator ::
  ConfigurationAggregator
newConfigurationAggregator =
  ConfigurationAggregator'
    { accountAggregationSources =
        Prelude.Nothing,
      configurationAggregatorArn = Prelude.Nothing,
      configurationAggregatorName = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      organizationAggregationSource = Prelude.Nothing
    }

-- | Provides a list of source accounts and regions to be aggregated.
configurationAggregator_accountAggregationSources :: Lens.Lens' ConfigurationAggregator (Prelude.Maybe [AccountAggregationSource])
configurationAggregator_accountAggregationSources = Lens.lens (\ConfigurationAggregator' {accountAggregationSources} -> accountAggregationSources) (\s@ConfigurationAggregator' {} a -> s {accountAggregationSources = a} :: ConfigurationAggregator) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the aggregator.
configurationAggregator_configurationAggregatorArn :: Lens.Lens' ConfigurationAggregator (Prelude.Maybe Prelude.Text)
configurationAggregator_configurationAggregatorArn = Lens.lens (\ConfigurationAggregator' {configurationAggregatorArn} -> configurationAggregatorArn) (\s@ConfigurationAggregator' {} a -> s {configurationAggregatorArn = a} :: ConfigurationAggregator)

-- | The name of the aggregator.
configurationAggregator_configurationAggregatorName :: Lens.Lens' ConfigurationAggregator (Prelude.Maybe Prelude.Text)
configurationAggregator_configurationAggregatorName = Lens.lens (\ConfigurationAggregator' {configurationAggregatorName} -> configurationAggregatorName) (\s@ConfigurationAggregator' {} a -> s {configurationAggregatorName = a} :: ConfigurationAggregator)

-- | Amazon Web Services service that created the configuration aggregator.
configurationAggregator_createdBy :: Lens.Lens' ConfigurationAggregator (Prelude.Maybe Prelude.Text)
configurationAggregator_createdBy = Lens.lens (\ConfigurationAggregator' {createdBy} -> createdBy) (\s@ConfigurationAggregator' {} a -> s {createdBy = a} :: ConfigurationAggregator)

-- | The time stamp when the configuration aggregator was created.
configurationAggregator_creationTime :: Lens.Lens' ConfigurationAggregator (Prelude.Maybe Prelude.UTCTime)
configurationAggregator_creationTime = Lens.lens (\ConfigurationAggregator' {creationTime} -> creationTime) (\s@ConfigurationAggregator' {} a -> s {creationTime = a} :: ConfigurationAggregator) Prelude.. Lens.mapping Data._Time

-- | The time of the last update.
configurationAggregator_lastUpdatedTime :: Lens.Lens' ConfigurationAggregator (Prelude.Maybe Prelude.UTCTime)
configurationAggregator_lastUpdatedTime = Lens.lens (\ConfigurationAggregator' {lastUpdatedTime} -> lastUpdatedTime) (\s@ConfigurationAggregator' {} a -> s {lastUpdatedTime = a} :: ConfigurationAggregator) Prelude.. Lens.mapping Data._Time

-- | Provides an organization and list of regions to be aggregated.
configurationAggregator_organizationAggregationSource :: Lens.Lens' ConfigurationAggregator (Prelude.Maybe OrganizationAggregationSource)
configurationAggregator_organizationAggregationSource = Lens.lens (\ConfigurationAggregator' {organizationAggregationSource} -> organizationAggregationSource) (\s@ConfigurationAggregator' {} a -> s {organizationAggregationSource = a} :: ConfigurationAggregator)

instance Data.FromJSON ConfigurationAggregator where
  parseJSON =
    Data.withObject
      "ConfigurationAggregator"
      ( \x ->
          ConfigurationAggregator'
            Prelude.<$> ( x
                            Data..:? "AccountAggregationSources"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ConfigurationAggregatorArn")
            Prelude.<*> (x Data..:? "ConfigurationAggregatorName")
            Prelude.<*> (x Data..:? "CreatedBy")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "LastUpdatedTime")
            Prelude.<*> (x Data..:? "OrganizationAggregationSource")
      )

instance Prelude.Hashable ConfigurationAggregator where
  hashWithSalt _salt ConfigurationAggregator' {..} =
    _salt
      `Prelude.hashWithSalt` accountAggregationSources
      `Prelude.hashWithSalt` configurationAggregatorArn
      `Prelude.hashWithSalt` configurationAggregatorName
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` organizationAggregationSource

instance Prelude.NFData ConfigurationAggregator where
  rnf ConfigurationAggregator' {..} =
    Prelude.rnf accountAggregationSources
      `Prelude.seq` Prelude.rnf configurationAggregatorArn
      `Prelude.seq` Prelude.rnf configurationAggregatorName
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf organizationAggregationSource
