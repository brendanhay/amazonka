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
-- Module      : Amazonka.AppRunner.Types.ObservabilityConfigurationSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.ObservabilityConfigurationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides summary information about an App Runner observability
-- configuration resource.
--
-- This type contains limited information about an observability
-- configuration. It includes only identification information, without
-- configuration details. It\'s returned by the
-- ListObservabilityConfigurations action. Complete configuration
-- information is returned by the CreateObservabilityConfiguration,
-- DescribeObservabilityConfiguration, and DeleteObservabilityConfiguration
-- actions using the ObservabilityConfiguration type.
--
-- /See:/ 'newObservabilityConfigurationSummary' smart constructor.
data ObservabilityConfigurationSummary = ObservabilityConfigurationSummary'
  { -- | The Amazon Resource Name (ARN) of this observability configuration.
    observabilityConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | The customer-provided observability configuration name. It can be used
    -- in multiple revisions of a configuration.
    observabilityConfigurationName :: Prelude.Maybe Prelude.Text,
    -- | The revision of this observability configuration. It\'s unique among all
    -- the active configurations (@\"Status\": \"ACTIVE\"@) that share the same
    -- @ObservabilityConfigurationName@.
    observabilityConfigurationRevision :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ObservabilityConfigurationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'observabilityConfigurationArn', 'observabilityConfigurationSummary_observabilityConfigurationArn' - The Amazon Resource Name (ARN) of this observability configuration.
--
-- 'observabilityConfigurationName', 'observabilityConfigurationSummary_observabilityConfigurationName' - The customer-provided observability configuration name. It can be used
-- in multiple revisions of a configuration.
--
-- 'observabilityConfigurationRevision', 'observabilityConfigurationSummary_observabilityConfigurationRevision' - The revision of this observability configuration. It\'s unique among all
-- the active configurations (@\"Status\": \"ACTIVE\"@) that share the same
-- @ObservabilityConfigurationName@.
newObservabilityConfigurationSummary ::
  ObservabilityConfigurationSummary
newObservabilityConfigurationSummary =
  ObservabilityConfigurationSummary'
    { observabilityConfigurationArn =
        Prelude.Nothing,
      observabilityConfigurationName =
        Prelude.Nothing,
      observabilityConfigurationRevision =
        Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of this observability configuration.
observabilityConfigurationSummary_observabilityConfigurationArn :: Lens.Lens' ObservabilityConfigurationSummary (Prelude.Maybe Prelude.Text)
observabilityConfigurationSummary_observabilityConfigurationArn = Lens.lens (\ObservabilityConfigurationSummary' {observabilityConfigurationArn} -> observabilityConfigurationArn) (\s@ObservabilityConfigurationSummary' {} a -> s {observabilityConfigurationArn = a} :: ObservabilityConfigurationSummary)

-- | The customer-provided observability configuration name. It can be used
-- in multiple revisions of a configuration.
observabilityConfigurationSummary_observabilityConfigurationName :: Lens.Lens' ObservabilityConfigurationSummary (Prelude.Maybe Prelude.Text)
observabilityConfigurationSummary_observabilityConfigurationName = Lens.lens (\ObservabilityConfigurationSummary' {observabilityConfigurationName} -> observabilityConfigurationName) (\s@ObservabilityConfigurationSummary' {} a -> s {observabilityConfigurationName = a} :: ObservabilityConfigurationSummary)

-- | The revision of this observability configuration. It\'s unique among all
-- the active configurations (@\"Status\": \"ACTIVE\"@) that share the same
-- @ObservabilityConfigurationName@.
observabilityConfigurationSummary_observabilityConfigurationRevision :: Lens.Lens' ObservabilityConfigurationSummary (Prelude.Maybe Prelude.Int)
observabilityConfigurationSummary_observabilityConfigurationRevision = Lens.lens (\ObservabilityConfigurationSummary' {observabilityConfigurationRevision} -> observabilityConfigurationRevision) (\s@ObservabilityConfigurationSummary' {} a -> s {observabilityConfigurationRevision = a} :: ObservabilityConfigurationSummary)

instance
  Data.FromJSON
    ObservabilityConfigurationSummary
  where
  parseJSON =
    Data.withObject
      "ObservabilityConfigurationSummary"
      ( \x ->
          ObservabilityConfigurationSummary'
            Prelude.<$> (x Data..:? "ObservabilityConfigurationArn")
            Prelude.<*> (x Data..:? "ObservabilityConfigurationName")
            Prelude.<*> (x Data..:? "ObservabilityConfigurationRevision")
      )

instance
  Prelude.Hashable
    ObservabilityConfigurationSummary
  where
  hashWithSalt
    _salt
    ObservabilityConfigurationSummary' {..} =
      _salt
        `Prelude.hashWithSalt` observabilityConfigurationArn
        `Prelude.hashWithSalt` observabilityConfigurationName
        `Prelude.hashWithSalt` observabilityConfigurationRevision

instance
  Prelude.NFData
    ObservabilityConfigurationSummary
  where
  rnf ObservabilityConfigurationSummary' {..} =
    Prelude.rnf observabilityConfigurationArn
      `Prelude.seq` Prelude.rnf observabilityConfigurationName
      `Prelude.seq` Prelude.rnf observabilityConfigurationRevision
