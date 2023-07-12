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
-- Module      : Amazonka.AppRunner.Types.ObservabilityConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.ObservabilityConfiguration where

import Amazonka.AppRunner.Types.ObservabilityConfigurationStatus
import Amazonka.AppRunner.Types.TraceConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an App Runner observability configuration resource. Multiple
-- revisions of a configuration have the same
-- @ObservabilityConfigurationName@ and different
-- @ObservabilityConfigurationRevision@ values.
--
-- The resource is designed to configure multiple features (currently one
-- feature, tracing). This type contains optional members that describe the
-- configuration of these features (currently one member,
-- @TraceConfiguration@). If a feature member isn\'t specified, the feature
-- isn\'t enabled.
--
-- /See:/ 'newObservabilityConfiguration' smart constructor.
data ObservabilityConfiguration = ObservabilityConfiguration'
  { -- | The time when the observability configuration was created. It\'s in Unix
    -- time stamp format.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The time when the observability configuration was deleted. It\'s in Unix
    -- time stamp format.
    deletedAt :: Prelude.Maybe Data.POSIX,
    -- | It\'s set to @true@ for the configuration with the highest @Revision@
    -- among all configurations that share the same
    -- @ObservabilityConfigurationName@. It\'s set to @false@ otherwise.
    latest :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of this observability configuration.
    observabilityConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | The customer-provided observability configuration name. It can be used
    -- in multiple revisions of a configuration.
    observabilityConfigurationName :: Prelude.Maybe Prelude.Text,
    -- | The revision of this observability configuration. It\'s unique among all
    -- the active configurations (@\"Status\": \"ACTIVE\"@) that share the same
    -- @ObservabilityConfigurationName@.
    observabilityConfigurationRevision :: Prelude.Maybe Prelude.Int,
    -- | The current state of the observability configuration. If the status of a
    -- configuration revision is @INACTIVE@, it was deleted and can\'t be used.
    -- Inactive configuration revisions are permanently removed some time after
    -- they are deleted.
    status :: Prelude.Maybe ObservabilityConfigurationStatus,
    -- | The configuration of the tracing feature within this observability
    -- configuration. If not specified, tracing isn\'t enabled.
    traceConfiguration :: Prelude.Maybe TraceConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ObservabilityConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'observabilityConfiguration_createdAt' - The time when the observability configuration was created. It\'s in Unix
-- time stamp format.
--
-- 'deletedAt', 'observabilityConfiguration_deletedAt' - The time when the observability configuration was deleted. It\'s in Unix
-- time stamp format.
--
-- 'latest', 'observabilityConfiguration_latest' - It\'s set to @true@ for the configuration with the highest @Revision@
-- among all configurations that share the same
-- @ObservabilityConfigurationName@. It\'s set to @false@ otherwise.
--
-- 'observabilityConfigurationArn', 'observabilityConfiguration_observabilityConfigurationArn' - The Amazon Resource Name (ARN) of this observability configuration.
--
-- 'observabilityConfigurationName', 'observabilityConfiguration_observabilityConfigurationName' - The customer-provided observability configuration name. It can be used
-- in multiple revisions of a configuration.
--
-- 'observabilityConfigurationRevision', 'observabilityConfiguration_observabilityConfigurationRevision' - The revision of this observability configuration. It\'s unique among all
-- the active configurations (@\"Status\": \"ACTIVE\"@) that share the same
-- @ObservabilityConfigurationName@.
--
-- 'status', 'observabilityConfiguration_status' - The current state of the observability configuration. If the status of a
-- configuration revision is @INACTIVE@, it was deleted and can\'t be used.
-- Inactive configuration revisions are permanently removed some time after
-- they are deleted.
--
-- 'traceConfiguration', 'observabilityConfiguration_traceConfiguration' - The configuration of the tracing feature within this observability
-- configuration. If not specified, tracing isn\'t enabled.
newObservabilityConfiguration ::
  ObservabilityConfiguration
newObservabilityConfiguration =
  ObservabilityConfiguration'
    { createdAt =
        Prelude.Nothing,
      deletedAt = Prelude.Nothing,
      latest = Prelude.Nothing,
      observabilityConfigurationArn = Prelude.Nothing,
      observabilityConfigurationName =
        Prelude.Nothing,
      observabilityConfigurationRevision =
        Prelude.Nothing,
      status = Prelude.Nothing,
      traceConfiguration = Prelude.Nothing
    }

-- | The time when the observability configuration was created. It\'s in Unix
-- time stamp format.
observabilityConfiguration_createdAt :: Lens.Lens' ObservabilityConfiguration (Prelude.Maybe Prelude.UTCTime)
observabilityConfiguration_createdAt = Lens.lens (\ObservabilityConfiguration' {createdAt} -> createdAt) (\s@ObservabilityConfiguration' {} a -> s {createdAt = a} :: ObservabilityConfiguration) Prelude.. Lens.mapping Data._Time

-- | The time when the observability configuration was deleted. It\'s in Unix
-- time stamp format.
observabilityConfiguration_deletedAt :: Lens.Lens' ObservabilityConfiguration (Prelude.Maybe Prelude.UTCTime)
observabilityConfiguration_deletedAt = Lens.lens (\ObservabilityConfiguration' {deletedAt} -> deletedAt) (\s@ObservabilityConfiguration' {} a -> s {deletedAt = a} :: ObservabilityConfiguration) Prelude.. Lens.mapping Data._Time

-- | It\'s set to @true@ for the configuration with the highest @Revision@
-- among all configurations that share the same
-- @ObservabilityConfigurationName@. It\'s set to @false@ otherwise.
observabilityConfiguration_latest :: Lens.Lens' ObservabilityConfiguration (Prelude.Maybe Prelude.Bool)
observabilityConfiguration_latest = Lens.lens (\ObservabilityConfiguration' {latest} -> latest) (\s@ObservabilityConfiguration' {} a -> s {latest = a} :: ObservabilityConfiguration)

-- | The Amazon Resource Name (ARN) of this observability configuration.
observabilityConfiguration_observabilityConfigurationArn :: Lens.Lens' ObservabilityConfiguration (Prelude.Maybe Prelude.Text)
observabilityConfiguration_observabilityConfigurationArn = Lens.lens (\ObservabilityConfiguration' {observabilityConfigurationArn} -> observabilityConfigurationArn) (\s@ObservabilityConfiguration' {} a -> s {observabilityConfigurationArn = a} :: ObservabilityConfiguration)

-- | The customer-provided observability configuration name. It can be used
-- in multiple revisions of a configuration.
observabilityConfiguration_observabilityConfigurationName :: Lens.Lens' ObservabilityConfiguration (Prelude.Maybe Prelude.Text)
observabilityConfiguration_observabilityConfigurationName = Lens.lens (\ObservabilityConfiguration' {observabilityConfigurationName} -> observabilityConfigurationName) (\s@ObservabilityConfiguration' {} a -> s {observabilityConfigurationName = a} :: ObservabilityConfiguration)

-- | The revision of this observability configuration. It\'s unique among all
-- the active configurations (@\"Status\": \"ACTIVE\"@) that share the same
-- @ObservabilityConfigurationName@.
observabilityConfiguration_observabilityConfigurationRevision :: Lens.Lens' ObservabilityConfiguration (Prelude.Maybe Prelude.Int)
observabilityConfiguration_observabilityConfigurationRevision = Lens.lens (\ObservabilityConfiguration' {observabilityConfigurationRevision} -> observabilityConfigurationRevision) (\s@ObservabilityConfiguration' {} a -> s {observabilityConfigurationRevision = a} :: ObservabilityConfiguration)

-- | The current state of the observability configuration. If the status of a
-- configuration revision is @INACTIVE@, it was deleted and can\'t be used.
-- Inactive configuration revisions are permanently removed some time after
-- they are deleted.
observabilityConfiguration_status :: Lens.Lens' ObservabilityConfiguration (Prelude.Maybe ObservabilityConfigurationStatus)
observabilityConfiguration_status = Lens.lens (\ObservabilityConfiguration' {status} -> status) (\s@ObservabilityConfiguration' {} a -> s {status = a} :: ObservabilityConfiguration)

-- | The configuration of the tracing feature within this observability
-- configuration. If not specified, tracing isn\'t enabled.
observabilityConfiguration_traceConfiguration :: Lens.Lens' ObservabilityConfiguration (Prelude.Maybe TraceConfiguration)
observabilityConfiguration_traceConfiguration = Lens.lens (\ObservabilityConfiguration' {traceConfiguration} -> traceConfiguration) (\s@ObservabilityConfiguration' {} a -> s {traceConfiguration = a} :: ObservabilityConfiguration)

instance Data.FromJSON ObservabilityConfiguration where
  parseJSON =
    Data.withObject
      "ObservabilityConfiguration"
      ( \x ->
          ObservabilityConfiguration'
            Prelude.<$> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "DeletedAt")
            Prelude.<*> (x Data..:? "Latest")
            Prelude.<*> (x Data..:? "ObservabilityConfigurationArn")
            Prelude.<*> (x Data..:? "ObservabilityConfigurationName")
            Prelude.<*> (x Data..:? "ObservabilityConfigurationRevision")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "TraceConfiguration")
      )

instance Prelude.Hashable ObservabilityConfiguration where
  hashWithSalt _salt ObservabilityConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` deletedAt
      `Prelude.hashWithSalt` latest
      `Prelude.hashWithSalt` observabilityConfigurationArn
      `Prelude.hashWithSalt` observabilityConfigurationName
      `Prelude.hashWithSalt` observabilityConfigurationRevision
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` traceConfiguration

instance Prelude.NFData ObservabilityConfiguration where
  rnf ObservabilityConfiguration' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf deletedAt
      `Prelude.seq` Prelude.rnf latest
      `Prelude.seq` Prelude.rnf observabilityConfigurationArn
      `Prelude.seq` Prelude.rnf observabilityConfigurationName
      `Prelude.seq` Prelude.rnf observabilityConfigurationRevision
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf traceConfiguration
