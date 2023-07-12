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
-- Module      : Amazonka.AppRunner.Types.ServiceObservabilityConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.ServiceObservabilityConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the observability configuration of an App Runner service.
-- These are additional observability features, like tracing, that you
-- choose to enable. They\'re configured in a separate resource that you
-- associate with your service.
--
-- /See:/ 'newServiceObservabilityConfiguration' smart constructor.
data ServiceObservabilityConfiguration = ServiceObservabilityConfiguration'
  { -- | The Amazon Resource Name (ARN) of the observability configuration that
    -- is associated with the service. Specified only when
    -- @ObservabilityEnabled@ is @true@.
    --
    -- Specify an ARN with a name and a revision number to associate that
    -- revision. For example:
    -- @arn:aws:apprunner:us-east-1:123456789012:observabilityconfiguration\/xray-tracing\/3@
    --
    -- Specify just the name to associate the latest revision. For example:
    -- @arn:aws:apprunner:us-east-1:123456789012:observabilityconfiguration\/xray-tracing@
    observabilityConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | When @true@, an observability configuration resource is associated with
    -- the service, and an @ObservabilityConfigurationArn@ is specified.
    observabilityEnabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceObservabilityConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'observabilityConfigurationArn', 'serviceObservabilityConfiguration_observabilityConfigurationArn' - The Amazon Resource Name (ARN) of the observability configuration that
-- is associated with the service. Specified only when
-- @ObservabilityEnabled@ is @true@.
--
-- Specify an ARN with a name and a revision number to associate that
-- revision. For example:
-- @arn:aws:apprunner:us-east-1:123456789012:observabilityconfiguration\/xray-tracing\/3@
--
-- Specify just the name to associate the latest revision. For example:
-- @arn:aws:apprunner:us-east-1:123456789012:observabilityconfiguration\/xray-tracing@
--
-- 'observabilityEnabled', 'serviceObservabilityConfiguration_observabilityEnabled' - When @true@, an observability configuration resource is associated with
-- the service, and an @ObservabilityConfigurationArn@ is specified.
newServiceObservabilityConfiguration ::
  -- | 'observabilityEnabled'
  Prelude.Bool ->
  ServiceObservabilityConfiguration
newServiceObservabilityConfiguration
  pObservabilityEnabled_ =
    ServiceObservabilityConfiguration'
      { observabilityConfigurationArn =
          Prelude.Nothing,
        observabilityEnabled =
          pObservabilityEnabled_
      }

-- | The Amazon Resource Name (ARN) of the observability configuration that
-- is associated with the service. Specified only when
-- @ObservabilityEnabled@ is @true@.
--
-- Specify an ARN with a name and a revision number to associate that
-- revision. For example:
-- @arn:aws:apprunner:us-east-1:123456789012:observabilityconfiguration\/xray-tracing\/3@
--
-- Specify just the name to associate the latest revision. For example:
-- @arn:aws:apprunner:us-east-1:123456789012:observabilityconfiguration\/xray-tracing@
serviceObservabilityConfiguration_observabilityConfigurationArn :: Lens.Lens' ServiceObservabilityConfiguration (Prelude.Maybe Prelude.Text)
serviceObservabilityConfiguration_observabilityConfigurationArn = Lens.lens (\ServiceObservabilityConfiguration' {observabilityConfigurationArn} -> observabilityConfigurationArn) (\s@ServiceObservabilityConfiguration' {} a -> s {observabilityConfigurationArn = a} :: ServiceObservabilityConfiguration)

-- | When @true@, an observability configuration resource is associated with
-- the service, and an @ObservabilityConfigurationArn@ is specified.
serviceObservabilityConfiguration_observabilityEnabled :: Lens.Lens' ServiceObservabilityConfiguration Prelude.Bool
serviceObservabilityConfiguration_observabilityEnabled = Lens.lens (\ServiceObservabilityConfiguration' {observabilityEnabled} -> observabilityEnabled) (\s@ServiceObservabilityConfiguration' {} a -> s {observabilityEnabled = a} :: ServiceObservabilityConfiguration)

instance
  Data.FromJSON
    ServiceObservabilityConfiguration
  where
  parseJSON =
    Data.withObject
      "ServiceObservabilityConfiguration"
      ( \x ->
          ServiceObservabilityConfiguration'
            Prelude.<$> (x Data..:? "ObservabilityConfigurationArn")
            Prelude.<*> (x Data..: "ObservabilityEnabled")
      )

instance
  Prelude.Hashable
    ServiceObservabilityConfiguration
  where
  hashWithSalt
    _salt
    ServiceObservabilityConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` observabilityConfigurationArn
        `Prelude.hashWithSalt` observabilityEnabled

instance
  Prelude.NFData
    ServiceObservabilityConfiguration
  where
  rnf ServiceObservabilityConfiguration' {..} =
    Prelude.rnf observabilityConfigurationArn
      `Prelude.seq` Prelude.rnf observabilityEnabled

instance
  Data.ToJSON
    ServiceObservabilityConfiguration
  where
  toJSON ServiceObservabilityConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ObservabilityConfigurationArn" Data..=)
              Prelude.<$> observabilityConfigurationArn,
            Prelude.Just
              ( "ObservabilityEnabled"
                  Data..= observabilityEnabled
              )
          ]
      )
