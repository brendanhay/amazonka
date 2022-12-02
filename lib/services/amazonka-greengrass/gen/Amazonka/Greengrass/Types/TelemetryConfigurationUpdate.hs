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
-- Module      : Amazonka.Greengrass.Types.TelemetryConfigurationUpdate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.TelemetryConfigurationUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types.Telemetry
import qualified Amazonka.Prelude as Prelude

-- | Configuration settings for running telemetry.
--
-- /See:/ 'newTelemetryConfigurationUpdate' smart constructor.
data TelemetryConfigurationUpdate = TelemetryConfigurationUpdate'
  { -- | Configure telemetry to be on or off.
    telemetry :: Telemetry
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TelemetryConfigurationUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'telemetry', 'telemetryConfigurationUpdate_telemetry' - Configure telemetry to be on or off.
newTelemetryConfigurationUpdate ::
  -- | 'telemetry'
  Telemetry ->
  TelemetryConfigurationUpdate
newTelemetryConfigurationUpdate pTelemetry_ =
  TelemetryConfigurationUpdate'
    { telemetry =
        pTelemetry_
    }

-- | Configure telemetry to be on or off.
telemetryConfigurationUpdate_telemetry :: Lens.Lens' TelemetryConfigurationUpdate Telemetry
telemetryConfigurationUpdate_telemetry = Lens.lens (\TelemetryConfigurationUpdate' {telemetry} -> telemetry) (\s@TelemetryConfigurationUpdate' {} a -> s {telemetry = a} :: TelemetryConfigurationUpdate)

instance
  Prelude.Hashable
    TelemetryConfigurationUpdate
  where
  hashWithSalt _salt TelemetryConfigurationUpdate' {..} =
    _salt `Prelude.hashWithSalt` telemetry

instance Prelude.NFData TelemetryConfigurationUpdate where
  rnf TelemetryConfigurationUpdate' {..} =
    Prelude.rnf telemetry

instance Data.ToJSON TelemetryConfigurationUpdate where
  toJSON TelemetryConfigurationUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Telemetry" Data..= telemetry)]
      )
