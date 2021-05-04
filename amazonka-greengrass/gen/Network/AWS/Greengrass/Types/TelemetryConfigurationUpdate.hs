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
-- Module      : Network.AWS.Greengrass.Types.TelemetryConfigurationUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.TelemetryConfigurationUpdate where

import Network.AWS.Greengrass.Types.Telemetry
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configuration settings for running telemetry.
--
-- /See:/ 'newTelemetryConfigurationUpdate' smart constructor.
data TelemetryConfigurationUpdate = TelemetryConfigurationUpdate'
  { -- | Configure telemetry to be on or off.
    telemetry :: Telemetry
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData TelemetryConfigurationUpdate

instance Prelude.ToJSON TelemetryConfigurationUpdate where
  toJSON TelemetryConfigurationUpdate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Telemetry" Prelude..= telemetry)]
      )
