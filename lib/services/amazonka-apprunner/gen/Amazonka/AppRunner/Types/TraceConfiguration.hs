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
-- Module      : Amazonka.AppRunner.Types.TraceConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.TraceConfiguration where

import Amazonka.AppRunner.Types.TracingVendor
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration of the tracing feature within an App Runner
-- observability configuration.
--
-- /See:/ 'newTraceConfiguration' smart constructor.
data TraceConfiguration = TraceConfiguration'
  { -- | The implementation provider chosen for tracing App Runner services.
    vendor :: TracingVendor
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TraceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vendor', 'traceConfiguration_vendor' - The implementation provider chosen for tracing App Runner services.
newTraceConfiguration ::
  -- | 'vendor'
  TracingVendor ->
  TraceConfiguration
newTraceConfiguration pVendor_ =
  TraceConfiguration' {vendor = pVendor_}

-- | The implementation provider chosen for tracing App Runner services.
traceConfiguration_vendor :: Lens.Lens' TraceConfiguration TracingVendor
traceConfiguration_vendor = Lens.lens (\TraceConfiguration' {vendor} -> vendor) (\s@TraceConfiguration' {} a -> s {vendor = a} :: TraceConfiguration)

instance Data.FromJSON TraceConfiguration where
  parseJSON =
    Data.withObject
      "TraceConfiguration"
      ( \x ->
          TraceConfiguration' Prelude.<$> (x Data..: "Vendor")
      )

instance Prelude.Hashable TraceConfiguration where
  hashWithSalt _salt TraceConfiguration' {..} =
    _salt `Prelude.hashWithSalt` vendor

instance Prelude.NFData TraceConfiguration where
  rnf TraceConfiguration' {..} = Prelude.rnf vendor

instance Data.ToJSON TraceConfiguration where
  toJSON TraceConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Vendor" Data..= vendor)]
      )
