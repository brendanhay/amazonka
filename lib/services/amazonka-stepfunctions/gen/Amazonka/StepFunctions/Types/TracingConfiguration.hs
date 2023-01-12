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
-- Module      : Amazonka.StepFunctions.Types.TracingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.TracingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Selects whether or not the state machine\'s X-Ray tracing is enabled.
-- Default is @false@
--
-- /See:/ 'newTracingConfiguration' smart constructor.
data TracingConfiguration = TracingConfiguration'
  { -- | When set to @true@, X-Ray tracing is enabled.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TracingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'tracingConfiguration_enabled' - When set to @true@, X-Ray tracing is enabled.
newTracingConfiguration ::
  TracingConfiguration
newTracingConfiguration =
  TracingConfiguration' {enabled = Prelude.Nothing}

-- | When set to @true@, X-Ray tracing is enabled.
tracingConfiguration_enabled :: Lens.Lens' TracingConfiguration (Prelude.Maybe Prelude.Bool)
tracingConfiguration_enabled = Lens.lens (\TracingConfiguration' {enabled} -> enabled) (\s@TracingConfiguration' {} a -> s {enabled = a} :: TracingConfiguration)

instance Data.FromJSON TracingConfiguration where
  parseJSON =
    Data.withObject
      "TracingConfiguration"
      ( \x ->
          TracingConfiguration'
            Prelude.<$> (x Data..:? "enabled")
      )

instance Prelude.Hashable TracingConfiguration where
  hashWithSalt _salt TracingConfiguration' {..} =
    _salt `Prelude.hashWithSalt` enabled

instance Prelude.NFData TracingConfiguration where
  rnf TracingConfiguration' {..} = Prelude.rnf enabled

instance Data.ToJSON TracingConfiguration where
  toJSON TracingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("enabled" Data..=) Prelude.<$> enabled]
      )
