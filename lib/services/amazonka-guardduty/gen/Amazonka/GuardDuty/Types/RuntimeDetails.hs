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
-- Module      : Amazonka.GuardDuty.Types.RuntimeDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.RuntimeDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.ProcessDetails
import Amazonka.GuardDuty.Types.RuntimeContext
import qualified Amazonka.Prelude as Prelude

-- | Information about the process and any required context values for a
-- specific finding.
--
-- /See:/ 'newRuntimeDetails' smart constructor.
data RuntimeDetails = RuntimeDetails'
  { -- | Additional information about the suspicious activity.
    context :: Prelude.Maybe RuntimeContext,
    -- | Information about the observed process.
    process :: Prelude.Maybe ProcessDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuntimeDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'context', 'runtimeDetails_context' - Additional information about the suspicious activity.
--
-- 'process', 'runtimeDetails_process' - Information about the observed process.
newRuntimeDetails ::
  RuntimeDetails
newRuntimeDetails =
  RuntimeDetails'
    { context = Prelude.Nothing,
      process = Prelude.Nothing
    }

-- | Additional information about the suspicious activity.
runtimeDetails_context :: Lens.Lens' RuntimeDetails (Prelude.Maybe RuntimeContext)
runtimeDetails_context = Lens.lens (\RuntimeDetails' {context} -> context) (\s@RuntimeDetails' {} a -> s {context = a} :: RuntimeDetails)

-- | Information about the observed process.
runtimeDetails_process :: Lens.Lens' RuntimeDetails (Prelude.Maybe ProcessDetails)
runtimeDetails_process = Lens.lens (\RuntimeDetails' {process} -> process) (\s@RuntimeDetails' {} a -> s {process = a} :: RuntimeDetails)

instance Data.FromJSON RuntimeDetails where
  parseJSON =
    Data.withObject
      "RuntimeDetails"
      ( \x ->
          RuntimeDetails'
            Prelude.<$> (x Data..:? "context")
            Prelude.<*> (x Data..:? "process")
      )

instance Prelude.Hashable RuntimeDetails where
  hashWithSalt _salt RuntimeDetails' {..} =
    _salt
      `Prelude.hashWithSalt` context
      `Prelude.hashWithSalt` process

instance Prelude.NFData RuntimeDetails where
  rnf RuntimeDetails' {..} =
    Prelude.rnf context
      `Prelude.seq` Prelude.rnf process
