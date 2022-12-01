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
-- Module      : Amazonka.Lambda.Types.TracingConfigResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.TracingConfigResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lambda.Types.TracingMode
import qualified Amazonka.Prelude as Prelude

-- | The function\'s X-Ray tracing configuration.
--
-- /See:/ 'newTracingConfigResponse' smart constructor.
data TracingConfigResponse = TracingConfigResponse'
  { -- | The tracing mode.
    mode :: Prelude.Maybe TracingMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TracingConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mode', 'tracingConfigResponse_mode' - The tracing mode.
newTracingConfigResponse ::
  TracingConfigResponse
newTracingConfigResponse =
  TracingConfigResponse' {mode = Prelude.Nothing}

-- | The tracing mode.
tracingConfigResponse_mode :: Lens.Lens' TracingConfigResponse (Prelude.Maybe TracingMode)
tracingConfigResponse_mode = Lens.lens (\TracingConfigResponse' {mode} -> mode) (\s@TracingConfigResponse' {} a -> s {mode = a} :: TracingConfigResponse)

instance Core.FromJSON TracingConfigResponse where
  parseJSON =
    Core.withObject
      "TracingConfigResponse"
      ( \x ->
          TracingConfigResponse'
            Prelude.<$> (x Core..:? "Mode")
      )

instance Prelude.Hashable TracingConfigResponse where
  hashWithSalt _salt TracingConfigResponse' {..} =
    _salt `Prelude.hashWithSalt` mode

instance Prelude.NFData TracingConfigResponse where
  rnf TracingConfigResponse' {..} = Prelude.rnf mode
