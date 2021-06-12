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
-- Module      : Network.AWS.Lambda.Types.TracingConfigResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.TracingConfigResponse where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types.TracingMode
import qualified Network.AWS.Lens as Lens

-- | The function\'s AWS X-Ray tracing configuration.
--
-- /See:/ 'newTracingConfigResponse' smart constructor.
data TracingConfigResponse = TracingConfigResponse'
  { -- | The tracing mode.
    mode :: Core.Maybe TracingMode
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  TracingConfigResponse' {mode = Core.Nothing}

-- | The tracing mode.
tracingConfigResponse_mode :: Lens.Lens' TracingConfigResponse (Core.Maybe TracingMode)
tracingConfigResponse_mode = Lens.lens (\TracingConfigResponse' {mode} -> mode) (\s@TracingConfigResponse' {} a -> s {mode = a} :: TracingConfigResponse)

instance Core.FromJSON TracingConfigResponse where
  parseJSON =
    Core.withObject
      "TracingConfigResponse"
      ( \x ->
          TracingConfigResponse' Core.<$> (x Core..:? "Mode")
      )

instance Core.Hashable TracingConfigResponse

instance Core.NFData TracingConfigResponse
