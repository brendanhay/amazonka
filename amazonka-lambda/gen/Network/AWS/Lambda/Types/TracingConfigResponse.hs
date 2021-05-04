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
-- Module      : Network.AWS.Lambda.Types.TracingConfigResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.TracingConfigResponse where

import Network.AWS.Lambda.Types.TracingMode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The function\'s AWS X-Ray tracing configuration.
--
-- /See:/ 'newTracingConfigResponse' smart constructor.
data TracingConfigResponse = TracingConfigResponse'
  { -- | The tracing mode.
    mode :: Prelude.Maybe TracingMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON TracingConfigResponse where
  parseJSON =
    Prelude.withObject
      "TracingConfigResponse"
      ( \x ->
          TracingConfigResponse'
            Prelude.<$> (x Prelude..:? "Mode")
      )

instance Prelude.Hashable TracingConfigResponse

instance Prelude.NFData TracingConfigResponse
