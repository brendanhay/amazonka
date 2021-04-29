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
-- Module      : Network.AWS.Lambda.Types.TracingConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.TracingConfig where

import Network.AWS.Lambda.Types.TracingMode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The function\'s AWS X-Ray tracing configuration. To sample and record
-- incoming requests, set @Mode@ to @Active@.
--
-- /See:/ 'newTracingConfig' smart constructor.
data TracingConfig = TracingConfig'
  { -- | The tracing mode.
    mode :: Prelude.Maybe TracingMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TracingConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mode', 'tracingConfig_mode' - The tracing mode.
newTracingConfig ::
  TracingConfig
newTracingConfig =
  TracingConfig' {mode = Prelude.Nothing}

-- | The tracing mode.
tracingConfig_mode :: Lens.Lens' TracingConfig (Prelude.Maybe TracingMode)
tracingConfig_mode = Lens.lens (\TracingConfig' {mode} -> mode) (\s@TracingConfig' {} a -> s {mode = a} :: TracingConfig)

instance Prelude.Hashable TracingConfig

instance Prelude.NFData TracingConfig

instance Prelude.ToJSON TracingConfig where
  toJSON TracingConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("Mode" Prelude..=) Prelude.<$> mode]
      )
