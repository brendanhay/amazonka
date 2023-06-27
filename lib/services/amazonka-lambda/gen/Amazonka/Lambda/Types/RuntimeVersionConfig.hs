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
-- Module      : Amazonka.Lambda.Types.RuntimeVersionConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.RuntimeVersionConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types.RuntimeVersionError
import qualified Amazonka.Prelude as Prelude

-- | The ARN of the runtime and any errors that occured.
--
-- /See:/ 'newRuntimeVersionConfig' smart constructor.
data RuntimeVersionConfig = RuntimeVersionConfig'
  { -- | Error response when Lambda is unable to retrieve the runtime version for
    -- a function.
    error :: Prelude.Maybe RuntimeVersionError,
    -- | The ARN of the runtime version you want the function to use.
    runtimeVersionArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuntimeVersionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'runtimeVersionConfig_error' - Error response when Lambda is unable to retrieve the runtime version for
-- a function.
--
-- 'runtimeVersionArn', 'runtimeVersionConfig_runtimeVersionArn' - The ARN of the runtime version you want the function to use.
newRuntimeVersionConfig ::
  RuntimeVersionConfig
newRuntimeVersionConfig =
  RuntimeVersionConfig'
    { error = Prelude.Nothing,
      runtimeVersionArn = Prelude.Nothing
    }

-- | Error response when Lambda is unable to retrieve the runtime version for
-- a function.
runtimeVersionConfig_error :: Lens.Lens' RuntimeVersionConfig (Prelude.Maybe RuntimeVersionError)
runtimeVersionConfig_error = Lens.lens (\RuntimeVersionConfig' {error} -> error) (\s@RuntimeVersionConfig' {} a -> s {error = a} :: RuntimeVersionConfig)

-- | The ARN of the runtime version you want the function to use.
runtimeVersionConfig_runtimeVersionArn :: Lens.Lens' RuntimeVersionConfig (Prelude.Maybe Prelude.Text)
runtimeVersionConfig_runtimeVersionArn = Lens.lens (\RuntimeVersionConfig' {runtimeVersionArn} -> runtimeVersionArn) (\s@RuntimeVersionConfig' {} a -> s {runtimeVersionArn = a} :: RuntimeVersionConfig)

instance Data.FromJSON RuntimeVersionConfig where
  parseJSON =
    Data.withObject
      "RuntimeVersionConfig"
      ( \x ->
          RuntimeVersionConfig'
            Prelude.<$> (x Data..:? "Error")
            Prelude.<*> (x Data..:? "RuntimeVersionArn")
      )

instance Prelude.Hashable RuntimeVersionConfig where
  hashWithSalt _salt RuntimeVersionConfig' {..} =
    _salt
      `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` runtimeVersionArn

instance Prelude.NFData RuntimeVersionConfig where
  rnf RuntimeVersionConfig' {..} =
    Prelude.rnf error
      `Prelude.seq` Prelude.rnf runtimeVersionArn
