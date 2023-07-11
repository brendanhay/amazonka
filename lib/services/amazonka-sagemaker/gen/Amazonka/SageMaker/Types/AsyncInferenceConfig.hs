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
-- Module      : Amazonka.SageMaker.Types.AsyncInferenceConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AsyncInferenceConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AsyncInferenceClientConfig
import Amazonka.SageMaker.Types.AsyncInferenceOutputConfig

-- | Specifies configuration for how an endpoint performs asynchronous
-- inference.
--
-- /See:/ 'newAsyncInferenceConfig' smart constructor.
data AsyncInferenceConfig = AsyncInferenceConfig'
  { -- | Configures the behavior of the client used by SageMaker to interact with
    -- the model container during asynchronous inference.
    clientConfig :: Prelude.Maybe AsyncInferenceClientConfig,
    -- | Specifies the configuration for asynchronous inference invocation
    -- outputs.
    outputConfig :: AsyncInferenceOutputConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AsyncInferenceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientConfig', 'asyncInferenceConfig_clientConfig' - Configures the behavior of the client used by SageMaker to interact with
-- the model container during asynchronous inference.
--
-- 'outputConfig', 'asyncInferenceConfig_outputConfig' - Specifies the configuration for asynchronous inference invocation
-- outputs.
newAsyncInferenceConfig ::
  -- | 'outputConfig'
  AsyncInferenceOutputConfig ->
  AsyncInferenceConfig
newAsyncInferenceConfig pOutputConfig_ =
  AsyncInferenceConfig'
    { clientConfig =
        Prelude.Nothing,
      outputConfig = pOutputConfig_
    }

-- | Configures the behavior of the client used by SageMaker to interact with
-- the model container during asynchronous inference.
asyncInferenceConfig_clientConfig :: Lens.Lens' AsyncInferenceConfig (Prelude.Maybe AsyncInferenceClientConfig)
asyncInferenceConfig_clientConfig = Lens.lens (\AsyncInferenceConfig' {clientConfig} -> clientConfig) (\s@AsyncInferenceConfig' {} a -> s {clientConfig = a} :: AsyncInferenceConfig)

-- | Specifies the configuration for asynchronous inference invocation
-- outputs.
asyncInferenceConfig_outputConfig :: Lens.Lens' AsyncInferenceConfig AsyncInferenceOutputConfig
asyncInferenceConfig_outputConfig = Lens.lens (\AsyncInferenceConfig' {outputConfig} -> outputConfig) (\s@AsyncInferenceConfig' {} a -> s {outputConfig = a} :: AsyncInferenceConfig)

instance Data.FromJSON AsyncInferenceConfig where
  parseJSON =
    Data.withObject
      "AsyncInferenceConfig"
      ( \x ->
          AsyncInferenceConfig'
            Prelude.<$> (x Data..:? "ClientConfig")
            Prelude.<*> (x Data..: "OutputConfig")
      )

instance Prelude.Hashable AsyncInferenceConfig where
  hashWithSalt _salt AsyncInferenceConfig' {..} =
    _salt
      `Prelude.hashWithSalt` clientConfig
      `Prelude.hashWithSalt` outputConfig

instance Prelude.NFData AsyncInferenceConfig where
  rnf AsyncInferenceConfig' {..} =
    Prelude.rnf clientConfig
      `Prelude.seq` Prelude.rnf outputConfig

instance Data.ToJSON AsyncInferenceConfig where
  toJSON AsyncInferenceConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientConfig" Data..=) Prelude.<$> clientConfig,
            Prelude.Just ("OutputConfig" Data..= outputConfig)
          ]
      )
