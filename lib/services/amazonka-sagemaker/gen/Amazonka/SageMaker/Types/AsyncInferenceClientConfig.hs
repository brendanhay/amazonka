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
-- Module      : Amazonka.SageMaker.Types.AsyncInferenceClientConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AsyncInferenceClientConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configures the behavior of the client used by SageMaker to interact with
-- the model container during asynchronous inference.
--
-- /See:/ 'newAsyncInferenceClientConfig' smart constructor.
data AsyncInferenceClientConfig = AsyncInferenceClientConfig'
  { -- | The maximum number of concurrent requests sent by the SageMaker client
    -- to the model container. If no value is provided, SageMaker chooses an
    -- optimal value.
    maxConcurrentInvocationsPerInstance :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AsyncInferenceClientConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxConcurrentInvocationsPerInstance', 'asyncInferenceClientConfig_maxConcurrentInvocationsPerInstance' - The maximum number of concurrent requests sent by the SageMaker client
-- to the model container. If no value is provided, SageMaker chooses an
-- optimal value.
newAsyncInferenceClientConfig ::
  AsyncInferenceClientConfig
newAsyncInferenceClientConfig =
  AsyncInferenceClientConfig'
    { maxConcurrentInvocationsPerInstance =
        Prelude.Nothing
    }

-- | The maximum number of concurrent requests sent by the SageMaker client
-- to the model container. If no value is provided, SageMaker chooses an
-- optimal value.
asyncInferenceClientConfig_maxConcurrentInvocationsPerInstance :: Lens.Lens' AsyncInferenceClientConfig (Prelude.Maybe Prelude.Natural)
asyncInferenceClientConfig_maxConcurrentInvocationsPerInstance = Lens.lens (\AsyncInferenceClientConfig' {maxConcurrentInvocationsPerInstance} -> maxConcurrentInvocationsPerInstance) (\s@AsyncInferenceClientConfig' {} a -> s {maxConcurrentInvocationsPerInstance = a} :: AsyncInferenceClientConfig)

instance Data.FromJSON AsyncInferenceClientConfig where
  parseJSON =
    Data.withObject
      "AsyncInferenceClientConfig"
      ( \x ->
          AsyncInferenceClientConfig'
            Prelude.<$> (x Data..:? "MaxConcurrentInvocationsPerInstance")
      )

instance Prelude.Hashable AsyncInferenceClientConfig where
  hashWithSalt _salt AsyncInferenceClientConfig' {..} =
    _salt
      `Prelude.hashWithSalt` maxConcurrentInvocationsPerInstance

instance Prelude.NFData AsyncInferenceClientConfig where
  rnf AsyncInferenceClientConfig' {..} =
    Prelude.rnf maxConcurrentInvocationsPerInstance

instance Data.ToJSON AsyncInferenceClientConfig where
  toJSON AsyncInferenceClientConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxConcurrentInvocationsPerInstance" Data..=)
              Prelude.<$> maxConcurrentInvocationsPerInstance
          ]
      )
