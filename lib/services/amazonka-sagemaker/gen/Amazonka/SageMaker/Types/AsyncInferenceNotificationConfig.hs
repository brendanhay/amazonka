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
-- Module      : Amazonka.SageMaker.Types.AsyncInferenceNotificationConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AsyncInferenceNotificationConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AsyncNotificationTopicTypes

-- | Specifies the configuration for notifications of inference results for
-- asynchronous inference.
--
-- /See:/ 'newAsyncInferenceNotificationConfig' smart constructor.
data AsyncInferenceNotificationConfig = AsyncInferenceNotificationConfig'
  { -- | Amazon SNS topic to post a notification to when inference fails. If no
    -- topic is provided, no notification is sent on failure.
    errorTopic :: Prelude.Maybe Prelude.Text,
    -- | The Amazon SNS topics where you want the inference response to be
    -- included.
    --
    -- The inference response is included only if the response size is less
    -- than or equal to 128 KB.
    includeInferenceResponseIn :: Prelude.Maybe [AsyncNotificationTopicTypes],
    -- | Amazon SNS topic to post a notification to when inference completes
    -- successfully. If no topic is provided, no notification is sent on
    -- success.
    successTopic :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AsyncInferenceNotificationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorTopic', 'asyncInferenceNotificationConfig_errorTopic' - Amazon SNS topic to post a notification to when inference fails. If no
-- topic is provided, no notification is sent on failure.
--
-- 'includeInferenceResponseIn', 'asyncInferenceNotificationConfig_includeInferenceResponseIn' - The Amazon SNS topics where you want the inference response to be
-- included.
--
-- The inference response is included only if the response size is less
-- than or equal to 128 KB.
--
-- 'successTopic', 'asyncInferenceNotificationConfig_successTopic' - Amazon SNS topic to post a notification to when inference completes
-- successfully. If no topic is provided, no notification is sent on
-- success.
newAsyncInferenceNotificationConfig ::
  AsyncInferenceNotificationConfig
newAsyncInferenceNotificationConfig =
  AsyncInferenceNotificationConfig'
    { errorTopic =
        Prelude.Nothing,
      includeInferenceResponseIn =
        Prelude.Nothing,
      successTopic = Prelude.Nothing
    }

-- | Amazon SNS topic to post a notification to when inference fails. If no
-- topic is provided, no notification is sent on failure.
asyncInferenceNotificationConfig_errorTopic :: Lens.Lens' AsyncInferenceNotificationConfig (Prelude.Maybe Prelude.Text)
asyncInferenceNotificationConfig_errorTopic = Lens.lens (\AsyncInferenceNotificationConfig' {errorTopic} -> errorTopic) (\s@AsyncInferenceNotificationConfig' {} a -> s {errorTopic = a} :: AsyncInferenceNotificationConfig)

-- | The Amazon SNS topics where you want the inference response to be
-- included.
--
-- The inference response is included only if the response size is less
-- than or equal to 128 KB.
asyncInferenceNotificationConfig_includeInferenceResponseIn :: Lens.Lens' AsyncInferenceNotificationConfig (Prelude.Maybe [AsyncNotificationTopicTypes])
asyncInferenceNotificationConfig_includeInferenceResponseIn = Lens.lens (\AsyncInferenceNotificationConfig' {includeInferenceResponseIn} -> includeInferenceResponseIn) (\s@AsyncInferenceNotificationConfig' {} a -> s {includeInferenceResponseIn = a} :: AsyncInferenceNotificationConfig) Prelude.. Lens.mapping Lens.coerced

-- | Amazon SNS topic to post a notification to when inference completes
-- successfully. If no topic is provided, no notification is sent on
-- success.
asyncInferenceNotificationConfig_successTopic :: Lens.Lens' AsyncInferenceNotificationConfig (Prelude.Maybe Prelude.Text)
asyncInferenceNotificationConfig_successTopic = Lens.lens (\AsyncInferenceNotificationConfig' {successTopic} -> successTopic) (\s@AsyncInferenceNotificationConfig' {} a -> s {successTopic = a} :: AsyncInferenceNotificationConfig)

instance
  Data.FromJSON
    AsyncInferenceNotificationConfig
  where
  parseJSON =
    Data.withObject
      "AsyncInferenceNotificationConfig"
      ( \x ->
          AsyncInferenceNotificationConfig'
            Prelude.<$> (x Data..:? "ErrorTopic")
            Prelude.<*> ( x
                            Data..:? "IncludeInferenceResponseIn"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "SuccessTopic")
      )

instance
  Prelude.Hashable
    AsyncInferenceNotificationConfig
  where
  hashWithSalt
    _salt
    AsyncInferenceNotificationConfig' {..} =
      _salt
        `Prelude.hashWithSalt` errorTopic
        `Prelude.hashWithSalt` includeInferenceResponseIn
        `Prelude.hashWithSalt` successTopic

instance
  Prelude.NFData
    AsyncInferenceNotificationConfig
  where
  rnf AsyncInferenceNotificationConfig' {..} =
    Prelude.rnf errorTopic
      `Prelude.seq` Prelude.rnf includeInferenceResponseIn
      `Prelude.seq` Prelude.rnf successTopic

instance Data.ToJSON AsyncInferenceNotificationConfig where
  toJSON AsyncInferenceNotificationConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ErrorTopic" Data..=) Prelude.<$> errorTopic,
            ("IncludeInferenceResponseIn" Data..=)
              Prelude.<$> includeInferenceResponseIn,
            ("SuccessTopic" Data..=) Prelude.<$> successTopic
          ]
      )
