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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AsyncInferenceNotificationConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the configuration for notifications of inference results for
-- asynchronous inference.
--
-- /See:/ 'newAsyncInferenceNotificationConfig' smart constructor.
data AsyncInferenceNotificationConfig = AsyncInferenceNotificationConfig'
  { -- | Amazon SNS topic to post a notification to when inference fails. If no
    -- topic is provided, no notification is sent on failure.
    errorTopic :: Prelude.Maybe Prelude.Text,
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
-- 'successTopic', 'asyncInferenceNotificationConfig_successTopic' - Amazon SNS topic to post a notification to when inference completes
-- successfully. If no topic is provided, no notification is sent on
-- success.
newAsyncInferenceNotificationConfig ::
  AsyncInferenceNotificationConfig
newAsyncInferenceNotificationConfig =
  AsyncInferenceNotificationConfig'
    { errorTopic =
        Prelude.Nothing,
      successTopic = Prelude.Nothing
    }

-- | Amazon SNS topic to post a notification to when inference fails. If no
-- topic is provided, no notification is sent on failure.
asyncInferenceNotificationConfig_errorTopic :: Lens.Lens' AsyncInferenceNotificationConfig (Prelude.Maybe Prelude.Text)
asyncInferenceNotificationConfig_errorTopic = Lens.lens (\AsyncInferenceNotificationConfig' {errorTopic} -> errorTopic) (\s@AsyncInferenceNotificationConfig' {} a -> s {errorTopic = a} :: AsyncInferenceNotificationConfig)

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
            Prelude.<*> (x Data..:? "SuccessTopic")
      )

instance
  Prelude.Hashable
    AsyncInferenceNotificationConfig
  where
  hashWithSalt
    _salt
    AsyncInferenceNotificationConfig' {..} =
      _salt `Prelude.hashWithSalt` errorTopic
        `Prelude.hashWithSalt` successTopic

instance
  Prelude.NFData
    AsyncInferenceNotificationConfig
  where
  rnf AsyncInferenceNotificationConfig' {..} =
    Prelude.rnf errorTopic
      `Prelude.seq` Prelude.rnf successTopic

instance Data.ToJSON AsyncInferenceNotificationConfig where
  toJSON AsyncInferenceNotificationConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ErrorTopic" Data..=) Prelude.<$> errorTopic,
            ("SuccessTopic" Data..=) Prelude.<$> successTopic
          ]
      )
