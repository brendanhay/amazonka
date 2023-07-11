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
-- Module      : Amazonka.SageMaker.Types.CallbackStepMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.CallbackStepMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.OutputParameter

-- | Metadata about a callback step.
--
-- /See:/ 'newCallbackStepMetadata' smart constructor.
data CallbackStepMetadata = CallbackStepMetadata'
  { -- | The pipeline generated token from the Amazon SQS queue.
    callbackToken :: Prelude.Maybe Prelude.Text,
    -- | A list of the output parameters of the callback step.
    outputParameters :: Prelude.Maybe [OutputParameter],
    -- | The URL of the Amazon Simple Queue Service (Amazon SQS) queue used by
    -- the callback step.
    sqsQueueUrl :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CallbackStepMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'callbackToken', 'callbackStepMetadata_callbackToken' - The pipeline generated token from the Amazon SQS queue.
--
-- 'outputParameters', 'callbackStepMetadata_outputParameters' - A list of the output parameters of the callback step.
--
-- 'sqsQueueUrl', 'callbackStepMetadata_sqsQueueUrl' - The URL of the Amazon Simple Queue Service (Amazon SQS) queue used by
-- the callback step.
newCallbackStepMetadata ::
  CallbackStepMetadata
newCallbackStepMetadata =
  CallbackStepMetadata'
    { callbackToken =
        Prelude.Nothing,
      outputParameters = Prelude.Nothing,
      sqsQueueUrl = Prelude.Nothing
    }

-- | The pipeline generated token from the Amazon SQS queue.
callbackStepMetadata_callbackToken :: Lens.Lens' CallbackStepMetadata (Prelude.Maybe Prelude.Text)
callbackStepMetadata_callbackToken = Lens.lens (\CallbackStepMetadata' {callbackToken} -> callbackToken) (\s@CallbackStepMetadata' {} a -> s {callbackToken = a} :: CallbackStepMetadata)

-- | A list of the output parameters of the callback step.
callbackStepMetadata_outputParameters :: Lens.Lens' CallbackStepMetadata (Prelude.Maybe [OutputParameter])
callbackStepMetadata_outputParameters = Lens.lens (\CallbackStepMetadata' {outputParameters} -> outputParameters) (\s@CallbackStepMetadata' {} a -> s {outputParameters = a} :: CallbackStepMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The URL of the Amazon Simple Queue Service (Amazon SQS) queue used by
-- the callback step.
callbackStepMetadata_sqsQueueUrl :: Lens.Lens' CallbackStepMetadata (Prelude.Maybe Prelude.Text)
callbackStepMetadata_sqsQueueUrl = Lens.lens (\CallbackStepMetadata' {sqsQueueUrl} -> sqsQueueUrl) (\s@CallbackStepMetadata' {} a -> s {sqsQueueUrl = a} :: CallbackStepMetadata)

instance Data.FromJSON CallbackStepMetadata where
  parseJSON =
    Data.withObject
      "CallbackStepMetadata"
      ( \x ->
          CallbackStepMetadata'
            Prelude.<$> (x Data..:? "CallbackToken")
            Prelude.<*> ( x
                            Data..:? "OutputParameters"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "SqsQueueUrl")
      )

instance Prelude.Hashable CallbackStepMetadata where
  hashWithSalt _salt CallbackStepMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` callbackToken
      `Prelude.hashWithSalt` outputParameters
      `Prelude.hashWithSalt` sqsQueueUrl

instance Prelude.NFData CallbackStepMetadata where
  rnf CallbackStepMetadata' {..} =
    Prelude.rnf callbackToken
      `Prelude.seq` Prelude.rnf outputParameters
      `Prelude.seq` Prelude.rnf sqsQueueUrl
