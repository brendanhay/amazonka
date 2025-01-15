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
-- Module      : Amazonka.StepFunctions.Types.LambdaFunctionSucceededEventDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.LambdaFunctionSucceededEventDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.StepFunctions.Types.HistoryEventExecutionDataDetails

-- | Contains details about a Lambda function that successfully terminated
-- during an execution.
--
-- /See:/ 'newLambdaFunctionSucceededEventDetails' smart constructor.
data LambdaFunctionSucceededEventDetails = LambdaFunctionSucceededEventDetails'
  { -- | The JSON data output by the Lambda function. Length constraints apply to
    -- the payload size, and are expressed as bytes in UTF-8 encoding.
    output :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Contains details about the output of an execution history event.
    outputDetails :: Prelude.Maybe HistoryEventExecutionDataDetails
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaFunctionSucceededEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'output', 'lambdaFunctionSucceededEventDetails_output' - The JSON data output by the Lambda function. Length constraints apply to
-- the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- 'outputDetails', 'lambdaFunctionSucceededEventDetails_outputDetails' - Contains details about the output of an execution history event.
newLambdaFunctionSucceededEventDetails ::
  LambdaFunctionSucceededEventDetails
newLambdaFunctionSucceededEventDetails =
  LambdaFunctionSucceededEventDetails'
    { output =
        Prelude.Nothing,
      outputDetails = Prelude.Nothing
    }

-- | The JSON data output by the Lambda function. Length constraints apply to
-- the payload size, and are expressed as bytes in UTF-8 encoding.
lambdaFunctionSucceededEventDetails_output :: Lens.Lens' LambdaFunctionSucceededEventDetails (Prelude.Maybe Prelude.Text)
lambdaFunctionSucceededEventDetails_output = Lens.lens (\LambdaFunctionSucceededEventDetails' {output} -> output) (\s@LambdaFunctionSucceededEventDetails' {} a -> s {output = a} :: LambdaFunctionSucceededEventDetails) Prelude.. Lens.mapping Data._Sensitive

-- | Contains details about the output of an execution history event.
lambdaFunctionSucceededEventDetails_outputDetails :: Lens.Lens' LambdaFunctionSucceededEventDetails (Prelude.Maybe HistoryEventExecutionDataDetails)
lambdaFunctionSucceededEventDetails_outputDetails = Lens.lens (\LambdaFunctionSucceededEventDetails' {outputDetails} -> outputDetails) (\s@LambdaFunctionSucceededEventDetails' {} a -> s {outputDetails = a} :: LambdaFunctionSucceededEventDetails)

instance
  Data.FromJSON
    LambdaFunctionSucceededEventDetails
  where
  parseJSON =
    Data.withObject
      "LambdaFunctionSucceededEventDetails"
      ( \x ->
          LambdaFunctionSucceededEventDetails'
            Prelude.<$> (x Data..:? "output")
            Prelude.<*> (x Data..:? "outputDetails")
      )

instance
  Prelude.Hashable
    LambdaFunctionSucceededEventDetails
  where
  hashWithSalt
    _salt
    LambdaFunctionSucceededEventDetails' {..} =
      _salt
        `Prelude.hashWithSalt` output
        `Prelude.hashWithSalt` outputDetails

instance
  Prelude.NFData
    LambdaFunctionSucceededEventDetails
  where
  rnf LambdaFunctionSucceededEventDetails' {..} =
    Prelude.rnf output `Prelude.seq`
      Prelude.rnf outputDetails
