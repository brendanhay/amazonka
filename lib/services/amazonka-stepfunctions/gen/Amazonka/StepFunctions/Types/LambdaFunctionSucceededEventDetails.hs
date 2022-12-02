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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
  { -- | Contains details about the output of an execution history event.
    outputDetails :: Prelude.Maybe HistoryEventExecutionDataDetails,
    -- | The JSON data output by the Lambda function. Length constraints apply to
    -- the payload size, and are expressed as bytes in UTF-8 encoding.
    output :: Prelude.Maybe (Data.Sensitive Prelude.Text)
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
-- 'outputDetails', 'lambdaFunctionSucceededEventDetails_outputDetails' - Contains details about the output of an execution history event.
--
-- 'output', 'lambdaFunctionSucceededEventDetails_output' - The JSON data output by the Lambda function. Length constraints apply to
-- the payload size, and are expressed as bytes in UTF-8 encoding.
newLambdaFunctionSucceededEventDetails ::
  LambdaFunctionSucceededEventDetails
newLambdaFunctionSucceededEventDetails =
  LambdaFunctionSucceededEventDetails'
    { outputDetails =
        Prelude.Nothing,
      output = Prelude.Nothing
    }

-- | Contains details about the output of an execution history event.
lambdaFunctionSucceededEventDetails_outputDetails :: Lens.Lens' LambdaFunctionSucceededEventDetails (Prelude.Maybe HistoryEventExecutionDataDetails)
lambdaFunctionSucceededEventDetails_outputDetails = Lens.lens (\LambdaFunctionSucceededEventDetails' {outputDetails} -> outputDetails) (\s@LambdaFunctionSucceededEventDetails' {} a -> s {outputDetails = a} :: LambdaFunctionSucceededEventDetails)

-- | The JSON data output by the Lambda function. Length constraints apply to
-- the payload size, and are expressed as bytes in UTF-8 encoding.
lambdaFunctionSucceededEventDetails_output :: Lens.Lens' LambdaFunctionSucceededEventDetails (Prelude.Maybe Prelude.Text)
lambdaFunctionSucceededEventDetails_output = Lens.lens (\LambdaFunctionSucceededEventDetails' {output} -> output) (\s@LambdaFunctionSucceededEventDetails' {} a -> s {output = a} :: LambdaFunctionSucceededEventDetails) Prelude.. Lens.mapping Data._Sensitive

instance
  Data.FromJSON
    LambdaFunctionSucceededEventDetails
  where
  parseJSON =
    Data.withObject
      "LambdaFunctionSucceededEventDetails"
      ( \x ->
          LambdaFunctionSucceededEventDetails'
            Prelude.<$> (x Data..:? "outputDetails")
            Prelude.<*> (x Data..:? "output")
      )

instance
  Prelude.Hashable
    LambdaFunctionSucceededEventDetails
  where
  hashWithSalt
    _salt
    LambdaFunctionSucceededEventDetails' {..} =
      _salt `Prelude.hashWithSalt` outputDetails
        `Prelude.hashWithSalt` output

instance
  Prelude.NFData
    LambdaFunctionSucceededEventDetails
  where
  rnf LambdaFunctionSucceededEventDetails' {..} =
    Prelude.rnf outputDetails
      `Prelude.seq` Prelude.rnf output
