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
-- Module      : Network.AWS.StepFunctions.Types.LambdaFunctionSucceededEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.LambdaFunctionSucceededEventDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails

-- | Contains details about a lambda function that successfully terminated
-- during an execution.
--
-- /See:/ 'newLambdaFunctionSucceededEventDetails' smart constructor.
data LambdaFunctionSucceededEventDetails = LambdaFunctionSucceededEventDetails'
  { -- | The JSON data output by the lambda function. Length constraints apply to
    -- the payload size, and are expressed as bytes in UTF-8 encoding.
    output :: Core.Maybe (Core.Sensitive Core.Text),
    -- | Contains details about the output of an execution history event.
    outputDetails :: Core.Maybe HistoryEventExecutionDataDetails
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'LambdaFunctionSucceededEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'output', 'lambdaFunctionSucceededEventDetails_output' - The JSON data output by the lambda function. Length constraints apply to
-- the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- 'outputDetails', 'lambdaFunctionSucceededEventDetails_outputDetails' - Contains details about the output of an execution history event.
newLambdaFunctionSucceededEventDetails ::
  LambdaFunctionSucceededEventDetails
newLambdaFunctionSucceededEventDetails =
  LambdaFunctionSucceededEventDetails'
    { output =
        Core.Nothing,
      outputDetails = Core.Nothing
    }

-- | The JSON data output by the lambda function. Length constraints apply to
-- the payload size, and are expressed as bytes in UTF-8 encoding.
lambdaFunctionSucceededEventDetails_output :: Lens.Lens' LambdaFunctionSucceededEventDetails (Core.Maybe Core.Text)
lambdaFunctionSucceededEventDetails_output = Lens.lens (\LambdaFunctionSucceededEventDetails' {output} -> output) (\s@LambdaFunctionSucceededEventDetails' {} a -> s {output = a} :: LambdaFunctionSucceededEventDetails) Core.. Lens.mapping Core._Sensitive

-- | Contains details about the output of an execution history event.
lambdaFunctionSucceededEventDetails_outputDetails :: Lens.Lens' LambdaFunctionSucceededEventDetails (Core.Maybe HistoryEventExecutionDataDetails)
lambdaFunctionSucceededEventDetails_outputDetails = Lens.lens (\LambdaFunctionSucceededEventDetails' {outputDetails} -> outputDetails) (\s@LambdaFunctionSucceededEventDetails' {} a -> s {outputDetails = a} :: LambdaFunctionSucceededEventDetails)

instance
  Core.FromJSON
    LambdaFunctionSucceededEventDetails
  where
  parseJSON =
    Core.withObject
      "LambdaFunctionSucceededEventDetails"
      ( \x ->
          LambdaFunctionSucceededEventDetails'
            Core.<$> (x Core..:? "output")
            Core.<*> (x Core..:? "outputDetails")
      )

instance
  Core.Hashable
    LambdaFunctionSucceededEventDetails

instance
  Core.NFData
    LambdaFunctionSucceededEventDetails
