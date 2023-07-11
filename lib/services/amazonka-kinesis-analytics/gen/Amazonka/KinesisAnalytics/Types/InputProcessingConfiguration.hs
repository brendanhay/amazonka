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
-- Module      : Amazonka.KinesisAnalytics.Types.InputProcessingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalytics.Types.InputProcessingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalytics.Types.InputLambdaProcessor
import qualified Amazonka.Prelude as Prelude

-- | Provides a description of a processor that is used to preprocess the
-- records in the stream before being processed by your application code.
-- Currently, the only input processor available is
-- <https://docs.aws.amazon.com/lambda/ AWS Lambda>.
--
-- /See:/ 'newInputProcessingConfiguration' smart constructor.
data InputProcessingConfiguration = InputProcessingConfiguration'
  { -- | The
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessor.html InputLambdaProcessor>
    -- that is used to preprocess the records in the stream before being
    -- processed by your application code.
    inputLambdaProcessor :: InputLambdaProcessor
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputProcessingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputLambdaProcessor', 'inputProcessingConfiguration_inputLambdaProcessor' - The
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessor.html InputLambdaProcessor>
-- that is used to preprocess the records in the stream before being
-- processed by your application code.
newInputProcessingConfiguration ::
  -- | 'inputLambdaProcessor'
  InputLambdaProcessor ->
  InputProcessingConfiguration
newInputProcessingConfiguration
  pInputLambdaProcessor_ =
    InputProcessingConfiguration'
      { inputLambdaProcessor =
          pInputLambdaProcessor_
      }

-- | The
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessor.html InputLambdaProcessor>
-- that is used to preprocess the records in the stream before being
-- processed by your application code.
inputProcessingConfiguration_inputLambdaProcessor :: Lens.Lens' InputProcessingConfiguration InputLambdaProcessor
inputProcessingConfiguration_inputLambdaProcessor = Lens.lens (\InputProcessingConfiguration' {inputLambdaProcessor} -> inputLambdaProcessor) (\s@InputProcessingConfiguration' {} a -> s {inputLambdaProcessor = a} :: InputProcessingConfiguration)

instance
  Prelude.Hashable
    InputProcessingConfiguration
  where
  hashWithSalt _salt InputProcessingConfiguration' {..} =
    _salt `Prelude.hashWithSalt` inputLambdaProcessor

instance Prelude.NFData InputProcessingConfiguration where
  rnf InputProcessingConfiguration' {..} =
    Prelude.rnf inputLambdaProcessor

instance Data.ToJSON InputProcessingConfiguration where
  toJSON InputProcessingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "InputLambdaProcessor"
                  Data..= inputLambdaProcessor
              )
          ]
      )
