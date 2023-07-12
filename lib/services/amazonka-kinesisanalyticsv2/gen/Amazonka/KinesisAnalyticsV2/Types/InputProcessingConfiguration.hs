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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.InputProcessingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.InputProcessingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types.InputLambdaProcessor
import qualified Amazonka.Prelude as Prelude

-- | For a SQL-based Kinesis Data Analytics application, describes a
-- processor that is used to preprocess the records in the stream before
-- being processed by your application code. Currently, the only input
-- processor available is
-- <https://docs.aws.amazon.com/lambda/ Amazon Lambda>.
--
-- /See:/ 'newInputProcessingConfiguration' smart constructor.
data InputProcessingConfiguration = InputProcessingConfiguration'
  { -- | The InputLambdaProcessor that is used to preprocess the records in the
    -- stream before being processed by your application code.
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
-- 'inputLambdaProcessor', 'inputProcessingConfiguration_inputLambdaProcessor' - The InputLambdaProcessor that is used to preprocess the records in the
-- stream before being processed by your application code.
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

-- | The InputLambdaProcessor that is used to preprocess the records in the
-- stream before being processed by your application code.
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
