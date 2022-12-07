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
-- Module      : Amazonka.KinesisAnalytics.Types.InputProcessingConfigurationUpdate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalytics.Types.InputProcessingConfigurationUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalytics.Types.InputLambdaProcessorUpdate
import qualified Amazonka.Prelude as Prelude

-- | Describes updates to an
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputProcessingConfiguration.html InputProcessingConfiguration>.
--
-- /See:/ 'newInputProcessingConfigurationUpdate' smart constructor.
data InputProcessingConfigurationUpdate = InputProcessingConfigurationUpdate'
  { -- | Provides update information for an
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessor.html InputLambdaProcessor>.
    inputLambdaProcessorUpdate :: InputLambdaProcessorUpdate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputProcessingConfigurationUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputLambdaProcessorUpdate', 'inputProcessingConfigurationUpdate_inputLambdaProcessorUpdate' - Provides update information for an
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessor.html InputLambdaProcessor>.
newInputProcessingConfigurationUpdate ::
  -- | 'inputLambdaProcessorUpdate'
  InputLambdaProcessorUpdate ->
  InputProcessingConfigurationUpdate
newInputProcessingConfigurationUpdate
  pInputLambdaProcessorUpdate_ =
    InputProcessingConfigurationUpdate'
      { inputLambdaProcessorUpdate =
          pInputLambdaProcessorUpdate_
      }

-- | Provides update information for an
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessor.html InputLambdaProcessor>.
inputProcessingConfigurationUpdate_inputLambdaProcessorUpdate :: Lens.Lens' InputProcessingConfigurationUpdate InputLambdaProcessorUpdate
inputProcessingConfigurationUpdate_inputLambdaProcessorUpdate = Lens.lens (\InputProcessingConfigurationUpdate' {inputLambdaProcessorUpdate} -> inputLambdaProcessorUpdate) (\s@InputProcessingConfigurationUpdate' {} a -> s {inputLambdaProcessorUpdate = a} :: InputProcessingConfigurationUpdate)

instance
  Prelude.Hashable
    InputProcessingConfigurationUpdate
  where
  hashWithSalt
    _salt
    InputProcessingConfigurationUpdate' {..} =
      _salt
        `Prelude.hashWithSalt` inputLambdaProcessorUpdate

instance
  Prelude.NFData
    InputProcessingConfigurationUpdate
  where
  rnf InputProcessingConfigurationUpdate' {..} =
    Prelude.rnf inputLambdaProcessorUpdate

instance
  Data.ToJSON
    InputProcessingConfigurationUpdate
  where
  toJSON InputProcessingConfigurationUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "InputLambdaProcessorUpdate"
                  Data..= inputLambdaProcessorUpdate
              )
          ]
      )
