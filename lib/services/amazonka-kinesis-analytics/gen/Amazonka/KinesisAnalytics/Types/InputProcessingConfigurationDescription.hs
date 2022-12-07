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
-- Module      : Amazonka.KinesisAnalytics.Types.InputProcessingConfigurationDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalytics.Types.InputProcessingConfigurationDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalytics.Types.InputLambdaProcessorDescription
import qualified Amazonka.Prelude as Prelude

-- | Provides configuration information about an input processor. Currently,
-- the only input processor available is
-- <https://docs.aws.amazon.com/lambda/ AWS Lambda>.
--
-- /See:/ 'newInputProcessingConfigurationDescription' smart constructor.
data InputProcessingConfigurationDescription = InputProcessingConfigurationDescription'
  { -- | Provides configuration information about the associated
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessorDescription.html InputLambdaProcessorDescription>.
    inputLambdaProcessorDescription :: Prelude.Maybe InputLambdaProcessorDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputProcessingConfigurationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputLambdaProcessorDescription', 'inputProcessingConfigurationDescription_inputLambdaProcessorDescription' - Provides configuration information about the associated
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessorDescription.html InputLambdaProcessorDescription>.
newInputProcessingConfigurationDescription ::
  InputProcessingConfigurationDescription
newInputProcessingConfigurationDescription =
  InputProcessingConfigurationDescription'
    { inputLambdaProcessorDescription =
        Prelude.Nothing
    }

-- | Provides configuration information about the associated
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessorDescription.html InputLambdaProcessorDescription>.
inputProcessingConfigurationDescription_inputLambdaProcessorDescription :: Lens.Lens' InputProcessingConfigurationDescription (Prelude.Maybe InputLambdaProcessorDescription)
inputProcessingConfigurationDescription_inputLambdaProcessorDescription = Lens.lens (\InputProcessingConfigurationDescription' {inputLambdaProcessorDescription} -> inputLambdaProcessorDescription) (\s@InputProcessingConfigurationDescription' {} a -> s {inputLambdaProcessorDescription = a} :: InputProcessingConfigurationDescription)

instance
  Data.FromJSON
    InputProcessingConfigurationDescription
  where
  parseJSON =
    Data.withObject
      "InputProcessingConfigurationDescription"
      ( \x ->
          InputProcessingConfigurationDescription'
            Prelude.<$> (x Data..:? "InputLambdaProcessorDescription")
      )

instance
  Prelude.Hashable
    InputProcessingConfigurationDescription
  where
  hashWithSalt
    _salt
    InputProcessingConfigurationDescription' {..} =
      _salt
        `Prelude.hashWithSalt` inputLambdaProcessorDescription

instance
  Prelude.NFData
    InputProcessingConfigurationDescription
  where
  rnf InputProcessingConfigurationDescription' {..} =
    Prelude.rnf inputLambdaProcessorDescription
