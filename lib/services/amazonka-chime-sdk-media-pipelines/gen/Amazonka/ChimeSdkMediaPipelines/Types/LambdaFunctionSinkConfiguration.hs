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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.LambdaFunctionSinkConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.LambdaFunctionSinkConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains the configuration settings for an AWS Lambda
-- function\'s data sink.
--
-- /See:/ 'newLambdaFunctionSinkConfiguration' smart constructor.
data LambdaFunctionSinkConfiguration = LambdaFunctionSinkConfiguration'
  { -- | The ARN of the sink.
    insightsTarget :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaFunctionSinkConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'insightsTarget', 'lambdaFunctionSinkConfiguration_insightsTarget' - The ARN of the sink.
newLambdaFunctionSinkConfiguration ::
  LambdaFunctionSinkConfiguration
newLambdaFunctionSinkConfiguration =
  LambdaFunctionSinkConfiguration'
    { insightsTarget =
        Prelude.Nothing
    }

-- | The ARN of the sink.
lambdaFunctionSinkConfiguration_insightsTarget :: Lens.Lens' LambdaFunctionSinkConfiguration (Prelude.Maybe Prelude.Text)
lambdaFunctionSinkConfiguration_insightsTarget = Lens.lens (\LambdaFunctionSinkConfiguration' {insightsTarget} -> insightsTarget) (\s@LambdaFunctionSinkConfiguration' {} a -> s {insightsTarget = a} :: LambdaFunctionSinkConfiguration) Prelude.. Lens.mapping Data._Sensitive

instance
  Data.FromJSON
    LambdaFunctionSinkConfiguration
  where
  parseJSON =
    Data.withObject
      "LambdaFunctionSinkConfiguration"
      ( \x ->
          LambdaFunctionSinkConfiguration'
            Prelude.<$> (x Data..:? "InsightsTarget")
      )

instance
  Prelude.Hashable
    LambdaFunctionSinkConfiguration
  where
  hashWithSalt
    _salt
    LambdaFunctionSinkConfiguration' {..} =
      _salt `Prelude.hashWithSalt` insightsTarget

instance
  Prelude.NFData
    LambdaFunctionSinkConfiguration
  where
  rnf LambdaFunctionSinkConfiguration' {..} =
    Prelude.rnf insightsTarget

instance Data.ToJSON LambdaFunctionSinkConfiguration where
  toJSON LambdaFunctionSinkConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InsightsTarget" Data..=)
              Prelude.<$> insightsTarget
          ]
      )
