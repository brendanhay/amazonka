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
-- Module      : Amazonka.AppSync.Types.LambdaDataSourceConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.LambdaDataSourceConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an Lambda data source configuration.
--
-- /See:/ 'newLambdaDataSourceConfig' smart constructor.
data LambdaDataSourceConfig = LambdaDataSourceConfig'
  { -- | The Amazon Resource Name (ARN) for the Lambda function.
    lambdaFunctionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaDataSourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lambdaFunctionArn', 'lambdaDataSourceConfig_lambdaFunctionArn' - The Amazon Resource Name (ARN) for the Lambda function.
newLambdaDataSourceConfig ::
  -- | 'lambdaFunctionArn'
  Prelude.Text ->
  LambdaDataSourceConfig
newLambdaDataSourceConfig pLambdaFunctionArn_ =
  LambdaDataSourceConfig'
    { lambdaFunctionArn =
        pLambdaFunctionArn_
    }

-- | The Amazon Resource Name (ARN) for the Lambda function.
lambdaDataSourceConfig_lambdaFunctionArn :: Lens.Lens' LambdaDataSourceConfig Prelude.Text
lambdaDataSourceConfig_lambdaFunctionArn = Lens.lens (\LambdaDataSourceConfig' {lambdaFunctionArn} -> lambdaFunctionArn) (\s@LambdaDataSourceConfig' {} a -> s {lambdaFunctionArn = a} :: LambdaDataSourceConfig)

instance Data.FromJSON LambdaDataSourceConfig where
  parseJSON =
    Data.withObject
      "LambdaDataSourceConfig"
      ( \x ->
          LambdaDataSourceConfig'
            Prelude.<$> (x Data..: "lambdaFunctionArn")
      )

instance Prelude.Hashable LambdaDataSourceConfig where
  hashWithSalt _salt LambdaDataSourceConfig' {..} =
    _salt `Prelude.hashWithSalt` lambdaFunctionArn

instance Prelude.NFData LambdaDataSourceConfig where
  rnf LambdaDataSourceConfig' {..} =
    Prelude.rnf lambdaFunctionArn

instance Data.ToJSON LambdaDataSourceConfig where
  toJSON LambdaDataSourceConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("lambdaFunctionArn" Data..= lambdaFunctionArn)
          ]
      )
