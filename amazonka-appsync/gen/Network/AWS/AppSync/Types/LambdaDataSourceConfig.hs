{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.AppSync.Types.LambdaDataSourceConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.LambdaDataSourceConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an AWS Lambda data source configuration.
--
-- /See:/ 'newLambdaDataSourceConfig' smart constructor.
data LambdaDataSourceConfig = LambdaDataSourceConfig'
  { -- | The ARN for the Lambda function.
    lambdaFunctionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LambdaDataSourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lambdaFunctionArn', 'lambdaDataSourceConfig_lambdaFunctionArn' - The ARN for the Lambda function.
newLambdaDataSourceConfig ::
  -- | 'lambdaFunctionArn'
  Prelude.Text ->
  LambdaDataSourceConfig
newLambdaDataSourceConfig pLambdaFunctionArn_ =
  LambdaDataSourceConfig'
    { lambdaFunctionArn =
        pLambdaFunctionArn_
    }

-- | The ARN for the Lambda function.
lambdaDataSourceConfig_lambdaFunctionArn :: Lens.Lens' LambdaDataSourceConfig Prelude.Text
lambdaDataSourceConfig_lambdaFunctionArn = Lens.lens (\LambdaDataSourceConfig' {lambdaFunctionArn} -> lambdaFunctionArn) (\s@LambdaDataSourceConfig' {} a -> s {lambdaFunctionArn = a} :: LambdaDataSourceConfig)

instance Prelude.FromJSON LambdaDataSourceConfig where
  parseJSON =
    Prelude.withObject
      "LambdaDataSourceConfig"
      ( \x ->
          LambdaDataSourceConfig'
            Prelude.<$> (x Prelude..: "lambdaFunctionArn")
      )

instance Prelude.Hashable LambdaDataSourceConfig

instance Prelude.NFData LambdaDataSourceConfig

instance Prelude.ToJSON LambdaDataSourceConfig where
  toJSON LambdaDataSourceConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("lambdaFunctionArn" Prelude..= lambdaFunctionArn)
          ]
      )
