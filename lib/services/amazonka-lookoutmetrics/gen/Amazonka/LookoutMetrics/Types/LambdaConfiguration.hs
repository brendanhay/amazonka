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
-- Module      : Amazonka.LookoutMetrics.Types.LambdaConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.LambdaConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a Lambda configuration.
--
-- /See:/ 'newLambdaConfiguration' smart constructor.
data LambdaConfiguration = LambdaConfiguration'
  { -- | The ARN of an IAM role that has permission to invoke the Lambda
    -- function.
    roleArn :: Prelude.Text,
    -- | The ARN of the Lambda function.
    lambdaArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'lambdaConfiguration_roleArn' - The ARN of an IAM role that has permission to invoke the Lambda
-- function.
--
-- 'lambdaArn', 'lambdaConfiguration_lambdaArn' - The ARN of the Lambda function.
newLambdaConfiguration ::
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'lambdaArn'
  Prelude.Text ->
  LambdaConfiguration
newLambdaConfiguration pRoleArn_ pLambdaArn_ =
  LambdaConfiguration'
    { roleArn = pRoleArn_,
      lambdaArn = pLambdaArn_
    }

-- | The ARN of an IAM role that has permission to invoke the Lambda
-- function.
lambdaConfiguration_roleArn :: Lens.Lens' LambdaConfiguration Prelude.Text
lambdaConfiguration_roleArn = Lens.lens (\LambdaConfiguration' {roleArn} -> roleArn) (\s@LambdaConfiguration' {} a -> s {roleArn = a} :: LambdaConfiguration)

-- | The ARN of the Lambda function.
lambdaConfiguration_lambdaArn :: Lens.Lens' LambdaConfiguration Prelude.Text
lambdaConfiguration_lambdaArn = Lens.lens (\LambdaConfiguration' {lambdaArn} -> lambdaArn) (\s@LambdaConfiguration' {} a -> s {lambdaArn = a} :: LambdaConfiguration)

instance Data.FromJSON LambdaConfiguration where
  parseJSON =
    Data.withObject
      "LambdaConfiguration"
      ( \x ->
          LambdaConfiguration'
            Prelude.<$> (x Data..: "RoleArn")
            Prelude.<*> (x Data..: "LambdaArn")
      )

instance Prelude.Hashable LambdaConfiguration where
  hashWithSalt _salt LambdaConfiguration' {..} =
    _salt `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` lambdaArn

instance Prelude.NFData LambdaConfiguration where
  rnf LambdaConfiguration' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf lambdaArn

instance Data.ToJSON LambdaConfiguration where
  toJSON LambdaConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("RoleArn" Data..= roleArn),
            Prelude.Just ("LambdaArn" Data..= lambdaArn)
          ]
      )
