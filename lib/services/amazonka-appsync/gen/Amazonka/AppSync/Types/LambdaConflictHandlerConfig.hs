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
-- Module      : Amazonka.AppSync.Types.LambdaConflictHandlerConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.LambdaConflictHandlerConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The @LambdaConflictHandlerConfig@ object when configuring @LAMBDA@ as
-- the Conflict Handler.
--
-- /See:/ 'newLambdaConflictHandlerConfig' smart constructor.
data LambdaConflictHandlerConfig = LambdaConflictHandlerConfig'
  { -- | The Amazon Resource Name (ARN) for the Lambda function to use as the
    -- Conflict Handler.
    lambdaConflictHandlerArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaConflictHandlerConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lambdaConflictHandlerArn', 'lambdaConflictHandlerConfig_lambdaConflictHandlerArn' - The Amazon Resource Name (ARN) for the Lambda function to use as the
-- Conflict Handler.
newLambdaConflictHandlerConfig ::
  LambdaConflictHandlerConfig
newLambdaConflictHandlerConfig =
  LambdaConflictHandlerConfig'
    { lambdaConflictHandlerArn =
        Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) for the Lambda function to use as the
-- Conflict Handler.
lambdaConflictHandlerConfig_lambdaConflictHandlerArn :: Lens.Lens' LambdaConflictHandlerConfig (Prelude.Maybe Prelude.Text)
lambdaConflictHandlerConfig_lambdaConflictHandlerArn = Lens.lens (\LambdaConflictHandlerConfig' {lambdaConflictHandlerArn} -> lambdaConflictHandlerArn) (\s@LambdaConflictHandlerConfig' {} a -> s {lambdaConflictHandlerArn = a} :: LambdaConflictHandlerConfig)

instance Data.FromJSON LambdaConflictHandlerConfig where
  parseJSON =
    Data.withObject
      "LambdaConflictHandlerConfig"
      ( \x ->
          LambdaConflictHandlerConfig'
            Prelude.<$> (x Data..:? "lambdaConflictHandlerArn")
      )

instance Prelude.Hashable LambdaConflictHandlerConfig where
  hashWithSalt _salt LambdaConflictHandlerConfig' {..} =
    _salt
      `Prelude.hashWithSalt` lambdaConflictHandlerArn

instance Prelude.NFData LambdaConflictHandlerConfig where
  rnf LambdaConflictHandlerConfig' {..} =
    Prelude.rnf lambdaConflictHandlerArn

instance Data.ToJSON LambdaConflictHandlerConfig where
  toJSON LambdaConflictHandlerConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("lambdaConflictHandlerArn" Data..=)
              Prelude.<$> lambdaConflictHandlerArn
          ]
      )
