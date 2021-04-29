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
-- Module      : Network.AWS.AppSync.Types.LambdaConflictHandlerConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.LambdaConflictHandlerConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The @LambdaConflictHandlerConfig@ object when configuring LAMBDA as the
-- Conflict Handler.
--
-- /See:/ 'newLambdaConflictHandlerConfig' smart constructor.
data LambdaConflictHandlerConfig = LambdaConflictHandlerConfig'
  { -- | The Arn for the Lambda function to use as the Conflict Handler.
    lambdaConflictHandlerArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LambdaConflictHandlerConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lambdaConflictHandlerArn', 'lambdaConflictHandlerConfig_lambdaConflictHandlerArn' - The Arn for the Lambda function to use as the Conflict Handler.
newLambdaConflictHandlerConfig ::
  LambdaConflictHandlerConfig
newLambdaConflictHandlerConfig =
  LambdaConflictHandlerConfig'
    { lambdaConflictHandlerArn =
        Prelude.Nothing
    }

-- | The Arn for the Lambda function to use as the Conflict Handler.
lambdaConflictHandlerConfig_lambdaConflictHandlerArn :: Lens.Lens' LambdaConflictHandlerConfig (Prelude.Maybe Prelude.Text)
lambdaConflictHandlerConfig_lambdaConflictHandlerArn = Lens.lens (\LambdaConflictHandlerConfig' {lambdaConflictHandlerArn} -> lambdaConflictHandlerArn) (\s@LambdaConflictHandlerConfig' {} a -> s {lambdaConflictHandlerArn = a} :: LambdaConflictHandlerConfig)

instance Prelude.FromJSON LambdaConflictHandlerConfig where
  parseJSON =
    Prelude.withObject
      "LambdaConflictHandlerConfig"
      ( \x ->
          LambdaConflictHandlerConfig'
            Prelude.<$> (x Prelude..:? "lambdaConflictHandlerArn")
      )

instance Prelude.Hashable LambdaConflictHandlerConfig

instance Prelude.NFData LambdaConflictHandlerConfig

instance Prelude.ToJSON LambdaConflictHandlerConfig where
  toJSON LambdaConflictHandlerConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("lambdaConflictHandlerArn" Prelude..=)
              Prelude.<$> lambdaConflictHandlerArn
          ]
      )
