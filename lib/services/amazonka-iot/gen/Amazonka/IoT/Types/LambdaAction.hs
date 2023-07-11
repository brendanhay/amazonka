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
-- Module      : Amazonka.IoT.Types.LambdaAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.LambdaAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an action to invoke a Lambda function.
--
-- /See:/ 'newLambdaAction' smart constructor.
data LambdaAction = LambdaAction'
  { -- | The ARN of the Lambda function.
    functionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'functionArn', 'lambdaAction_functionArn' - The ARN of the Lambda function.
newLambdaAction ::
  -- | 'functionArn'
  Prelude.Text ->
  LambdaAction
newLambdaAction pFunctionArn_ =
  LambdaAction' {functionArn = pFunctionArn_}

-- | The ARN of the Lambda function.
lambdaAction_functionArn :: Lens.Lens' LambdaAction Prelude.Text
lambdaAction_functionArn = Lens.lens (\LambdaAction' {functionArn} -> functionArn) (\s@LambdaAction' {} a -> s {functionArn = a} :: LambdaAction)

instance Data.FromJSON LambdaAction where
  parseJSON =
    Data.withObject
      "LambdaAction"
      ( \x ->
          LambdaAction' Prelude.<$> (x Data..: "functionArn")
      )

instance Prelude.Hashable LambdaAction where
  hashWithSalt _salt LambdaAction' {..} =
    _salt `Prelude.hashWithSalt` functionArn

instance Prelude.NFData LambdaAction where
  rnf LambdaAction' {..} = Prelude.rnf functionArn

instance Data.ToJSON LambdaAction where
  toJSON LambdaAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("functionArn" Data..= functionArn)]
      )
