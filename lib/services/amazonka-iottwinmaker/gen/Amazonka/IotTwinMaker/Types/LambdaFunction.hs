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
-- Module      : Amazonka.IotTwinMaker.Types.LambdaFunction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.LambdaFunction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Lambda function.
--
-- /See:/ 'newLambdaFunction' smart constructor.
data LambdaFunction = LambdaFunction'
  { -- | The ARN of the Lambda function.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaFunction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'lambdaFunction_arn' - The ARN of the Lambda function.
newLambdaFunction ::
  -- | 'arn'
  Prelude.Text ->
  LambdaFunction
newLambdaFunction pArn_ =
  LambdaFunction' {arn = pArn_}

-- | The ARN of the Lambda function.
lambdaFunction_arn :: Lens.Lens' LambdaFunction Prelude.Text
lambdaFunction_arn = Lens.lens (\LambdaFunction' {arn} -> arn) (\s@LambdaFunction' {} a -> s {arn = a} :: LambdaFunction)

instance Data.FromJSON LambdaFunction where
  parseJSON =
    Data.withObject
      "LambdaFunction"
      ( \x ->
          LambdaFunction' Prelude.<$> (x Data..: "arn")
      )

instance Prelude.Hashable LambdaFunction where
  hashWithSalt _salt LambdaFunction' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData LambdaFunction where
  rnf LambdaFunction' {..} = Prelude.rnf arn

instance Data.ToJSON LambdaFunction where
  toJSON LambdaFunction' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Data..= arn)]
      )
