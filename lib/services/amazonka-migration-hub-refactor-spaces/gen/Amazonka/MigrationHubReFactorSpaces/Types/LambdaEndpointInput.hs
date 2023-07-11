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
-- Module      : Amazonka.MigrationHubReFactorSpaces.Types.LambdaEndpointInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubReFactorSpaces.Types.LambdaEndpointInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The input for the Lambda endpoint type.
--
-- /See:/ 'newLambdaEndpointInput' smart constructor.
data LambdaEndpointInput = LambdaEndpointInput'
  { -- | The Amazon Resource Name (ARN) of the Lambda function or alias.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaEndpointInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'lambdaEndpointInput_arn' - The Amazon Resource Name (ARN) of the Lambda function or alias.
newLambdaEndpointInput ::
  -- | 'arn'
  Prelude.Text ->
  LambdaEndpointInput
newLambdaEndpointInput pArn_ =
  LambdaEndpointInput' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the Lambda function or alias.
lambdaEndpointInput_arn :: Lens.Lens' LambdaEndpointInput Prelude.Text
lambdaEndpointInput_arn = Lens.lens (\LambdaEndpointInput' {arn} -> arn) (\s@LambdaEndpointInput' {} a -> s {arn = a} :: LambdaEndpointInput)

instance Data.FromJSON LambdaEndpointInput where
  parseJSON =
    Data.withObject
      "LambdaEndpointInput"
      ( \x ->
          LambdaEndpointInput' Prelude.<$> (x Data..: "Arn")
      )

instance Prelude.Hashable LambdaEndpointInput where
  hashWithSalt _salt LambdaEndpointInput' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData LambdaEndpointInput where
  rnf LambdaEndpointInput' {..} = Prelude.rnf arn

instance Data.ToJSON LambdaEndpointInput where
  toJSON LambdaEndpointInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Arn" Data..= arn)]
      )
