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
-- Module      : Amazonka.SecurityHub.Types.AwsLambdaFunctionEnvironmentError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsLambdaFunctionEnvironmentError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Error messages for environment variables that could not be applied.
--
-- /See:/ 'newAwsLambdaFunctionEnvironmentError' smart constructor.
data AwsLambdaFunctionEnvironmentError = AwsLambdaFunctionEnvironmentError'
  { -- | The error message.
    message :: Prelude.Maybe Prelude.Text,
    -- | The error code.
    errorCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsLambdaFunctionEnvironmentError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'awsLambdaFunctionEnvironmentError_message' - The error message.
--
-- 'errorCode', 'awsLambdaFunctionEnvironmentError_errorCode' - The error code.
newAwsLambdaFunctionEnvironmentError ::
  AwsLambdaFunctionEnvironmentError
newAwsLambdaFunctionEnvironmentError =
  AwsLambdaFunctionEnvironmentError'
    { message =
        Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | The error message.
awsLambdaFunctionEnvironmentError_message :: Lens.Lens' AwsLambdaFunctionEnvironmentError (Prelude.Maybe Prelude.Text)
awsLambdaFunctionEnvironmentError_message = Lens.lens (\AwsLambdaFunctionEnvironmentError' {message} -> message) (\s@AwsLambdaFunctionEnvironmentError' {} a -> s {message = a} :: AwsLambdaFunctionEnvironmentError)

-- | The error code.
awsLambdaFunctionEnvironmentError_errorCode :: Lens.Lens' AwsLambdaFunctionEnvironmentError (Prelude.Maybe Prelude.Text)
awsLambdaFunctionEnvironmentError_errorCode = Lens.lens (\AwsLambdaFunctionEnvironmentError' {errorCode} -> errorCode) (\s@AwsLambdaFunctionEnvironmentError' {} a -> s {errorCode = a} :: AwsLambdaFunctionEnvironmentError)

instance
  Core.FromJSON
    AwsLambdaFunctionEnvironmentError
  where
  parseJSON =
    Core.withObject
      "AwsLambdaFunctionEnvironmentError"
      ( \x ->
          AwsLambdaFunctionEnvironmentError'
            Prelude.<$> (x Core..:? "Message")
            Prelude.<*> (x Core..:? "ErrorCode")
      )

instance
  Prelude.Hashable
    AwsLambdaFunctionEnvironmentError
  where
  hashWithSalt
    _salt
    AwsLambdaFunctionEnvironmentError' {..} =
      _salt `Prelude.hashWithSalt` message
        `Prelude.hashWithSalt` errorCode

instance
  Prelude.NFData
    AwsLambdaFunctionEnvironmentError
  where
  rnf AwsLambdaFunctionEnvironmentError' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf errorCode

instance
  Core.ToJSON
    AwsLambdaFunctionEnvironmentError
  where
  toJSON AwsLambdaFunctionEnvironmentError' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Message" Core..=) Prelude.<$> message,
            ("ErrorCode" Core..=) Prelude.<$> errorCode
          ]
      )
