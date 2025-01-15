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
-- Module      : Amazonka.SecurityHub.Types.AwsLambdaFunctionEnvironment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsLambdaFunctionEnvironment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsLambdaFunctionEnvironmentError

-- | A function\'s environment variable settings.
--
-- /See:/ 'newAwsLambdaFunctionEnvironment' smart constructor.
data AwsLambdaFunctionEnvironment = AwsLambdaFunctionEnvironment'
  { -- | An @AwsLambdaFunctionEnvironmentError@ object.
    error :: Prelude.Maybe AwsLambdaFunctionEnvironmentError,
    -- | Environment variable key-value pairs.
    variables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsLambdaFunctionEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'awsLambdaFunctionEnvironment_error' - An @AwsLambdaFunctionEnvironmentError@ object.
--
-- 'variables', 'awsLambdaFunctionEnvironment_variables' - Environment variable key-value pairs.
newAwsLambdaFunctionEnvironment ::
  AwsLambdaFunctionEnvironment
newAwsLambdaFunctionEnvironment =
  AwsLambdaFunctionEnvironment'
    { error =
        Prelude.Nothing,
      variables = Prelude.Nothing
    }

-- | An @AwsLambdaFunctionEnvironmentError@ object.
awsLambdaFunctionEnvironment_error :: Lens.Lens' AwsLambdaFunctionEnvironment (Prelude.Maybe AwsLambdaFunctionEnvironmentError)
awsLambdaFunctionEnvironment_error = Lens.lens (\AwsLambdaFunctionEnvironment' {error} -> error) (\s@AwsLambdaFunctionEnvironment' {} a -> s {error = a} :: AwsLambdaFunctionEnvironment)

-- | Environment variable key-value pairs.
awsLambdaFunctionEnvironment_variables :: Lens.Lens' AwsLambdaFunctionEnvironment (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
awsLambdaFunctionEnvironment_variables = Lens.lens (\AwsLambdaFunctionEnvironment' {variables} -> variables) (\s@AwsLambdaFunctionEnvironment' {} a -> s {variables = a} :: AwsLambdaFunctionEnvironment) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AwsLambdaFunctionEnvironment where
  parseJSON =
    Data.withObject
      "AwsLambdaFunctionEnvironment"
      ( \x ->
          AwsLambdaFunctionEnvironment'
            Prelude.<$> (x Data..:? "Error")
            Prelude.<*> (x Data..:? "Variables" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    AwsLambdaFunctionEnvironment
  where
  hashWithSalt _salt AwsLambdaFunctionEnvironment' {..} =
    _salt
      `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` variables

instance Prelude.NFData AwsLambdaFunctionEnvironment where
  rnf AwsLambdaFunctionEnvironment' {..} =
    Prelude.rnf error `Prelude.seq`
      Prelude.rnf variables

instance Data.ToJSON AwsLambdaFunctionEnvironment where
  toJSON AwsLambdaFunctionEnvironment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Error" Data..=) Prelude.<$> error,
            ("Variables" Data..=) Prelude.<$> variables
          ]
      )
