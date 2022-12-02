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
-- Module      : Amazonka.Lambda.Types.EnvironmentResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.EnvironmentResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types.EnvironmentError
import qualified Amazonka.Prelude as Prelude

-- | The results of an operation to update or read environment variables. If
-- the operation is successful, the response contains the environment
-- variables. If it failed, the response contains details about the error.
--
-- /See:/ 'newEnvironmentResponse' smart constructor.
data EnvironmentResponse = EnvironmentResponse'
  { -- | Error messages for environment variables that couldn\'t be applied.
    error :: Prelude.Maybe EnvironmentError,
    -- | Environment variable key-value pairs. Omitted from CloudTrail logs.
    variables :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text (Data.Sensitive Prelude.Text)))
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'environmentResponse_error' - Error messages for environment variables that couldn\'t be applied.
--
-- 'variables', 'environmentResponse_variables' - Environment variable key-value pairs. Omitted from CloudTrail logs.
newEnvironmentResponse ::
  EnvironmentResponse
newEnvironmentResponse =
  EnvironmentResponse'
    { error = Prelude.Nothing,
      variables = Prelude.Nothing
    }

-- | Error messages for environment variables that couldn\'t be applied.
environmentResponse_error :: Lens.Lens' EnvironmentResponse (Prelude.Maybe EnvironmentError)
environmentResponse_error = Lens.lens (\EnvironmentResponse' {error} -> error) (\s@EnvironmentResponse' {} a -> s {error = a} :: EnvironmentResponse)

-- | Environment variable key-value pairs. Omitted from CloudTrail logs.
environmentResponse_variables :: Lens.Lens' EnvironmentResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
environmentResponse_variables = Lens.lens (\EnvironmentResponse' {variables} -> variables) (\s@EnvironmentResponse' {} a -> s {variables = a} :: EnvironmentResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

instance Data.FromJSON EnvironmentResponse where
  parseJSON =
    Data.withObject
      "EnvironmentResponse"
      ( \x ->
          EnvironmentResponse'
            Prelude.<$> (x Data..:? "Error")
            Prelude.<*> (x Data..:? "Variables" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable EnvironmentResponse where
  hashWithSalt _salt EnvironmentResponse' {..} =
    _salt `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` variables

instance Prelude.NFData EnvironmentResponse where
  rnf EnvironmentResponse' {..} =
    Prelude.rnf error
      `Prelude.seq` Prelude.rnf variables
