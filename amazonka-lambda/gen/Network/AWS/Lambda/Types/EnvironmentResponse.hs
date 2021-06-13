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
-- Module      : Network.AWS.Lambda.Types.EnvironmentResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.EnvironmentResponse where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types.EnvironmentError
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The results of an operation to update or read environment variables. If
-- the operation is successful, the response contains the environment
-- variables. If it failed, the response contains details about the error.
--
-- /See:/ 'newEnvironmentResponse' smart constructor.
data EnvironmentResponse = EnvironmentResponse'
  { -- | Environment variable key-value pairs.
    variables :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text (Core.Sensitive Prelude.Text))),
    -- | Error messages for environment variables that couldn\'t be applied.
    error :: Prelude.Maybe EnvironmentError
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
-- 'variables', 'environmentResponse_variables' - Environment variable key-value pairs.
--
-- 'error', 'environmentResponse_error' - Error messages for environment variables that couldn\'t be applied.
newEnvironmentResponse ::
  EnvironmentResponse
newEnvironmentResponse =
  EnvironmentResponse'
    { variables = Prelude.Nothing,
      error = Prelude.Nothing
    }

-- | Environment variable key-value pairs.
environmentResponse_variables :: Lens.Lens' EnvironmentResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
environmentResponse_variables = Lens.lens (\EnvironmentResponse' {variables} -> variables) (\s@EnvironmentResponse' {} a -> s {variables = a} :: EnvironmentResponse) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens._Coerce)

-- | Error messages for environment variables that couldn\'t be applied.
environmentResponse_error :: Lens.Lens' EnvironmentResponse (Prelude.Maybe EnvironmentError)
environmentResponse_error = Lens.lens (\EnvironmentResponse' {error} -> error) (\s@EnvironmentResponse' {} a -> s {error = a} :: EnvironmentResponse)

instance Core.FromJSON EnvironmentResponse where
  parseJSON =
    Core.withObject
      "EnvironmentResponse"
      ( \x ->
          EnvironmentResponse'
            Prelude.<$> (x Core..:? "Variables" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Error")
      )

instance Prelude.Hashable EnvironmentResponse

instance Prelude.NFData EnvironmentResponse
