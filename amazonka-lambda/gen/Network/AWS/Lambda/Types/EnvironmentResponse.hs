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

-- | The results of an operation to update or read environment variables. If
-- the operation is successful, the response contains the environment
-- variables. If it failed, the response contains details about the error.
--
-- /See:/ 'newEnvironmentResponse' smart constructor.
data EnvironmentResponse = EnvironmentResponse'
  { -- | Environment variable key-value pairs.
    variables :: Core.Maybe (Core.Sensitive (Core.HashMap Core.Text (Core.Sensitive Core.Text))),
    -- | Error messages for environment variables that couldn\'t be applied.
    error :: Core.Maybe EnvironmentError
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
    { variables = Core.Nothing,
      error = Core.Nothing
    }

-- | Environment variable key-value pairs.
environmentResponse_variables :: Lens.Lens' EnvironmentResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
environmentResponse_variables = Lens.lens (\EnvironmentResponse' {variables} -> variables) (\s@EnvironmentResponse' {} a -> s {variables = a} :: EnvironmentResponse) Core.. Lens.mapping (Core._Sensitive Core.. Lens._Coerce)

-- | Error messages for environment variables that couldn\'t be applied.
environmentResponse_error :: Lens.Lens' EnvironmentResponse (Core.Maybe EnvironmentError)
environmentResponse_error = Lens.lens (\EnvironmentResponse' {error} -> error) (\s@EnvironmentResponse' {} a -> s {error = a} :: EnvironmentResponse)

instance Core.FromJSON EnvironmentResponse where
  parseJSON =
    Core.withObject
      "EnvironmentResponse"
      ( \x ->
          EnvironmentResponse'
            Core.<$> (x Core..:? "Variables" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Error")
      )

instance Core.Hashable EnvironmentResponse

instance Core.NFData EnvironmentResponse
