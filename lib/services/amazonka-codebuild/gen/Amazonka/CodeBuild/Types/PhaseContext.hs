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
-- Module      : Amazonka.CodeBuild.Types.PhaseContext
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.PhaseContext where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Additional information about a build phase that has an error. You can
-- use this information for troubleshooting.
--
-- /See:/ 'newPhaseContext' smart constructor.
data PhaseContext = PhaseContext'
  { -- | An explanation of the build phase\'s context. This might include a
    -- command ID and an exit code.
    message :: Prelude.Maybe Prelude.Text,
    -- | The status code for the context of the build phase.
    statusCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PhaseContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'phaseContext_message' - An explanation of the build phase\'s context. This might include a
-- command ID and an exit code.
--
-- 'statusCode', 'phaseContext_statusCode' - The status code for the context of the build phase.
newPhaseContext ::
  PhaseContext
newPhaseContext =
  PhaseContext'
    { message = Prelude.Nothing,
      statusCode = Prelude.Nothing
    }

-- | An explanation of the build phase\'s context. This might include a
-- command ID and an exit code.
phaseContext_message :: Lens.Lens' PhaseContext (Prelude.Maybe Prelude.Text)
phaseContext_message = Lens.lens (\PhaseContext' {message} -> message) (\s@PhaseContext' {} a -> s {message = a} :: PhaseContext)

-- | The status code for the context of the build phase.
phaseContext_statusCode :: Lens.Lens' PhaseContext (Prelude.Maybe Prelude.Text)
phaseContext_statusCode = Lens.lens (\PhaseContext' {statusCode} -> statusCode) (\s@PhaseContext' {} a -> s {statusCode = a} :: PhaseContext)

instance Core.FromJSON PhaseContext where
  parseJSON =
    Core.withObject
      "PhaseContext"
      ( \x ->
          PhaseContext'
            Prelude.<$> (x Core..:? "message")
            Prelude.<*> (x Core..:? "statusCode")
      )

instance Prelude.Hashable PhaseContext where
  hashWithSalt _salt PhaseContext' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` statusCode

instance Prelude.NFData PhaseContext where
  rnf PhaseContext' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf statusCode
