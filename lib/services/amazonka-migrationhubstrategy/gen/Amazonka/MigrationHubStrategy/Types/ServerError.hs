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
-- Module      : Amazonka.MigrationHubStrategy.Types.ServerError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.ServerError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.ServerErrorCategory
import qualified Amazonka.Prelude as Prelude

-- | The error in server analysis.
--
-- /See:/ 'newServerError' smart constructor.
data ServerError = ServerError'
  { -- | The error category of server analysis.
    serverErrorCategory :: Prelude.Maybe ServerErrorCategory
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServerError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverErrorCategory', 'serverError_serverErrorCategory' - The error category of server analysis.
newServerError ::
  ServerError
newServerError =
  ServerError' {serverErrorCategory = Prelude.Nothing}

-- | The error category of server analysis.
serverError_serverErrorCategory :: Lens.Lens' ServerError (Prelude.Maybe ServerErrorCategory)
serverError_serverErrorCategory = Lens.lens (\ServerError' {serverErrorCategory} -> serverErrorCategory) (\s@ServerError' {} a -> s {serverErrorCategory = a} :: ServerError)

instance Data.FromJSON ServerError where
  parseJSON =
    Data.withObject
      "ServerError"
      ( \x ->
          ServerError'
            Prelude.<$> (x Data..:? "serverErrorCategory")
      )

instance Prelude.Hashable ServerError where
  hashWithSalt _salt ServerError' {..} =
    _salt `Prelude.hashWithSalt` serverErrorCategory

instance Prelude.NFData ServerError where
  rnf ServerError' {..} =
    Prelude.rnf serverErrorCategory
