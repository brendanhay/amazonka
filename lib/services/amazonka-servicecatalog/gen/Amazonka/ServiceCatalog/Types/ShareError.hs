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
-- Module      : Amazonka.ServiceCatalog.Types.ShareError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ShareError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Errors that occurred during the portfolio share operation.
--
-- /See:/ 'newShareError' smart constructor.
data ShareError = ShareError'
  { -- | Information about the error.
    message :: Prelude.Maybe Prelude.Text,
    -- | List of accounts impacted by the error.
    accounts :: Prelude.Maybe [Prelude.Text],
    -- | Error type that happened when processing the operation.
    error :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ShareError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'shareError_message' - Information about the error.
--
-- 'accounts', 'shareError_accounts' - List of accounts impacted by the error.
--
-- 'error', 'shareError_error' - Error type that happened when processing the operation.
newShareError ::
  ShareError
newShareError =
  ShareError'
    { message = Prelude.Nothing,
      accounts = Prelude.Nothing,
      error = Prelude.Nothing
    }

-- | Information about the error.
shareError_message :: Lens.Lens' ShareError (Prelude.Maybe Prelude.Text)
shareError_message = Lens.lens (\ShareError' {message} -> message) (\s@ShareError' {} a -> s {message = a} :: ShareError)

-- | List of accounts impacted by the error.
shareError_accounts :: Lens.Lens' ShareError (Prelude.Maybe [Prelude.Text])
shareError_accounts = Lens.lens (\ShareError' {accounts} -> accounts) (\s@ShareError' {} a -> s {accounts = a} :: ShareError) Prelude.. Lens.mapping Lens.coerced

-- | Error type that happened when processing the operation.
shareError_error :: Lens.Lens' ShareError (Prelude.Maybe Prelude.Text)
shareError_error = Lens.lens (\ShareError' {error} -> error) (\s@ShareError' {} a -> s {error = a} :: ShareError)

instance Data.FromJSON ShareError where
  parseJSON =
    Data.withObject
      "ShareError"
      ( \x ->
          ShareError'
            Prelude.<$> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "Accounts" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Error")
      )

instance Prelude.Hashable ShareError where
  hashWithSalt _salt ShareError' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` accounts
      `Prelude.hashWithSalt` error

instance Prelude.NFData ShareError where
  rnf ShareError' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf accounts
      `Prelude.seq` Prelude.rnf error
