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
-- Module      : Network.AWS.ServiceCatalog.Types.ShareError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ShareError where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Errors that occurred during the portfolio share operation.
--
-- /See:/ 'newShareError' smart constructor.
data ShareError = ShareError'
  { -- | Information about the error.
    message :: Core.Maybe Core.Text,
    -- | List of accounts impacted by the error.
    accounts :: Core.Maybe [Core.Text],
    -- | Error type that happened when processing the operation.
    error :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { message = Core.Nothing,
      accounts = Core.Nothing,
      error = Core.Nothing
    }

-- | Information about the error.
shareError_message :: Lens.Lens' ShareError (Core.Maybe Core.Text)
shareError_message = Lens.lens (\ShareError' {message} -> message) (\s@ShareError' {} a -> s {message = a} :: ShareError)

-- | List of accounts impacted by the error.
shareError_accounts :: Lens.Lens' ShareError (Core.Maybe [Core.Text])
shareError_accounts = Lens.lens (\ShareError' {accounts} -> accounts) (\s@ShareError' {} a -> s {accounts = a} :: ShareError) Core.. Lens.mapping Lens._Coerce

-- | Error type that happened when processing the operation.
shareError_error :: Lens.Lens' ShareError (Core.Maybe Core.Text)
shareError_error = Lens.lens (\ShareError' {error} -> error) (\s@ShareError' {} a -> s {error = a} :: ShareError)

instance Core.FromJSON ShareError where
  parseJSON =
    Core.withObject
      "ShareError"
      ( \x ->
          ShareError'
            Core.<$> (x Core..:? "Message")
            Core.<*> (x Core..:? "Accounts" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Error")
      )

instance Core.Hashable ShareError

instance Core.NFData ShareError
