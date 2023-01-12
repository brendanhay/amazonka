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
-- Module      : Amazonka.FSx.Types.AdministrativeActionFailureDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.AdministrativeActionFailureDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a failed administrative action.
--
-- /See:/ 'newAdministrativeActionFailureDetails' smart constructor.
data AdministrativeActionFailureDetails = AdministrativeActionFailureDetails'
  { -- | Error message providing details about the failed administrative action.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdministrativeActionFailureDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'administrativeActionFailureDetails_message' - Error message providing details about the failed administrative action.
newAdministrativeActionFailureDetails ::
  AdministrativeActionFailureDetails
newAdministrativeActionFailureDetails =
  AdministrativeActionFailureDetails'
    { message =
        Prelude.Nothing
    }

-- | Error message providing details about the failed administrative action.
administrativeActionFailureDetails_message :: Lens.Lens' AdministrativeActionFailureDetails (Prelude.Maybe Prelude.Text)
administrativeActionFailureDetails_message = Lens.lens (\AdministrativeActionFailureDetails' {message} -> message) (\s@AdministrativeActionFailureDetails' {} a -> s {message = a} :: AdministrativeActionFailureDetails)

instance
  Data.FromJSON
    AdministrativeActionFailureDetails
  where
  parseJSON =
    Data.withObject
      "AdministrativeActionFailureDetails"
      ( \x ->
          AdministrativeActionFailureDetails'
            Prelude.<$> (x Data..:? "Message")
      )

instance
  Prelude.Hashable
    AdministrativeActionFailureDetails
  where
  hashWithSalt
    _salt
    AdministrativeActionFailureDetails' {..} =
      _salt `Prelude.hashWithSalt` message

instance
  Prelude.NFData
    AdministrativeActionFailureDetails
  where
  rnf AdministrativeActionFailureDetails' {..} =
    Prelude.rnf message
