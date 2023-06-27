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
-- Module      : Amazonka.OpenSearch.Types.ValidationFailure
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.ValidationFailure where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A validation failure that occurred as the result of a pre-update
-- validation check (verbose dry run) on a domain.
--
-- /See:/ 'newValidationFailure' smart constructor.
data ValidationFailure = ValidationFailure'
  { -- | The error code of the failure.
    code :: Prelude.Maybe Prelude.Text,
    -- | A message corresponding to the failure.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidationFailure' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'validationFailure_code' - The error code of the failure.
--
-- 'message', 'validationFailure_message' - A message corresponding to the failure.
newValidationFailure ::
  ValidationFailure
newValidationFailure =
  ValidationFailure'
    { code = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The error code of the failure.
validationFailure_code :: Lens.Lens' ValidationFailure (Prelude.Maybe Prelude.Text)
validationFailure_code = Lens.lens (\ValidationFailure' {code} -> code) (\s@ValidationFailure' {} a -> s {code = a} :: ValidationFailure)

-- | A message corresponding to the failure.
validationFailure_message :: Lens.Lens' ValidationFailure (Prelude.Maybe Prelude.Text)
validationFailure_message = Lens.lens (\ValidationFailure' {message} -> message) (\s@ValidationFailure' {} a -> s {message = a} :: ValidationFailure)

instance Data.FromJSON ValidationFailure where
  parseJSON =
    Data.withObject
      "ValidationFailure"
      ( \x ->
          ValidationFailure'
            Prelude.<$> (x Data..:? "Code")
            Prelude.<*> (x Data..:? "Message")
      )

instance Prelude.Hashable ValidationFailure where
  hashWithSalt _salt ValidationFailure' {..} =
    _salt
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData ValidationFailure where
  rnf ValidationFailure' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
