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
-- Module      : Amazonka.QuickSight.Types.NamespaceError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.NamespaceError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.NamespaceErrorType

-- | Errors that occur during namespace creation.
--
-- /See:/ 'newNamespaceError' smart constructor.
data NamespaceError = NamespaceError'
  { -- | The message for the error.
    message :: Prelude.Maybe Prelude.Text,
    -- | The error type.
    type' :: Prelude.Maybe NamespaceErrorType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NamespaceError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'namespaceError_message' - The message for the error.
--
-- 'type'', 'namespaceError_type' - The error type.
newNamespaceError ::
  NamespaceError
newNamespaceError =
  NamespaceError'
    { message = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The message for the error.
namespaceError_message :: Lens.Lens' NamespaceError (Prelude.Maybe Prelude.Text)
namespaceError_message = Lens.lens (\NamespaceError' {message} -> message) (\s@NamespaceError' {} a -> s {message = a} :: NamespaceError)

-- | The error type.
namespaceError_type :: Lens.Lens' NamespaceError (Prelude.Maybe NamespaceErrorType)
namespaceError_type = Lens.lens (\NamespaceError' {type'} -> type') (\s@NamespaceError' {} a -> s {type' = a} :: NamespaceError)

instance Data.FromJSON NamespaceError where
  parseJSON =
    Data.withObject
      "NamespaceError"
      ( \x ->
          NamespaceError'
            Prelude.<$> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable NamespaceError where
  hashWithSalt _salt NamespaceError' {..} =
    _salt
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` type'

instance Prelude.NFData NamespaceError where
  rnf NamespaceError' {..} =
    Prelude.rnf message `Prelude.seq` Prelude.rnf type'
