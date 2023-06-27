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
-- Module      : Amazonka.MGN.Types.ImportTaskError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.ImportTaskError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types.ImportErrorData
import Amazonka.MGN.Types.ImportErrorType
import qualified Amazonka.Prelude as Prelude

-- | Import task error.
--
-- /See:/ 'newImportTaskError' smart constructor.
data ImportTaskError = ImportTaskError'
  { -- | Import task error data.
    errorData :: Prelude.Maybe ImportErrorData,
    -- | Import task error datetime.
    errorDateTime :: Prelude.Maybe Prelude.Text,
    -- | Import task error type.
    errorType :: Prelude.Maybe ImportErrorType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportTaskError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorData', 'importTaskError_errorData' - Import task error data.
--
-- 'errorDateTime', 'importTaskError_errorDateTime' - Import task error datetime.
--
-- 'errorType', 'importTaskError_errorType' - Import task error type.
newImportTaskError ::
  ImportTaskError
newImportTaskError =
  ImportTaskError'
    { errorData = Prelude.Nothing,
      errorDateTime = Prelude.Nothing,
      errorType = Prelude.Nothing
    }

-- | Import task error data.
importTaskError_errorData :: Lens.Lens' ImportTaskError (Prelude.Maybe ImportErrorData)
importTaskError_errorData = Lens.lens (\ImportTaskError' {errorData} -> errorData) (\s@ImportTaskError' {} a -> s {errorData = a} :: ImportTaskError)

-- | Import task error datetime.
importTaskError_errorDateTime :: Lens.Lens' ImportTaskError (Prelude.Maybe Prelude.Text)
importTaskError_errorDateTime = Lens.lens (\ImportTaskError' {errorDateTime} -> errorDateTime) (\s@ImportTaskError' {} a -> s {errorDateTime = a} :: ImportTaskError)

-- | Import task error type.
importTaskError_errorType :: Lens.Lens' ImportTaskError (Prelude.Maybe ImportErrorType)
importTaskError_errorType = Lens.lens (\ImportTaskError' {errorType} -> errorType) (\s@ImportTaskError' {} a -> s {errorType = a} :: ImportTaskError)

instance Data.FromJSON ImportTaskError where
  parseJSON =
    Data.withObject
      "ImportTaskError"
      ( \x ->
          ImportTaskError'
            Prelude.<$> (x Data..:? "errorData")
            Prelude.<*> (x Data..:? "errorDateTime")
            Prelude.<*> (x Data..:? "errorType")
      )

instance Prelude.Hashable ImportTaskError where
  hashWithSalt _salt ImportTaskError' {..} =
    _salt
      `Prelude.hashWithSalt` errorData
      `Prelude.hashWithSalt` errorDateTime
      `Prelude.hashWithSalt` errorType

instance Prelude.NFData ImportTaskError where
  rnf ImportTaskError' {..} =
    Prelude.rnf errorData
      `Prelude.seq` Prelude.rnf errorDateTime
      `Prelude.seq` Prelude.rnf errorType
