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
-- Module      : Amazonka.MGN.Types.ExportTaskError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.ExportTaskError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types.ExportErrorData
import qualified Amazonka.Prelude as Prelude

-- | Export task error.
--
-- /See:/ 'newExportTaskError' smart constructor.
data ExportTaskError = ExportTaskError'
  { -- | Export task error data.
    errorData :: Prelude.Maybe ExportErrorData,
    -- | Export task error datetime.
    errorDateTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportTaskError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorData', 'exportTaskError_errorData' - Export task error data.
--
-- 'errorDateTime', 'exportTaskError_errorDateTime' - Export task error datetime.
newExportTaskError ::
  ExportTaskError
newExportTaskError =
  ExportTaskError'
    { errorData = Prelude.Nothing,
      errorDateTime = Prelude.Nothing
    }

-- | Export task error data.
exportTaskError_errorData :: Lens.Lens' ExportTaskError (Prelude.Maybe ExportErrorData)
exportTaskError_errorData = Lens.lens (\ExportTaskError' {errorData} -> errorData) (\s@ExportTaskError' {} a -> s {errorData = a} :: ExportTaskError)

-- | Export task error datetime.
exportTaskError_errorDateTime :: Lens.Lens' ExportTaskError (Prelude.Maybe Prelude.Text)
exportTaskError_errorDateTime = Lens.lens (\ExportTaskError' {errorDateTime} -> errorDateTime) (\s@ExportTaskError' {} a -> s {errorDateTime = a} :: ExportTaskError)

instance Data.FromJSON ExportTaskError where
  parseJSON =
    Data.withObject
      "ExportTaskError"
      ( \x ->
          ExportTaskError'
            Prelude.<$> (x Data..:? "errorData")
            Prelude.<*> (x Data..:? "errorDateTime")
      )

instance Prelude.Hashable ExportTaskError where
  hashWithSalt _salt ExportTaskError' {..} =
    _salt
      `Prelude.hashWithSalt` errorData
      `Prelude.hashWithSalt` errorDateTime

instance Prelude.NFData ExportTaskError where
  rnf ExportTaskError' {..} =
    Prelude.rnf errorData
      `Prelude.seq` Prelude.rnf errorDateTime
