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
-- Module      : Amazonka.MGN.Types.ExportErrorData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.ExportErrorData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Export errors data.
--
-- /See:/ 'newExportErrorData' smart constructor.
data ExportErrorData = ExportErrorData'
  { -- | Export errors data raw error.
    rawError :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportErrorData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rawError', 'exportErrorData_rawError' - Export errors data raw error.
newExportErrorData ::
  ExportErrorData
newExportErrorData =
  ExportErrorData' {rawError = Prelude.Nothing}

-- | Export errors data raw error.
exportErrorData_rawError :: Lens.Lens' ExportErrorData (Prelude.Maybe Prelude.Text)
exportErrorData_rawError = Lens.lens (\ExportErrorData' {rawError} -> rawError) (\s@ExportErrorData' {} a -> s {rawError = a} :: ExportErrorData)

instance Data.FromJSON ExportErrorData where
  parseJSON =
    Data.withObject
      "ExportErrorData"
      ( \x ->
          ExportErrorData' Prelude.<$> (x Data..:? "rawError")
      )

instance Prelude.Hashable ExportErrorData where
  hashWithSalt _salt ExportErrorData' {..} =
    _salt `Prelude.hashWithSalt` rawError

instance Prelude.NFData ExportErrorData where
  rnf ExportErrorData' {..} = Prelude.rnf rawError
