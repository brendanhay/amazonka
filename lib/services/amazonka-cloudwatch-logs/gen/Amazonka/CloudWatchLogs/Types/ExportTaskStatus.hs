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
-- Module      : Amazonka.CloudWatchLogs.Types.ExportTaskStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchLogs.Types.ExportTaskStatus where

import Amazonka.CloudWatchLogs.Types.ExportTaskStatusCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the status of an export task.
--
-- /See:/ 'newExportTaskStatus' smart constructor.
data ExportTaskStatus = ExportTaskStatus'
  { -- | The status code of the export task.
    code :: Prelude.Maybe ExportTaskStatusCode,
    -- | The status message related to the status code.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportTaskStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'exportTaskStatus_code' - The status code of the export task.
--
-- 'message', 'exportTaskStatus_message' - The status message related to the status code.
newExportTaskStatus ::
  ExportTaskStatus
newExportTaskStatus =
  ExportTaskStatus'
    { code = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The status code of the export task.
exportTaskStatus_code :: Lens.Lens' ExportTaskStatus (Prelude.Maybe ExportTaskStatusCode)
exportTaskStatus_code = Lens.lens (\ExportTaskStatus' {code} -> code) (\s@ExportTaskStatus' {} a -> s {code = a} :: ExportTaskStatus)

-- | The status message related to the status code.
exportTaskStatus_message :: Lens.Lens' ExportTaskStatus (Prelude.Maybe Prelude.Text)
exportTaskStatus_message = Lens.lens (\ExportTaskStatus' {message} -> message) (\s@ExportTaskStatus' {} a -> s {message = a} :: ExportTaskStatus)

instance Data.FromJSON ExportTaskStatus where
  parseJSON =
    Data.withObject
      "ExportTaskStatus"
      ( \x ->
          ExportTaskStatus'
            Prelude.<$> (x Data..:? "code")
            Prelude.<*> (x Data..:? "message")
      )

instance Prelude.Hashable ExportTaskStatus where
  hashWithSalt _salt ExportTaskStatus' {..} =
    _salt `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData ExportTaskStatus where
  rnf ExportTaskStatus' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
