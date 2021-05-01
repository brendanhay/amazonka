{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudWatchLogs.Types.ExportTaskStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.ExportTaskStatus where

import Network.AWS.CloudWatchLogs.Types.ExportTaskStatusCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the status of an export task.
--
-- /See:/ 'newExportTaskStatus' smart constructor.
data ExportTaskStatus = ExportTaskStatus'
  { -- | The status message related to the status code.
    message :: Prelude.Maybe Prelude.Text,
    -- | The status code of the export task.
    code :: Prelude.Maybe ExportTaskStatusCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ExportTaskStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'exportTaskStatus_message' - The status message related to the status code.
--
-- 'code', 'exportTaskStatus_code' - The status code of the export task.
newExportTaskStatus ::
  ExportTaskStatus
newExportTaskStatus =
  ExportTaskStatus'
    { message = Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | The status message related to the status code.
exportTaskStatus_message :: Lens.Lens' ExportTaskStatus (Prelude.Maybe Prelude.Text)
exportTaskStatus_message = Lens.lens (\ExportTaskStatus' {message} -> message) (\s@ExportTaskStatus' {} a -> s {message = a} :: ExportTaskStatus)

-- | The status code of the export task.
exportTaskStatus_code :: Lens.Lens' ExportTaskStatus (Prelude.Maybe ExportTaskStatusCode)
exportTaskStatus_code = Lens.lens (\ExportTaskStatus' {code} -> code) (\s@ExportTaskStatus' {} a -> s {code = a} :: ExportTaskStatus)

instance Prelude.FromJSON ExportTaskStatus where
  parseJSON =
    Prelude.withObject
      "ExportTaskStatus"
      ( \x ->
          ExportTaskStatus'
            Prelude.<$> (x Prelude..:? "message")
            Prelude.<*> (x Prelude..:? "code")
      )

instance Prelude.Hashable ExportTaskStatus

instance Prelude.NFData ExportTaskStatus
