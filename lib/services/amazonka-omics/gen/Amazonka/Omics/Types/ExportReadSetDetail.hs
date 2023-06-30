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
-- Module      : Amazonka.Omics.Types.ExportReadSetDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ExportReadSetDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.ReadSetExportJobItemStatus
import qualified Amazonka.Prelude as Prelude

-- | Details about a read set.
--
-- /See:/ 'newExportReadSetDetail' smart constructor.
data ExportReadSetDetail = ExportReadSetDetail'
  { -- | The set\'s status message.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The set\'s ID.
    id :: Prelude.Text,
    -- | The set\'s status.
    status :: ReadSetExportJobItemStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportReadSetDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessage', 'exportReadSetDetail_statusMessage' - The set\'s status message.
--
-- 'id', 'exportReadSetDetail_id' - The set\'s ID.
--
-- 'status', 'exportReadSetDetail_status' - The set\'s status.
newExportReadSetDetail ::
  -- | 'id'
  Prelude.Text ->
  -- | 'status'
  ReadSetExportJobItemStatus ->
  ExportReadSetDetail
newExportReadSetDetail pId_ pStatus_ =
  ExportReadSetDetail'
    { statusMessage =
        Prelude.Nothing,
      id = pId_,
      status = pStatus_
    }

-- | The set\'s status message.
exportReadSetDetail_statusMessage :: Lens.Lens' ExportReadSetDetail (Prelude.Maybe Prelude.Text)
exportReadSetDetail_statusMessage = Lens.lens (\ExportReadSetDetail' {statusMessage} -> statusMessage) (\s@ExportReadSetDetail' {} a -> s {statusMessage = a} :: ExportReadSetDetail)

-- | The set\'s ID.
exportReadSetDetail_id :: Lens.Lens' ExportReadSetDetail Prelude.Text
exportReadSetDetail_id = Lens.lens (\ExportReadSetDetail' {id} -> id) (\s@ExportReadSetDetail' {} a -> s {id = a} :: ExportReadSetDetail)

-- | The set\'s status.
exportReadSetDetail_status :: Lens.Lens' ExportReadSetDetail ReadSetExportJobItemStatus
exportReadSetDetail_status = Lens.lens (\ExportReadSetDetail' {status} -> status) (\s@ExportReadSetDetail' {} a -> s {status = a} :: ExportReadSetDetail)

instance Data.FromJSON ExportReadSetDetail where
  parseJSON =
    Data.withObject
      "ExportReadSetDetail"
      ( \x ->
          ExportReadSetDetail'
            Prelude.<$> (x Data..:? "statusMessage")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "status")
      )

instance Prelude.Hashable ExportReadSetDetail where
  hashWithSalt _salt ExportReadSetDetail' {..} =
    _salt
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` status

instance Prelude.NFData ExportReadSetDetail where
  rnf ExportReadSetDetail' {..} =
    Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf status
