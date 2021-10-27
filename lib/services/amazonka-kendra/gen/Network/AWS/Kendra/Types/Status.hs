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
-- Module      : Network.AWS.Kendra.Types.Status
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.Status where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.DocumentStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about the status of documents submitted for
-- indexing.
--
-- /See:/ 'newStatus' smart constructor.
data Status = Status'
  { -- | Provides detailed information about why the document couldn\'t be
    -- indexed. Use this information to correct the error before you resubmit
    -- the document for indexing.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the document.
    documentId :: Prelude.Maybe Prelude.Text,
    -- | Indicates the source of the error.
    failureCode :: Prelude.Maybe Prelude.Text,
    -- | The current status of a document.
    --
    -- If the document was submitted for deletion, the status is @NOT_FOUND@
    -- after the document is deleted.
    documentStatus :: Prelude.Maybe DocumentStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Status' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureReason', 'status_failureReason' - Provides detailed information about why the document couldn\'t be
-- indexed. Use this information to correct the error before you resubmit
-- the document for indexing.
--
-- 'documentId', 'status_documentId' - The unique identifier of the document.
--
-- 'failureCode', 'status_failureCode' - Indicates the source of the error.
--
-- 'documentStatus', 'status_documentStatus' - The current status of a document.
--
-- If the document was submitted for deletion, the status is @NOT_FOUND@
-- after the document is deleted.
newStatus ::
  Status
newStatus =
  Status'
    { failureReason = Prelude.Nothing,
      documentId = Prelude.Nothing,
      failureCode = Prelude.Nothing,
      documentStatus = Prelude.Nothing
    }

-- | Provides detailed information about why the document couldn\'t be
-- indexed. Use this information to correct the error before you resubmit
-- the document for indexing.
status_failureReason :: Lens.Lens' Status (Prelude.Maybe Prelude.Text)
status_failureReason = Lens.lens (\Status' {failureReason} -> failureReason) (\s@Status' {} a -> s {failureReason = a} :: Status)

-- | The unique identifier of the document.
status_documentId :: Lens.Lens' Status (Prelude.Maybe Prelude.Text)
status_documentId = Lens.lens (\Status' {documentId} -> documentId) (\s@Status' {} a -> s {documentId = a} :: Status)

-- | Indicates the source of the error.
status_failureCode :: Lens.Lens' Status (Prelude.Maybe Prelude.Text)
status_failureCode = Lens.lens (\Status' {failureCode} -> failureCode) (\s@Status' {} a -> s {failureCode = a} :: Status)

-- | The current status of a document.
--
-- If the document was submitted for deletion, the status is @NOT_FOUND@
-- after the document is deleted.
status_documentStatus :: Lens.Lens' Status (Prelude.Maybe DocumentStatus)
status_documentStatus = Lens.lens (\Status' {documentStatus} -> documentStatus) (\s@Status' {} a -> s {documentStatus = a} :: Status)

instance Core.FromJSON Status where
  parseJSON =
    Core.withObject
      "Status"
      ( \x ->
          Status'
            Prelude.<$> (x Core..:? "FailureReason")
            Prelude.<*> (x Core..:? "DocumentId")
            Prelude.<*> (x Core..:? "FailureCode")
            Prelude.<*> (x Core..:? "DocumentStatus")
      )

instance Prelude.Hashable Status

instance Prelude.NFData Status
