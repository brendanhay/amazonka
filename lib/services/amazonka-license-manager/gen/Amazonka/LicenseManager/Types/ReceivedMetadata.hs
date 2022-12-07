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
-- Module      : Amazonka.LicenseManager.Types.ReceivedMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.ReceivedMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types.AllowedOperation
import Amazonka.LicenseManager.Types.ReceivedStatus
import qualified Amazonka.Prelude as Prelude

-- | Metadata associated with received licenses and grants.
--
-- /See:/ 'newReceivedMetadata' smart constructor.
data ReceivedMetadata = ReceivedMetadata'
  { -- | Allowed operations.
    allowedOperations :: Prelude.Maybe (Prelude.NonEmpty AllowedOperation),
    -- | Received status.
    receivedStatus :: Prelude.Maybe ReceivedStatus,
    -- | Received status reason.
    receivedStatusReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReceivedMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowedOperations', 'receivedMetadata_allowedOperations' - Allowed operations.
--
-- 'receivedStatus', 'receivedMetadata_receivedStatus' - Received status.
--
-- 'receivedStatusReason', 'receivedMetadata_receivedStatusReason' - Received status reason.
newReceivedMetadata ::
  ReceivedMetadata
newReceivedMetadata =
  ReceivedMetadata'
    { allowedOperations =
        Prelude.Nothing,
      receivedStatus = Prelude.Nothing,
      receivedStatusReason = Prelude.Nothing
    }

-- | Allowed operations.
receivedMetadata_allowedOperations :: Lens.Lens' ReceivedMetadata (Prelude.Maybe (Prelude.NonEmpty AllowedOperation))
receivedMetadata_allowedOperations = Lens.lens (\ReceivedMetadata' {allowedOperations} -> allowedOperations) (\s@ReceivedMetadata' {} a -> s {allowedOperations = a} :: ReceivedMetadata) Prelude.. Lens.mapping Lens.coerced

-- | Received status.
receivedMetadata_receivedStatus :: Lens.Lens' ReceivedMetadata (Prelude.Maybe ReceivedStatus)
receivedMetadata_receivedStatus = Lens.lens (\ReceivedMetadata' {receivedStatus} -> receivedStatus) (\s@ReceivedMetadata' {} a -> s {receivedStatus = a} :: ReceivedMetadata)

-- | Received status reason.
receivedMetadata_receivedStatusReason :: Lens.Lens' ReceivedMetadata (Prelude.Maybe Prelude.Text)
receivedMetadata_receivedStatusReason = Lens.lens (\ReceivedMetadata' {receivedStatusReason} -> receivedStatusReason) (\s@ReceivedMetadata' {} a -> s {receivedStatusReason = a} :: ReceivedMetadata)

instance Data.FromJSON ReceivedMetadata where
  parseJSON =
    Data.withObject
      "ReceivedMetadata"
      ( \x ->
          ReceivedMetadata'
            Prelude.<$> (x Data..:? "AllowedOperations")
            Prelude.<*> (x Data..:? "ReceivedStatus")
            Prelude.<*> (x Data..:? "ReceivedStatusReason")
      )

instance Prelude.Hashable ReceivedMetadata where
  hashWithSalt _salt ReceivedMetadata' {..} =
    _salt `Prelude.hashWithSalt` allowedOperations
      `Prelude.hashWithSalt` receivedStatus
      `Prelude.hashWithSalt` receivedStatusReason

instance Prelude.NFData ReceivedMetadata where
  rnf ReceivedMetadata' {..} =
    Prelude.rnf allowedOperations
      `Prelude.seq` Prelude.rnf receivedStatus
      `Prelude.seq` Prelude.rnf receivedStatusReason
