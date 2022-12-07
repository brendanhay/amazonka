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
-- Module      : Amazonka.FMS.Types.FailedItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.FailedItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types.FailedItemReason
import qualified Amazonka.Prelude as Prelude

-- | Details of a resource that failed when trying to update it\'s
-- association to a resource set.
--
-- /See:/ 'newFailedItem' smart constructor.
data FailedItem = FailedItem'
  { -- | The univeral resource indicator (URI) of the resource that failed.
    uri :: Prelude.Maybe Prelude.Text,
    -- | The reason the resource\'s association could not be updated.
    reason :: Prelude.Maybe FailedItemReason
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailedItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uri', 'failedItem_uri' - The univeral resource indicator (URI) of the resource that failed.
--
-- 'reason', 'failedItem_reason' - The reason the resource\'s association could not be updated.
newFailedItem ::
  FailedItem
newFailedItem =
  FailedItem'
    { uri = Prelude.Nothing,
      reason = Prelude.Nothing
    }

-- | The univeral resource indicator (URI) of the resource that failed.
failedItem_uri :: Lens.Lens' FailedItem (Prelude.Maybe Prelude.Text)
failedItem_uri = Lens.lens (\FailedItem' {uri} -> uri) (\s@FailedItem' {} a -> s {uri = a} :: FailedItem)

-- | The reason the resource\'s association could not be updated.
failedItem_reason :: Lens.Lens' FailedItem (Prelude.Maybe FailedItemReason)
failedItem_reason = Lens.lens (\FailedItem' {reason} -> reason) (\s@FailedItem' {} a -> s {reason = a} :: FailedItem)

instance Data.FromJSON FailedItem where
  parseJSON =
    Data.withObject
      "FailedItem"
      ( \x ->
          FailedItem'
            Prelude.<$> (x Data..:? "URI") Prelude.<*> (x Data..:? "Reason")
      )

instance Prelude.Hashable FailedItem where
  hashWithSalt _salt FailedItem' {..} =
    _salt `Prelude.hashWithSalt` uri
      `Prelude.hashWithSalt` reason

instance Prelude.NFData FailedItem where
  rnf FailedItem' {..} =
    Prelude.rnf uri `Prelude.seq` Prelude.rnf reason
