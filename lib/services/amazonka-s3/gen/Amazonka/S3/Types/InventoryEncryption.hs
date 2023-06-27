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
-- Module      : Amazonka.S3.Types.InventoryEncryption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.InventoryEncryption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.SSEKMS
import Amazonka.S3.Types.SSES3

-- | Contains the type of server-side encryption used to encrypt the
-- inventory results.
--
-- /See:/ 'newInventoryEncryption' smart constructor.
data InventoryEncryption = InventoryEncryption'
  { -- | Specifies the use of SSE-KMS to encrypt delivered inventory reports.
    ssekms :: Prelude.Maybe SSEKMS,
    -- | Specifies the use of SSE-S3 to encrypt delivered inventory reports.
    sses3 :: Prelude.Maybe SSES3
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InventoryEncryption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ssekms', 'inventoryEncryption_ssekms' - Specifies the use of SSE-KMS to encrypt delivered inventory reports.
--
-- 'sses3', 'inventoryEncryption_sses3' - Specifies the use of SSE-S3 to encrypt delivered inventory reports.
newInventoryEncryption ::
  InventoryEncryption
newInventoryEncryption =
  InventoryEncryption'
    { ssekms = Prelude.Nothing,
      sses3 = Prelude.Nothing
    }

-- | Specifies the use of SSE-KMS to encrypt delivered inventory reports.
inventoryEncryption_ssekms :: Lens.Lens' InventoryEncryption (Prelude.Maybe SSEKMS)
inventoryEncryption_ssekms = Lens.lens (\InventoryEncryption' {ssekms} -> ssekms) (\s@InventoryEncryption' {} a -> s {ssekms = a} :: InventoryEncryption)

-- | Specifies the use of SSE-S3 to encrypt delivered inventory reports.
inventoryEncryption_sses3 :: Lens.Lens' InventoryEncryption (Prelude.Maybe SSES3)
inventoryEncryption_sses3 = Lens.lens (\InventoryEncryption' {sses3} -> sses3) (\s@InventoryEncryption' {} a -> s {sses3 = a} :: InventoryEncryption)

instance Data.FromXML InventoryEncryption where
  parseXML x =
    InventoryEncryption'
      Prelude.<$> (x Data..@? "SSE-KMS")
      Prelude.<*> (x Data..@? "SSE-S3")

instance Prelude.Hashable InventoryEncryption where
  hashWithSalt _salt InventoryEncryption' {..} =
    _salt
      `Prelude.hashWithSalt` ssekms
      `Prelude.hashWithSalt` sses3

instance Prelude.NFData InventoryEncryption where
  rnf InventoryEncryption' {..} =
    Prelude.rnf ssekms `Prelude.seq` Prelude.rnf sses3

instance Data.ToXML InventoryEncryption where
  toXML InventoryEncryption' {..} =
    Prelude.mconcat
      ["SSE-KMS" Data.@= ssekms, "SSE-S3" Data.@= sses3]
