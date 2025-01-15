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
-- Module      : Amazonka.EC2.Types.PtrUpdateStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.PtrUpdateStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The status of an updated pointer (PTR) record for an Elastic IP address.
--
-- /See:/ 'newPtrUpdateStatus' smart constructor.
data PtrUpdateStatus = PtrUpdateStatus'
  { -- | The reason for the PTR record update.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The status of the PTR record update.
    status :: Prelude.Maybe Prelude.Text,
    -- | The value for the PTR record update.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PtrUpdateStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reason', 'ptrUpdateStatus_reason' - The reason for the PTR record update.
--
-- 'status', 'ptrUpdateStatus_status' - The status of the PTR record update.
--
-- 'value', 'ptrUpdateStatus_value' - The value for the PTR record update.
newPtrUpdateStatus ::
  PtrUpdateStatus
newPtrUpdateStatus =
  PtrUpdateStatus'
    { reason = Prelude.Nothing,
      status = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The reason for the PTR record update.
ptrUpdateStatus_reason :: Lens.Lens' PtrUpdateStatus (Prelude.Maybe Prelude.Text)
ptrUpdateStatus_reason = Lens.lens (\PtrUpdateStatus' {reason} -> reason) (\s@PtrUpdateStatus' {} a -> s {reason = a} :: PtrUpdateStatus)

-- | The status of the PTR record update.
ptrUpdateStatus_status :: Lens.Lens' PtrUpdateStatus (Prelude.Maybe Prelude.Text)
ptrUpdateStatus_status = Lens.lens (\PtrUpdateStatus' {status} -> status) (\s@PtrUpdateStatus' {} a -> s {status = a} :: PtrUpdateStatus)

-- | The value for the PTR record update.
ptrUpdateStatus_value :: Lens.Lens' PtrUpdateStatus (Prelude.Maybe Prelude.Text)
ptrUpdateStatus_value = Lens.lens (\PtrUpdateStatus' {value} -> value) (\s@PtrUpdateStatus' {} a -> s {value = a} :: PtrUpdateStatus)

instance Data.FromXML PtrUpdateStatus where
  parseXML x =
    PtrUpdateStatus'
      Prelude.<$> (x Data..@? "reason")
      Prelude.<*> (x Data..@? "status")
      Prelude.<*> (x Data..@? "value")

instance Prelude.Hashable PtrUpdateStatus where
  hashWithSalt _salt PtrUpdateStatus' {..} =
    _salt
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` value

instance Prelude.NFData PtrUpdateStatus where
  rnf PtrUpdateStatus' {..} =
    Prelude.rnf reason `Prelude.seq`
      Prelude.rnf status `Prelude.seq`
        Prelude.rnf value
