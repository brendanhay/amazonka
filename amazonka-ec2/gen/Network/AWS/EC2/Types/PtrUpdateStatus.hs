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
-- Module      : Network.AWS.EC2.Types.PtrUpdateStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PtrUpdateStatus where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The status of an updated pointer (PTR) record for an Elastic IP address.
--
-- /See:/ 'newPtrUpdateStatus' smart constructor.
data PtrUpdateStatus = PtrUpdateStatus'
  { -- | The status of the PTR record update.
    status :: Prelude.Maybe Prelude.Text,
    -- | The reason for the PTR record update.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The value for the PTR record update.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PtrUpdateStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'ptrUpdateStatus_status' - The status of the PTR record update.
--
-- 'reason', 'ptrUpdateStatus_reason' - The reason for the PTR record update.
--
-- 'value', 'ptrUpdateStatus_value' - The value for the PTR record update.
newPtrUpdateStatus ::
  PtrUpdateStatus
newPtrUpdateStatus =
  PtrUpdateStatus'
    { status = Prelude.Nothing,
      reason = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The status of the PTR record update.
ptrUpdateStatus_status :: Lens.Lens' PtrUpdateStatus (Prelude.Maybe Prelude.Text)
ptrUpdateStatus_status = Lens.lens (\PtrUpdateStatus' {status} -> status) (\s@PtrUpdateStatus' {} a -> s {status = a} :: PtrUpdateStatus)

-- | The reason for the PTR record update.
ptrUpdateStatus_reason :: Lens.Lens' PtrUpdateStatus (Prelude.Maybe Prelude.Text)
ptrUpdateStatus_reason = Lens.lens (\PtrUpdateStatus' {reason} -> reason) (\s@PtrUpdateStatus' {} a -> s {reason = a} :: PtrUpdateStatus)

-- | The value for the PTR record update.
ptrUpdateStatus_value :: Lens.Lens' PtrUpdateStatus (Prelude.Maybe Prelude.Text)
ptrUpdateStatus_value = Lens.lens (\PtrUpdateStatus' {value} -> value) (\s@PtrUpdateStatus' {} a -> s {value = a} :: PtrUpdateStatus)

instance Prelude.FromXML PtrUpdateStatus where
  parseXML x =
    PtrUpdateStatus'
      Prelude.<$> (x Prelude..@? "status")
      Prelude.<*> (x Prelude..@? "reason")
      Prelude.<*> (x Prelude..@? "value")

instance Prelude.Hashable PtrUpdateStatus

instance Prelude.NFData PtrUpdateStatus
