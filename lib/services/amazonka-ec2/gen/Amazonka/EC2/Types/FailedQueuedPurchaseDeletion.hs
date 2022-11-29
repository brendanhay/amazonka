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
-- Module      : Amazonka.EC2.Types.FailedQueuedPurchaseDeletion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.FailedQueuedPurchaseDeletion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.DeleteQueuedReservedInstancesError
import qualified Amazonka.Prelude as Prelude

-- | Describes a Reserved Instance whose queued purchase was not deleted.
--
-- /See:/ 'newFailedQueuedPurchaseDeletion' smart constructor.
data FailedQueuedPurchaseDeletion = FailedQueuedPurchaseDeletion'
  { -- | The ID of the Reserved Instance.
    reservedInstancesId :: Prelude.Maybe Prelude.Text,
    -- | The error.
    error :: Prelude.Maybe DeleteQueuedReservedInstancesError
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailedQueuedPurchaseDeletion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedInstancesId', 'failedQueuedPurchaseDeletion_reservedInstancesId' - The ID of the Reserved Instance.
--
-- 'error', 'failedQueuedPurchaseDeletion_error' - The error.
newFailedQueuedPurchaseDeletion ::
  FailedQueuedPurchaseDeletion
newFailedQueuedPurchaseDeletion =
  FailedQueuedPurchaseDeletion'
    { reservedInstancesId =
        Prelude.Nothing,
      error = Prelude.Nothing
    }

-- | The ID of the Reserved Instance.
failedQueuedPurchaseDeletion_reservedInstancesId :: Lens.Lens' FailedQueuedPurchaseDeletion (Prelude.Maybe Prelude.Text)
failedQueuedPurchaseDeletion_reservedInstancesId = Lens.lens (\FailedQueuedPurchaseDeletion' {reservedInstancesId} -> reservedInstancesId) (\s@FailedQueuedPurchaseDeletion' {} a -> s {reservedInstancesId = a} :: FailedQueuedPurchaseDeletion)

-- | The error.
failedQueuedPurchaseDeletion_error :: Lens.Lens' FailedQueuedPurchaseDeletion (Prelude.Maybe DeleteQueuedReservedInstancesError)
failedQueuedPurchaseDeletion_error = Lens.lens (\FailedQueuedPurchaseDeletion' {error} -> error) (\s@FailedQueuedPurchaseDeletion' {} a -> s {error = a} :: FailedQueuedPurchaseDeletion)

instance Core.FromXML FailedQueuedPurchaseDeletion where
  parseXML x =
    FailedQueuedPurchaseDeletion'
      Prelude.<$> (x Core..@? "reservedInstancesId")
      Prelude.<*> (x Core..@? "error")

instance
  Prelude.Hashable
    FailedQueuedPurchaseDeletion
  where
  hashWithSalt _salt FailedQueuedPurchaseDeletion' {..} =
    _salt `Prelude.hashWithSalt` reservedInstancesId
      `Prelude.hashWithSalt` error

instance Prelude.NFData FailedQueuedPurchaseDeletion where
  rnf FailedQueuedPurchaseDeletion' {..} =
    Prelude.rnf reservedInstancesId
      `Prelude.seq` Prelude.rnf error
