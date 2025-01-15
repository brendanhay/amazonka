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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.FailedQueuedPurchaseDeletion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.DeleteQueuedReservedInstancesError
import qualified Amazonka.Prelude as Prelude

-- | Describes a Reserved Instance whose queued purchase was not deleted.
--
-- /See:/ 'newFailedQueuedPurchaseDeletion' smart constructor.
data FailedQueuedPurchaseDeletion = FailedQueuedPurchaseDeletion'
  { -- | The error.
    error :: Prelude.Maybe DeleteQueuedReservedInstancesError,
    -- | The ID of the Reserved Instance.
    reservedInstancesId :: Prelude.Maybe Prelude.Text
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
-- 'error', 'failedQueuedPurchaseDeletion_error' - The error.
--
-- 'reservedInstancesId', 'failedQueuedPurchaseDeletion_reservedInstancesId' - The ID of the Reserved Instance.
newFailedQueuedPurchaseDeletion ::
  FailedQueuedPurchaseDeletion
newFailedQueuedPurchaseDeletion =
  FailedQueuedPurchaseDeletion'
    { error =
        Prelude.Nothing,
      reservedInstancesId = Prelude.Nothing
    }

-- | The error.
failedQueuedPurchaseDeletion_error :: Lens.Lens' FailedQueuedPurchaseDeletion (Prelude.Maybe DeleteQueuedReservedInstancesError)
failedQueuedPurchaseDeletion_error = Lens.lens (\FailedQueuedPurchaseDeletion' {error} -> error) (\s@FailedQueuedPurchaseDeletion' {} a -> s {error = a} :: FailedQueuedPurchaseDeletion)

-- | The ID of the Reserved Instance.
failedQueuedPurchaseDeletion_reservedInstancesId :: Lens.Lens' FailedQueuedPurchaseDeletion (Prelude.Maybe Prelude.Text)
failedQueuedPurchaseDeletion_reservedInstancesId = Lens.lens (\FailedQueuedPurchaseDeletion' {reservedInstancesId} -> reservedInstancesId) (\s@FailedQueuedPurchaseDeletion' {} a -> s {reservedInstancesId = a} :: FailedQueuedPurchaseDeletion)

instance Data.FromXML FailedQueuedPurchaseDeletion where
  parseXML x =
    FailedQueuedPurchaseDeletion'
      Prelude.<$> (x Data..@? "error")
      Prelude.<*> (x Data..@? "reservedInstancesId")

instance
  Prelude.Hashable
    FailedQueuedPurchaseDeletion
  where
  hashWithSalt _salt FailedQueuedPurchaseDeletion' {..} =
    _salt
      `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` reservedInstancesId

instance Prelude.NFData FailedQueuedPurchaseDeletion where
  rnf FailedQueuedPurchaseDeletion' {..} =
    Prelude.rnf error `Prelude.seq`
      Prelude.rnf reservedInstancesId
