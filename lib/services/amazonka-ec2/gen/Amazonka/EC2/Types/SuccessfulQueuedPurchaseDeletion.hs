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
-- Module      : Amazonka.EC2.Types.SuccessfulQueuedPurchaseDeletion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SuccessfulQueuedPurchaseDeletion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes a Reserved Instance whose queued purchase was successfully
-- deleted.
--
-- /See:/ 'newSuccessfulQueuedPurchaseDeletion' smart constructor.
data SuccessfulQueuedPurchaseDeletion = SuccessfulQueuedPurchaseDeletion'
  { -- | The ID of the Reserved Instance.
    reservedInstancesId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SuccessfulQueuedPurchaseDeletion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedInstancesId', 'successfulQueuedPurchaseDeletion_reservedInstancesId' - The ID of the Reserved Instance.
newSuccessfulQueuedPurchaseDeletion ::
  SuccessfulQueuedPurchaseDeletion
newSuccessfulQueuedPurchaseDeletion =
  SuccessfulQueuedPurchaseDeletion'
    { reservedInstancesId =
        Prelude.Nothing
    }

-- | The ID of the Reserved Instance.
successfulQueuedPurchaseDeletion_reservedInstancesId :: Lens.Lens' SuccessfulQueuedPurchaseDeletion (Prelude.Maybe Prelude.Text)
successfulQueuedPurchaseDeletion_reservedInstancesId = Lens.lens (\SuccessfulQueuedPurchaseDeletion' {reservedInstancesId} -> reservedInstancesId) (\s@SuccessfulQueuedPurchaseDeletion' {} a -> s {reservedInstancesId = a} :: SuccessfulQueuedPurchaseDeletion)

instance
  Core.FromXML
    SuccessfulQueuedPurchaseDeletion
  where
  parseXML x =
    SuccessfulQueuedPurchaseDeletion'
      Prelude.<$> (x Core..@? "reservedInstancesId")

instance
  Prelude.Hashable
    SuccessfulQueuedPurchaseDeletion
  where
  hashWithSalt
    _salt
    SuccessfulQueuedPurchaseDeletion' {..} =
      _salt `Prelude.hashWithSalt` reservedInstancesId

instance
  Prelude.NFData
    SuccessfulQueuedPurchaseDeletion
  where
  rnf SuccessfulQueuedPurchaseDeletion' {..} =
    Prelude.rnf reservedInstancesId
