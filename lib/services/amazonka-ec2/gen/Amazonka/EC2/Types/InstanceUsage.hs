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
-- Module      : Amazonka.EC2.Types.InstanceUsage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceUsage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Information about the Capacity Reservation usage.
--
-- /See:/ 'newInstanceUsage' smart constructor.
data InstanceUsage = InstanceUsage'
  { -- | The number of instances the Amazon Web Services account currently has in
    -- the Capacity Reservation.
    usedInstanceCount :: Prelude.Maybe Prelude.Int,
    -- | The ID of the Amazon Web Services account that is making use of the
    -- Capacity Reservation.
    accountId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'usedInstanceCount', 'instanceUsage_usedInstanceCount' - The number of instances the Amazon Web Services account currently has in
-- the Capacity Reservation.
--
-- 'accountId', 'instanceUsage_accountId' - The ID of the Amazon Web Services account that is making use of the
-- Capacity Reservation.
newInstanceUsage ::
  InstanceUsage
newInstanceUsage =
  InstanceUsage'
    { usedInstanceCount = Prelude.Nothing,
      accountId = Prelude.Nothing
    }

-- | The number of instances the Amazon Web Services account currently has in
-- the Capacity Reservation.
instanceUsage_usedInstanceCount :: Lens.Lens' InstanceUsage (Prelude.Maybe Prelude.Int)
instanceUsage_usedInstanceCount = Lens.lens (\InstanceUsage' {usedInstanceCount} -> usedInstanceCount) (\s@InstanceUsage' {} a -> s {usedInstanceCount = a} :: InstanceUsage)

-- | The ID of the Amazon Web Services account that is making use of the
-- Capacity Reservation.
instanceUsage_accountId :: Lens.Lens' InstanceUsage (Prelude.Maybe Prelude.Text)
instanceUsage_accountId = Lens.lens (\InstanceUsage' {accountId} -> accountId) (\s@InstanceUsage' {} a -> s {accountId = a} :: InstanceUsage)

instance Data.FromXML InstanceUsage where
  parseXML x =
    InstanceUsage'
      Prelude.<$> (x Data..@? "usedInstanceCount")
      Prelude.<*> (x Data..@? "accountId")

instance Prelude.Hashable InstanceUsage where
  hashWithSalt _salt InstanceUsage' {..} =
    _salt `Prelude.hashWithSalt` usedInstanceCount
      `Prelude.hashWithSalt` accountId

instance Prelude.NFData InstanceUsage where
  rnf InstanceUsage' {..} =
    Prelude.rnf usedInstanceCount
      `Prelude.seq` Prelude.rnf accountId
