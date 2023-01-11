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
-- Module      : Amazonka.EC2.Types.ReservedInstancesId
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ReservedInstancesId where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the ID of a Reserved Instance.
--
-- /See:/ 'newReservedInstancesId' smart constructor.
data ReservedInstancesId = ReservedInstancesId'
  { -- | The ID of the Reserved Instance.
    reservedInstancesId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReservedInstancesId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedInstancesId', 'reservedInstancesId_reservedInstancesId' - The ID of the Reserved Instance.
newReservedInstancesId ::
  ReservedInstancesId
newReservedInstancesId =
  ReservedInstancesId'
    { reservedInstancesId =
        Prelude.Nothing
    }

-- | The ID of the Reserved Instance.
reservedInstancesId_reservedInstancesId :: Lens.Lens' ReservedInstancesId (Prelude.Maybe Prelude.Text)
reservedInstancesId_reservedInstancesId = Lens.lens (\ReservedInstancesId' {reservedInstancesId} -> reservedInstancesId) (\s@ReservedInstancesId' {} a -> s {reservedInstancesId = a} :: ReservedInstancesId)

instance Data.FromXML ReservedInstancesId where
  parseXML x =
    ReservedInstancesId'
      Prelude.<$> (x Data..@? "reservedInstancesId")

instance Prelude.Hashable ReservedInstancesId where
  hashWithSalt _salt ReservedInstancesId' {..} =
    _salt `Prelude.hashWithSalt` reservedInstancesId

instance Prelude.NFData ReservedInstancesId where
  rnf ReservedInstancesId' {..} =
    Prelude.rnf reservedInstancesId
