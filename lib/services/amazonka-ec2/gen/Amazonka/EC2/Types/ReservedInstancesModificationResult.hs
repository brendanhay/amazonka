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
-- Module      : Amazonka.EC2.Types.ReservedInstancesModificationResult
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ReservedInstancesModificationResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ReservedInstancesConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Describes the modification request\/s.
--
-- /See:/ 'newReservedInstancesModificationResult' smart constructor.
data ReservedInstancesModificationResult = ReservedInstancesModificationResult'
  { -- | The ID for the Reserved Instances that were created as part of the
    -- modification request. This field is only available when the modification
    -- is fulfilled.
    reservedInstancesId :: Prelude.Maybe Prelude.Text,
    -- | The target Reserved Instances configurations supplied as part of the
    -- modification request.
    targetConfiguration :: Prelude.Maybe ReservedInstancesConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReservedInstancesModificationResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedInstancesId', 'reservedInstancesModificationResult_reservedInstancesId' - The ID for the Reserved Instances that were created as part of the
-- modification request. This field is only available when the modification
-- is fulfilled.
--
-- 'targetConfiguration', 'reservedInstancesModificationResult_targetConfiguration' - The target Reserved Instances configurations supplied as part of the
-- modification request.
newReservedInstancesModificationResult ::
  ReservedInstancesModificationResult
newReservedInstancesModificationResult =
  ReservedInstancesModificationResult'
    { reservedInstancesId =
        Prelude.Nothing,
      targetConfiguration = Prelude.Nothing
    }

-- | The ID for the Reserved Instances that were created as part of the
-- modification request. This field is only available when the modification
-- is fulfilled.
reservedInstancesModificationResult_reservedInstancesId :: Lens.Lens' ReservedInstancesModificationResult (Prelude.Maybe Prelude.Text)
reservedInstancesModificationResult_reservedInstancesId = Lens.lens (\ReservedInstancesModificationResult' {reservedInstancesId} -> reservedInstancesId) (\s@ReservedInstancesModificationResult' {} a -> s {reservedInstancesId = a} :: ReservedInstancesModificationResult)

-- | The target Reserved Instances configurations supplied as part of the
-- modification request.
reservedInstancesModificationResult_targetConfiguration :: Lens.Lens' ReservedInstancesModificationResult (Prelude.Maybe ReservedInstancesConfiguration)
reservedInstancesModificationResult_targetConfiguration = Lens.lens (\ReservedInstancesModificationResult' {targetConfiguration} -> targetConfiguration) (\s@ReservedInstancesModificationResult' {} a -> s {targetConfiguration = a} :: ReservedInstancesModificationResult)

instance
  Data.FromXML
    ReservedInstancesModificationResult
  where
  parseXML x =
    ReservedInstancesModificationResult'
      Prelude.<$> (x Data..@? "reservedInstancesId")
      Prelude.<*> (x Data..@? "targetConfiguration")

instance
  Prelude.Hashable
    ReservedInstancesModificationResult
  where
  hashWithSalt
    _salt
    ReservedInstancesModificationResult' {..} =
      _salt `Prelude.hashWithSalt` reservedInstancesId
        `Prelude.hashWithSalt` targetConfiguration

instance
  Prelude.NFData
    ReservedInstancesModificationResult
  where
  rnf ReservedInstancesModificationResult' {..} =
    Prelude.rnf reservedInstancesId
      `Prelude.seq` Prelude.rnf targetConfiguration
