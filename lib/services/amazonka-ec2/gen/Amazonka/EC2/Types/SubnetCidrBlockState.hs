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
-- Module      : Amazonka.EC2.Types.SubnetCidrBlockState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SubnetCidrBlockState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.SubnetCidrBlockStateCode
import qualified Amazonka.Prelude as Prelude

-- | Describes the state of a CIDR block.
--
-- /See:/ 'newSubnetCidrBlockState' smart constructor.
data SubnetCidrBlockState = SubnetCidrBlockState'
  { -- | The state of a CIDR block.
    state :: Prelude.Maybe SubnetCidrBlockStateCode,
    -- | A message about the status of the CIDR block, if applicable.
    statusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SubnetCidrBlockState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'subnetCidrBlockState_state' - The state of a CIDR block.
--
-- 'statusMessage', 'subnetCidrBlockState_statusMessage' - A message about the status of the CIDR block, if applicable.
newSubnetCidrBlockState ::
  SubnetCidrBlockState
newSubnetCidrBlockState =
  SubnetCidrBlockState'
    { state = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | The state of a CIDR block.
subnetCidrBlockState_state :: Lens.Lens' SubnetCidrBlockState (Prelude.Maybe SubnetCidrBlockStateCode)
subnetCidrBlockState_state = Lens.lens (\SubnetCidrBlockState' {state} -> state) (\s@SubnetCidrBlockState' {} a -> s {state = a} :: SubnetCidrBlockState)

-- | A message about the status of the CIDR block, if applicable.
subnetCidrBlockState_statusMessage :: Lens.Lens' SubnetCidrBlockState (Prelude.Maybe Prelude.Text)
subnetCidrBlockState_statusMessage = Lens.lens (\SubnetCidrBlockState' {statusMessage} -> statusMessage) (\s@SubnetCidrBlockState' {} a -> s {statusMessage = a} :: SubnetCidrBlockState)

instance Data.FromXML SubnetCidrBlockState where
  parseXML x =
    SubnetCidrBlockState'
      Prelude.<$> (x Data..@? "state")
      Prelude.<*> (x Data..@? "statusMessage")

instance Prelude.Hashable SubnetCidrBlockState where
  hashWithSalt _salt SubnetCidrBlockState' {..} =
    _salt `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData SubnetCidrBlockState where
  rnf SubnetCidrBlockState' {..} =
    Prelude.rnf state
      `Prelude.seq` Prelude.rnf statusMessage
