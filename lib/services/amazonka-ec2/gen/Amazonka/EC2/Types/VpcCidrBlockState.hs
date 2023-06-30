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
-- Module      : Amazonka.EC2.Types.VpcCidrBlockState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VpcCidrBlockState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.VpcCidrBlockStateCode
import qualified Amazonka.Prelude as Prelude

-- | Describes the state of a CIDR block.
--
-- /See:/ 'newVpcCidrBlockState' smart constructor.
data VpcCidrBlockState = VpcCidrBlockState'
  { -- | The state of the CIDR block.
    state :: Prelude.Maybe VpcCidrBlockStateCode,
    -- | A message about the status of the CIDR block, if applicable.
    statusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcCidrBlockState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'vpcCidrBlockState_state' - The state of the CIDR block.
--
-- 'statusMessage', 'vpcCidrBlockState_statusMessage' - A message about the status of the CIDR block, if applicable.
newVpcCidrBlockState ::
  VpcCidrBlockState
newVpcCidrBlockState =
  VpcCidrBlockState'
    { state = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | The state of the CIDR block.
vpcCidrBlockState_state :: Lens.Lens' VpcCidrBlockState (Prelude.Maybe VpcCidrBlockStateCode)
vpcCidrBlockState_state = Lens.lens (\VpcCidrBlockState' {state} -> state) (\s@VpcCidrBlockState' {} a -> s {state = a} :: VpcCidrBlockState)

-- | A message about the status of the CIDR block, if applicable.
vpcCidrBlockState_statusMessage :: Lens.Lens' VpcCidrBlockState (Prelude.Maybe Prelude.Text)
vpcCidrBlockState_statusMessage = Lens.lens (\VpcCidrBlockState' {statusMessage} -> statusMessage) (\s@VpcCidrBlockState' {} a -> s {statusMessage = a} :: VpcCidrBlockState)

instance Data.FromXML VpcCidrBlockState where
  parseXML x =
    VpcCidrBlockState'
      Prelude.<$> (x Data..@? "state")
      Prelude.<*> (x Data..@? "statusMessage")

instance Prelude.Hashable VpcCidrBlockState where
  hashWithSalt _salt VpcCidrBlockState' {..} =
    _salt
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData VpcCidrBlockState where
  rnf VpcCidrBlockState' {..} =
    Prelude.rnf state
      `Prelude.seq` Prelude.rnf statusMessage
