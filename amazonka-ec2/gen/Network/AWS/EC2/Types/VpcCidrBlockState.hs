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
-- Module      : Network.AWS.EC2.Types.VpcCidrBlockState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VpcCidrBlockState where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.VpcCidrBlockStateCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the state of a CIDR block.
--
-- /See:/ 'newVpcCidrBlockState' smart constructor.
data VpcCidrBlockState = VpcCidrBlockState'
  { -- | A message about the status of the CIDR block, if applicable.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The state of the CIDR block.
    state :: Prelude.Maybe VpcCidrBlockStateCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'VpcCidrBlockState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessage', 'vpcCidrBlockState_statusMessage' - A message about the status of the CIDR block, if applicable.
--
-- 'state', 'vpcCidrBlockState_state' - The state of the CIDR block.
newVpcCidrBlockState ::
  VpcCidrBlockState
newVpcCidrBlockState =
  VpcCidrBlockState'
    { statusMessage = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | A message about the status of the CIDR block, if applicable.
vpcCidrBlockState_statusMessage :: Lens.Lens' VpcCidrBlockState (Prelude.Maybe Prelude.Text)
vpcCidrBlockState_statusMessage = Lens.lens (\VpcCidrBlockState' {statusMessage} -> statusMessage) (\s@VpcCidrBlockState' {} a -> s {statusMessage = a} :: VpcCidrBlockState)

-- | The state of the CIDR block.
vpcCidrBlockState_state :: Lens.Lens' VpcCidrBlockState (Prelude.Maybe VpcCidrBlockStateCode)
vpcCidrBlockState_state = Lens.lens (\VpcCidrBlockState' {state} -> state) (\s@VpcCidrBlockState' {} a -> s {state = a} :: VpcCidrBlockState)

instance Prelude.FromXML VpcCidrBlockState where
  parseXML x =
    VpcCidrBlockState'
      Prelude.<$> (x Prelude..@? "statusMessage")
      Prelude.<*> (x Prelude..@? "state")

instance Prelude.Hashable VpcCidrBlockState

instance Prelude.NFData VpcCidrBlockState
