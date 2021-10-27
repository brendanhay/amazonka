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
-- Module      : Network.AWS.ManagedBlockChain.Types.MemberFrameworkConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ManagedBlockChain.Types.MemberFrameworkConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.ManagedBlockChain.Types.MemberFabricConfiguration
import qualified Network.AWS.Prelude as Prelude

-- | Configuration properties relevant to a member for the blockchain
-- framework that the Managed Blockchain network uses.
--
-- /See:/ 'newMemberFrameworkConfiguration' smart constructor.
data MemberFrameworkConfiguration = MemberFrameworkConfiguration'
  { -- | Attributes of Hyperledger Fabric for a member on a Managed Blockchain
    -- network that uses Hyperledger Fabric.
    fabric :: Prelude.Maybe MemberFabricConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MemberFrameworkConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fabric', 'memberFrameworkConfiguration_fabric' - Attributes of Hyperledger Fabric for a member on a Managed Blockchain
-- network that uses Hyperledger Fabric.
newMemberFrameworkConfiguration ::
  MemberFrameworkConfiguration
newMemberFrameworkConfiguration =
  MemberFrameworkConfiguration'
    { fabric =
        Prelude.Nothing
    }

-- | Attributes of Hyperledger Fabric for a member on a Managed Blockchain
-- network that uses Hyperledger Fabric.
memberFrameworkConfiguration_fabric :: Lens.Lens' MemberFrameworkConfiguration (Prelude.Maybe MemberFabricConfiguration)
memberFrameworkConfiguration_fabric = Lens.lens (\MemberFrameworkConfiguration' {fabric} -> fabric) (\s@MemberFrameworkConfiguration' {} a -> s {fabric = a} :: MemberFrameworkConfiguration)

instance
  Prelude.Hashable
    MemberFrameworkConfiguration

instance Prelude.NFData MemberFrameworkConfiguration

instance Core.ToJSON MemberFrameworkConfiguration where
  toJSON MemberFrameworkConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Fabric" Core..=) Prelude.<$> fabric]
      )
