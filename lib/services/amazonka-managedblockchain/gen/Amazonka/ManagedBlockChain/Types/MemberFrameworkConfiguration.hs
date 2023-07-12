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
-- Module      : Amazonka.ManagedBlockChain.Types.MemberFrameworkConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.MemberFrameworkConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ManagedBlockChain.Types.MemberFabricConfiguration
import qualified Amazonka.Prelude as Prelude

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
  where
  hashWithSalt _salt MemberFrameworkConfiguration' {..} =
    _salt `Prelude.hashWithSalt` fabric

instance Prelude.NFData MemberFrameworkConfiguration where
  rnf MemberFrameworkConfiguration' {..} =
    Prelude.rnf fabric

instance Data.ToJSON MemberFrameworkConfiguration where
  toJSON MemberFrameworkConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Fabric" Data..=) Prelude.<$> fabric]
      )
