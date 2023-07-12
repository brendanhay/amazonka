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
-- Module      : Amazonka.ManagedBlockChain.Types.MemberLogPublishingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.MemberLogPublishingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ManagedBlockChain.Types.MemberFabricLogPublishingConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Configuration properties for logging events associated with a member of
-- a Managed Blockchain network.
--
-- /See:/ 'newMemberLogPublishingConfiguration' smart constructor.
data MemberLogPublishingConfiguration = MemberLogPublishingConfiguration'
  { -- | Configuration properties for logging events associated with a member of
    -- a Managed Blockchain network using the Hyperledger Fabric framework.
    fabric :: Prelude.Maybe MemberFabricLogPublishingConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MemberLogPublishingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fabric', 'memberLogPublishingConfiguration_fabric' - Configuration properties for logging events associated with a member of
-- a Managed Blockchain network using the Hyperledger Fabric framework.
newMemberLogPublishingConfiguration ::
  MemberLogPublishingConfiguration
newMemberLogPublishingConfiguration =
  MemberLogPublishingConfiguration'
    { fabric =
        Prelude.Nothing
    }

-- | Configuration properties for logging events associated with a member of
-- a Managed Blockchain network using the Hyperledger Fabric framework.
memberLogPublishingConfiguration_fabric :: Lens.Lens' MemberLogPublishingConfiguration (Prelude.Maybe MemberFabricLogPublishingConfiguration)
memberLogPublishingConfiguration_fabric = Lens.lens (\MemberLogPublishingConfiguration' {fabric} -> fabric) (\s@MemberLogPublishingConfiguration' {} a -> s {fabric = a} :: MemberLogPublishingConfiguration)

instance
  Data.FromJSON
    MemberLogPublishingConfiguration
  where
  parseJSON =
    Data.withObject
      "MemberLogPublishingConfiguration"
      ( \x ->
          MemberLogPublishingConfiguration'
            Prelude.<$> (x Data..:? "Fabric")
      )

instance
  Prelude.Hashable
    MemberLogPublishingConfiguration
  where
  hashWithSalt
    _salt
    MemberLogPublishingConfiguration' {..} =
      _salt `Prelude.hashWithSalt` fabric

instance
  Prelude.NFData
    MemberLogPublishingConfiguration
  where
  rnf MemberLogPublishingConfiguration' {..} =
    Prelude.rnf fabric

instance Data.ToJSON MemberLogPublishingConfiguration where
  toJSON MemberLogPublishingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Fabric" Data..=) Prelude.<$> fabric]
      )
