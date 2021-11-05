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
-- Module      : Network.AWS.ManagedBlockChain.Types.MemberLogPublishingConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ManagedBlockChain.Types.MemberLogPublishingConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.ManagedBlockChain.Types.MemberFabricLogPublishingConfiguration
import qualified Network.AWS.Prelude as Prelude

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
  Core.FromJSON
    MemberLogPublishingConfiguration
  where
  parseJSON =
    Core.withObject
      "MemberLogPublishingConfiguration"
      ( \x ->
          MemberLogPublishingConfiguration'
            Prelude.<$> (x Core..:? "Fabric")
      )

instance
  Prelude.Hashable
    MemberLogPublishingConfiguration

instance
  Prelude.NFData
    MemberLogPublishingConfiguration

instance Core.ToJSON MemberLogPublishingConfiguration where
  toJSON MemberLogPublishingConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Fabric" Core..=) Prelude.<$> fabric]
      )
