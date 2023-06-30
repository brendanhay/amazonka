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
-- Module      : Amazonka.MigrationHubReFactorSpaces.Types.EnvironmentVpc
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubReFactorSpaces.Types.EnvironmentVpc where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides summary information for the @EnvironmentVpc@ resource as a
-- response to @ListEnvironmentVpc@.
--
-- /See:/ 'newEnvironmentVpc' smart constructor.
data EnvironmentVpc = EnvironmentVpc'
  { -- | The Amazon Web Services account ID of the virtual private cloud (VPC)
    -- owner.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The list of Amazon Virtual Private Cloud (Amazon VPC) CIDR blocks.
    cidrBlocks :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A timestamp that indicates when the VPC is first added to the
    -- environment.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The unique identifier of the environment.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | A timestamp that indicates when the VPC was last updated by the
    -- environment.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | The ID of the VPC.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The name of the VPC at the time it is added to the environment.
    vpcName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentVpc' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'environmentVpc_accountId' - The Amazon Web Services account ID of the virtual private cloud (VPC)
-- owner.
--
-- 'cidrBlocks', 'environmentVpc_cidrBlocks' - The list of Amazon Virtual Private Cloud (Amazon VPC) CIDR blocks.
--
-- 'createdTime', 'environmentVpc_createdTime' - A timestamp that indicates when the VPC is first added to the
-- environment.
--
-- 'environmentId', 'environmentVpc_environmentId' - The unique identifier of the environment.
--
-- 'lastUpdatedTime', 'environmentVpc_lastUpdatedTime' - A timestamp that indicates when the VPC was last updated by the
-- environment.
--
-- 'vpcId', 'environmentVpc_vpcId' - The ID of the VPC.
--
-- 'vpcName', 'environmentVpc_vpcName' - The name of the VPC at the time it is added to the environment.
newEnvironmentVpc ::
  EnvironmentVpc
newEnvironmentVpc =
  EnvironmentVpc'
    { accountId = Prelude.Nothing,
      cidrBlocks = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      environmentId = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      vpcName = Prelude.Nothing
    }

-- | The Amazon Web Services account ID of the virtual private cloud (VPC)
-- owner.
environmentVpc_accountId :: Lens.Lens' EnvironmentVpc (Prelude.Maybe Prelude.Text)
environmentVpc_accountId = Lens.lens (\EnvironmentVpc' {accountId} -> accountId) (\s@EnvironmentVpc' {} a -> s {accountId = a} :: EnvironmentVpc)

-- | The list of Amazon Virtual Private Cloud (Amazon VPC) CIDR blocks.
environmentVpc_cidrBlocks :: Lens.Lens' EnvironmentVpc (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
environmentVpc_cidrBlocks = Lens.lens (\EnvironmentVpc' {cidrBlocks} -> cidrBlocks) (\s@EnvironmentVpc' {} a -> s {cidrBlocks = a} :: EnvironmentVpc) Prelude.. Lens.mapping Lens.coerced

-- | A timestamp that indicates when the VPC is first added to the
-- environment.
environmentVpc_createdTime :: Lens.Lens' EnvironmentVpc (Prelude.Maybe Prelude.UTCTime)
environmentVpc_createdTime = Lens.lens (\EnvironmentVpc' {createdTime} -> createdTime) (\s@EnvironmentVpc' {} a -> s {createdTime = a} :: EnvironmentVpc) Prelude.. Lens.mapping Data._Time

-- | The unique identifier of the environment.
environmentVpc_environmentId :: Lens.Lens' EnvironmentVpc (Prelude.Maybe Prelude.Text)
environmentVpc_environmentId = Lens.lens (\EnvironmentVpc' {environmentId} -> environmentId) (\s@EnvironmentVpc' {} a -> s {environmentId = a} :: EnvironmentVpc)

-- | A timestamp that indicates when the VPC was last updated by the
-- environment.
environmentVpc_lastUpdatedTime :: Lens.Lens' EnvironmentVpc (Prelude.Maybe Prelude.UTCTime)
environmentVpc_lastUpdatedTime = Lens.lens (\EnvironmentVpc' {lastUpdatedTime} -> lastUpdatedTime) (\s@EnvironmentVpc' {} a -> s {lastUpdatedTime = a} :: EnvironmentVpc) Prelude.. Lens.mapping Data._Time

-- | The ID of the VPC.
environmentVpc_vpcId :: Lens.Lens' EnvironmentVpc (Prelude.Maybe Prelude.Text)
environmentVpc_vpcId = Lens.lens (\EnvironmentVpc' {vpcId} -> vpcId) (\s@EnvironmentVpc' {} a -> s {vpcId = a} :: EnvironmentVpc)

-- | The name of the VPC at the time it is added to the environment.
environmentVpc_vpcName :: Lens.Lens' EnvironmentVpc (Prelude.Maybe Prelude.Text)
environmentVpc_vpcName = Lens.lens (\EnvironmentVpc' {vpcName} -> vpcName) (\s@EnvironmentVpc' {} a -> s {vpcName = a} :: EnvironmentVpc)

instance Data.FromJSON EnvironmentVpc where
  parseJSON =
    Data.withObject
      "EnvironmentVpc"
      ( \x ->
          EnvironmentVpc'
            Prelude.<$> (x Data..:? "AccountId")
            Prelude.<*> (x Data..:? "CidrBlocks")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "EnvironmentId")
            Prelude.<*> (x Data..:? "LastUpdatedTime")
            Prelude.<*> (x Data..:? "VpcId")
            Prelude.<*> (x Data..:? "VpcName")
      )

instance Prelude.Hashable EnvironmentVpc where
  hashWithSalt _salt EnvironmentVpc' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` cidrBlocks
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` vpcName

instance Prelude.NFData EnvironmentVpc where
  rnf EnvironmentVpc' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf cidrBlocks
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf vpcName
