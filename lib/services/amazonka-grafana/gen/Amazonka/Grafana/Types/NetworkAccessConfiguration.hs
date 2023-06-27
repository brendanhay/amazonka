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
-- Module      : Amazonka.Grafana.Types.NetworkAccessConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Grafana.Types.NetworkAccessConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration settings for in-bound network access to your
-- workspace.
--
-- When this is configured, only listed IP addresses and VPC endpoints will
-- be able to access your workspace. Standard Grafana authentication and
-- authorization will still be required.
--
-- If this is not configured, or is removed, then all IP addresses and VPC
-- endpoints will be allowed. Standard Grafana authentication and
-- authorization will still be required.
--
-- /See:/ 'newNetworkAccessConfiguration' smart constructor.
data NetworkAccessConfiguration = NetworkAccessConfiguration'
  { -- | An array of prefix list IDs. A prefix list is a list of CIDR ranges of
    -- IP addresses. The IP addresses specified are allowed to access your
    -- workspace. If the list is not included in the configuration then no IP
    -- addresses will be allowed to access the workspace. You create a prefix
    -- list using the Amazon VPC console.
    --
    -- Prefix list IDs have the format @pl-@/@1a2b3c4d@/@ @.
    --
    -- For more information about prefix lists, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/managed-prefix-lists.html Group CIDR blocks using managed prefix lists>in
    -- the /Amazon Virtual Private Cloud User Guide/.
    prefixListIds :: [Prelude.Text],
    -- | An array of Amazon VPC endpoint IDs for the workspace. You can create
    -- VPC endpoints to your Amazon Managed Grafana workspace for access from
    -- within a VPC. If a @NetworkAccessConfiguration@ is specified then only
    -- VPC endpoints specified here will be allowed to access the workspace.
    --
    -- VPC endpoint IDs have the format @vpce-@/@1a2b3c4d@/@ @.
    --
    -- For more information about creating an interface VPC endpoint, see
    -- <https://docs.aws.amazon.com/grafana/latest/userguide/VPC-endpoints Interface VPC endpoints>
    -- in the /Amazon Managed Grafana User Guide/.
    --
    -- The only VPC endpoints that can be specified here are interface VPC
    -- endpoints for Grafana workspaces (using the
    -- @com.amazonaws.[region].grafana-workspace@ service endpoint). Other VPC
    -- endpoints will be ignored.
    vpceIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkAccessConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefixListIds', 'networkAccessConfiguration_prefixListIds' - An array of prefix list IDs. A prefix list is a list of CIDR ranges of
-- IP addresses. The IP addresses specified are allowed to access your
-- workspace. If the list is not included in the configuration then no IP
-- addresses will be allowed to access the workspace. You create a prefix
-- list using the Amazon VPC console.
--
-- Prefix list IDs have the format @pl-@/@1a2b3c4d@/@ @.
--
-- For more information about prefix lists, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/managed-prefix-lists.html Group CIDR blocks using managed prefix lists>in
-- the /Amazon Virtual Private Cloud User Guide/.
--
-- 'vpceIds', 'networkAccessConfiguration_vpceIds' - An array of Amazon VPC endpoint IDs for the workspace. You can create
-- VPC endpoints to your Amazon Managed Grafana workspace for access from
-- within a VPC. If a @NetworkAccessConfiguration@ is specified then only
-- VPC endpoints specified here will be allowed to access the workspace.
--
-- VPC endpoint IDs have the format @vpce-@/@1a2b3c4d@/@ @.
--
-- For more information about creating an interface VPC endpoint, see
-- <https://docs.aws.amazon.com/grafana/latest/userguide/VPC-endpoints Interface VPC endpoints>
-- in the /Amazon Managed Grafana User Guide/.
--
-- The only VPC endpoints that can be specified here are interface VPC
-- endpoints for Grafana workspaces (using the
-- @com.amazonaws.[region].grafana-workspace@ service endpoint). Other VPC
-- endpoints will be ignored.
newNetworkAccessConfiguration ::
  NetworkAccessConfiguration
newNetworkAccessConfiguration =
  NetworkAccessConfiguration'
    { prefixListIds =
        Prelude.mempty,
      vpceIds = Prelude.mempty
    }

-- | An array of prefix list IDs. A prefix list is a list of CIDR ranges of
-- IP addresses. The IP addresses specified are allowed to access your
-- workspace. If the list is not included in the configuration then no IP
-- addresses will be allowed to access the workspace. You create a prefix
-- list using the Amazon VPC console.
--
-- Prefix list IDs have the format @pl-@/@1a2b3c4d@/@ @.
--
-- For more information about prefix lists, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/managed-prefix-lists.html Group CIDR blocks using managed prefix lists>in
-- the /Amazon Virtual Private Cloud User Guide/.
networkAccessConfiguration_prefixListIds :: Lens.Lens' NetworkAccessConfiguration [Prelude.Text]
networkAccessConfiguration_prefixListIds = Lens.lens (\NetworkAccessConfiguration' {prefixListIds} -> prefixListIds) (\s@NetworkAccessConfiguration' {} a -> s {prefixListIds = a} :: NetworkAccessConfiguration) Prelude.. Lens.coerced

-- | An array of Amazon VPC endpoint IDs for the workspace. You can create
-- VPC endpoints to your Amazon Managed Grafana workspace for access from
-- within a VPC. If a @NetworkAccessConfiguration@ is specified then only
-- VPC endpoints specified here will be allowed to access the workspace.
--
-- VPC endpoint IDs have the format @vpce-@/@1a2b3c4d@/@ @.
--
-- For more information about creating an interface VPC endpoint, see
-- <https://docs.aws.amazon.com/grafana/latest/userguide/VPC-endpoints Interface VPC endpoints>
-- in the /Amazon Managed Grafana User Guide/.
--
-- The only VPC endpoints that can be specified here are interface VPC
-- endpoints for Grafana workspaces (using the
-- @com.amazonaws.[region].grafana-workspace@ service endpoint). Other VPC
-- endpoints will be ignored.
networkAccessConfiguration_vpceIds :: Lens.Lens' NetworkAccessConfiguration [Prelude.Text]
networkAccessConfiguration_vpceIds = Lens.lens (\NetworkAccessConfiguration' {vpceIds} -> vpceIds) (\s@NetworkAccessConfiguration' {} a -> s {vpceIds = a} :: NetworkAccessConfiguration) Prelude.. Lens.coerced

instance Data.FromJSON NetworkAccessConfiguration where
  parseJSON =
    Data.withObject
      "NetworkAccessConfiguration"
      ( \x ->
          NetworkAccessConfiguration'
            Prelude.<$> (x Data..:? "prefixListIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "vpceIds" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable NetworkAccessConfiguration where
  hashWithSalt _salt NetworkAccessConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` prefixListIds
      `Prelude.hashWithSalt` vpceIds

instance Prelude.NFData NetworkAccessConfiguration where
  rnf NetworkAccessConfiguration' {..} =
    Prelude.rnf prefixListIds
      `Prelude.seq` Prelude.rnf vpceIds

instance Data.ToJSON NetworkAccessConfiguration where
  toJSON NetworkAccessConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("prefixListIds" Data..= prefixListIds),
            Prelude.Just ("vpceIds" Data..= vpceIds)
          ]
      )
