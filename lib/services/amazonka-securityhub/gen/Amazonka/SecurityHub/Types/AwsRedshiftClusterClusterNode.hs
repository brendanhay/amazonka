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
-- Module      : Amazonka.SecurityHub.Types.AwsRedshiftClusterClusterNode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRedshiftClusterClusterNode where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A node in an Amazon Redshift cluster.
--
-- /See:/ 'newAwsRedshiftClusterClusterNode' smart constructor.
data AwsRedshiftClusterClusterNode = AwsRedshiftClusterClusterNode'
  { -- | The role of the node. A node might be a leader node or a compute node.
    nodeRole :: Prelude.Maybe Prelude.Text,
    -- | The private IP address of the node.
    privateIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The public IP address of the node.
    publicIpAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRedshiftClusterClusterNode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nodeRole', 'awsRedshiftClusterClusterNode_nodeRole' - The role of the node. A node might be a leader node or a compute node.
--
-- 'privateIpAddress', 'awsRedshiftClusterClusterNode_privateIpAddress' - The private IP address of the node.
--
-- 'publicIpAddress', 'awsRedshiftClusterClusterNode_publicIpAddress' - The public IP address of the node.
newAwsRedshiftClusterClusterNode ::
  AwsRedshiftClusterClusterNode
newAwsRedshiftClusterClusterNode =
  AwsRedshiftClusterClusterNode'
    { nodeRole =
        Prelude.Nothing,
      privateIpAddress = Prelude.Nothing,
      publicIpAddress = Prelude.Nothing
    }

-- | The role of the node. A node might be a leader node or a compute node.
awsRedshiftClusterClusterNode_nodeRole :: Lens.Lens' AwsRedshiftClusterClusterNode (Prelude.Maybe Prelude.Text)
awsRedshiftClusterClusterNode_nodeRole = Lens.lens (\AwsRedshiftClusterClusterNode' {nodeRole} -> nodeRole) (\s@AwsRedshiftClusterClusterNode' {} a -> s {nodeRole = a} :: AwsRedshiftClusterClusterNode)

-- | The private IP address of the node.
awsRedshiftClusterClusterNode_privateIpAddress :: Lens.Lens' AwsRedshiftClusterClusterNode (Prelude.Maybe Prelude.Text)
awsRedshiftClusterClusterNode_privateIpAddress = Lens.lens (\AwsRedshiftClusterClusterNode' {privateIpAddress} -> privateIpAddress) (\s@AwsRedshiftClusterClusterNode' {} a -> s {privateIpAddress = a} :: AwsRedshiftClusterClusterNode)

-- | The public IP address of the node.
awsRedshiftClusterClusterNode_publicIpAddress :: Lens.Lens' AwsRedshiftClusterClusterNode (Prelude.Maybe Prelude.Text)
awsRedshiftClusterClusterNode_publicIpAddress = Lens.lens (\AwsRedshiftClusterClusterNode' {publicIpAddress} -> publicIpAddress) (\s@AwsRedshiftClusterClusterNode' {} a -> s {publicIpAddress = a} :: AwsRedshiftClusterClusterNode)

instance Data.FromJSON AwsRedshiftClusterClusterNode where
  parseJSON =
    Data.withObject
      "AwsRedshiftClusterClusterNode"
      ( \x ->
          AwsRedshiftClusterClusterNode'
            Prelude.<$> (x Data..:? "NodeRole")
            Prelude.<*> (x Data..:? "PrivateIpAddress")
            Prelude.<*> (x Data..:? "PublicIpAddress")
      )

instance
  Prelude.Hashable
    AwsRedshiftClusterClusterNode
  where
  hashWithSalt _salt AwsRedshiftClusterClusterNode' {..} =
    _salt `Prelude.hashWithSalt` nodeRole
      `Prelude.hashWithSalt` privateIpAddress
      `Prelude.hashWithSalt` publicIpAddress

instance Prelude.NFData AwsRedshiftClusterClusterNode where
  rnf AwsRedshiftClusterClusterNode' {..} =
    Prelude.rnf nodeRole
      `Prelude.seq` Prelude.rnf privateIpAddress
      `Prelude.seq` Prelude.rnf publicIpAddress

instance Data.ToJSON AwsRedshiftClusterClusterNode where
  toJSON AwsRedshiftClusterClusterNode' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NodeRole" Data..=) Prelude.<$> nodeRole,
            ("PrivateIpAddress" Data..=)
              Prelude.<$> privateIpAddress,
            ("PublicIpAddress" Data..=)
              Prelude.<$> publicIpAddress
          ]
      )
