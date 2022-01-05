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
-- Module      : Amazonka.ManagedBlockChain.Types.Network
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.Network where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.ManagedBlockChain.Types.Framework
import Amazonka.ManagedBlockChain.Types.NetworkFrameworkAttributes
import Amazonka.ManagedBlockChain.Types.NetworkStatus
import Amazonka.ManagedBlockChain.Types.VotingPolicy
import qualified Amazonka.Prelude as Prelude

-- | Network configuration properties.
--
-- /See:/ 'newNetwork' smart constructor.
data Network = Network'
  { -- | The current status of the network.
    status :: Prelude.Maybe NetworkStatus,
    -- | The blockchain framework that the network uses.
    framework :: Prelude.Maybe Framework,
    -- | The Amazon Resource Name (ARN) of the network. For more information
    -- about ARNs and their format, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The version of the blockchain framework that the network uses.
    frameworkVersion :: Prelude.Maybe Prelude.Text,
    -- | The VPC endpoint service name of the VPC endpoint service of the
    -- network. Members use the VPC endpoint service name to create a VPC
    -- endpoint to access network resources.
    vpcEndpointServiceName :: Prelude.Maybe Prelude.Text,
    -- | The name of the network.
    name :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the network.
    id :: Prelude.Maybe Prelude.Text,
    -- | The voting rules for the network to decide if a proposal is accepted.
    votingPolicy :: Prelude.Maybe VotingPolicy,
    -- | The date and time that the network was created.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | Attributes of the blockchain framework that the network uses.
    frameworkAttributes :: Prelude.Maybe NetworkFrameworkAttributes,
    -- | Attributes of the blockchain framework for the network.
    description :: Prelude.Maybe Prelude.Text,
    -- | Tags assigned to the network. Each tag consists of a key and optional
    -- value.
    --
    -- For more information about tags, see
    -- <https://docs.aws.amazon.com/managed-blockchain/latest/ethereum-dev/tagging-resources.html Tagging Resources>
    -- in the /Amazon Managed Blockchain Ethereum Developer Guide/, or
    -- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/tagging-resources.html Tagging Resources>
    -- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Network' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'network_status' - The current status of the network.
--
-- 'framework', 'network_framework' - The blockchain framework that the network uses.
--
-- 'arn', 'network_arn' - The Amazon Resource Name (ARN) of the network. For more information
-- about ARNs and their format, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'frameworkVersion', 'network_frameworkVersion' - The version of the blockchain framework that the network uses.
--
-- 'vpcEndpointServiceName', 'network_vpcEndpointServiceName' - The VPC endpoint service name of the VPC endpoint service of the
-- network. Members use the VPC endpoint service name to create a VPC
-- endpoint to access network resources.
--
-- 'name', 'network_name' - The name of the network.
--
-- 'id', 'network_id' - The unique identifier of the network.
--
-- 'votingPolicy', 'network_votingPolicy' - The voting rules for the network to decide if a proposal is accepted.
--
-- 'creationDate', 'network_creationDate' - The date and time that the network was created.
--
-- 'frameworkAttributes', 'network_frameworkAttributes' - Attributes of the blockchain framework that the network uses.
--
-- 'description', 'network_description' - Attributes of the blockchain framework for the network.
--
-- 'tags', 'network_tags' - Tags assigned to the network. Each tag consists of a key and optional
-- value.
--
-- For more information about tags, see
-- <https://docs.aws.amazon.com/managed-blockchain/latest/ethereum-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Ethereum Developer Guide/, or
-- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
newNetwork ::
  Network
newNetwork =
  Network'
    { status = Prelude.Nothing,
      framework = Prelude.Nothing,
      arn = Prelude.Nothing,
      frameworkVersion = Prelude.Nothing,
      vpcEndpointServiceName = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      votingPolicy = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      frameworkAttributes = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The current status of the network.
network_status :: Lens.Lens' Network (Prelude.Maybe NetworkStatus)
network_status = Lens.lens (\Network' {status} -> status) (\s@Network' {} a -> s {status = a} :: Network)

-- | The blockchain framework that the network uses.
network_framework :: Lens.Lens' Network (Prelude.Maybe Framework)
network_framework = Lens.lens (\Network' {framework} -> framework) (\s@Network' {} a -> s {framework = a} :: Network)

-- | The Amazon Resource Name (ARN) of the network. For more information
-- about ARNs and their format, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
network_arn :: Lens.Lens' Network (Prelude.Maybe Prelude.Text)
network_arn = Lens.lens (\Network' {arn} -> arn) (\s@Network' {} a -> s {arn = a} :: Network)

-- | The version of the blockchain framework that the network uses.
network_frameworkVersion :: Lens.Lens' Network (Prelude.Maybe Prelude.Text)
network_frameworkVersion = Lens.lens (\Network' {frameworkVersion} -> frameworkVersion) (\s@Network' {} a -> s {frameworkVersion = a} :: Network)

-- | The VPC endpoint service name of the VPC endpoint service of the
-- network. Members use the VPC endpoint service name to create a VPC
-- endpoint to access network resources.
network_vpcEndpointServiceName :: Lens.Lens' Network (Prelude.Maybe Prelude.Text)
network_vpcEndpointServiceName = Lens.lens (\Network' {vpcEndpointServiceName} -> vpcEndpointServiceName) (\s@Network' {} a -> s {vpcEndpointServiceName = a} :: Network)

-- | The name of the network.
network_name :: Lens.Lens' Network (Prelude.Maybe Prelude.Text)
network_name = Lens.lens (\Network' {name} -> name) (\s@Network' {} a -> s {name = a} :: Network)

-- | The unique identifier of the network.
network_id :: Lens.Lens' Network (Prelude.Maybe Prelude.Text)
network_id = Lens.lens (\Network' {id} -> id) (\s@Network' {} a -> s {id = a} :: Network)

-- | The voting rules for the network to decide if a proposal is accepted.
network_votingPolicy :: Lens.Lens' Network (Prelude.Maybe VotingPolicy)
network_votingPolicy = Lens.lens (\Network' {votingPolicy} -> votingPolicy) (\s@Network' {} a -> s {votingPolicy = a} :: Network)

-- | The date and time that the network was created.
network_creationDate :: Lens.Lens' Network (Prelude.Maybe Prelude.UTCTime)
network_creationDate = Lens.lens (\Network' {creationDate} -> creationDate) (\s@Network' {} a -> s {creationDate = a} :: Network) Prelude.. Lens.mapping Core._Time

-- | Attributes of the blockchain framework that the network uses.
network_frameworkAttributes :: Lens.Lens' Network (Prelude.Maybe NetworkFrameworkAttributes)
network_frameworkAttributes = Lens.lens (\Network' {frameworkAttributes} -> frameworkAttributes) (\s@Network' {} a -> s {frameworkAttributes = a} :: Network)

-- | Attributes of the blockchain framework for the network.
network_description :: Lens.Lens' Network (Prelude.Maybe Prelude.Text)
network_description = Lens.lens (\Network' {description} -> description) (\s@Network' {} a -> s {description = a} :: Network)

-- | Tags assigned to the network. Each tag consists of a key and optional
-- value.
--
-- For more information about tags, see
-- <https://docs.aws.amazon.com/managed-blockchain/latest/ethereum-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Ethereum Developer Guide/, or
-- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
network_tags :: Lens.Lens' Network (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
network_tags = Lens.lens (\Network' {tags} -> tags) (\s@Network' {} a -> s {tags = a} :: Network) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Network where
  parseJSON =
    Core.withObject
      "Network"
      ( \x ->
          Network'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Framework")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "FrameworkVersion")
            Prelude.<*> (x Core..:? "VpcEndpointServiceName")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "VotingPolicy")
            Prelude.<*> (x Core..:? "CreationDate")
            Prelude.<*> (x Core..:? "FrameworkAttributes")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Network where
  hashWithSalt _salt Network' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` framework
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` frameworkVersion
      `Prelude.hashWithSalt` vpcEndpointServiceName
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` votingPolicy
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` frameworkAttributes
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags

instance Prelude.NFData Network where
  rnf Network' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf framework
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf frameworkVersion
      `Prelude.seq` Prelude.rnf vpcEndpointServiceName
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf votingPolicy
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf frameworkAttributes
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
