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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.Network where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ManagedBlockChain.Types.Framework
import Amazonka.ManagedBlockChain.Types.NetworkFrameworkAttributes
import Amazonka.ManagedBlockChain.Types.NetworkStatus
import Amazonka.ManagedBlockChain.Types.VotingPolicy
import qualified Amazonka.Prelude as Prelude

-- | Network configuration properties.
--
-- /See:/ 'newNetwork' smart constructor.
data Network = Network'
  { -- | Tags assigned to the network. Each tag consists of a key and optional
    -- value.
    --
    -- For more information about tags, see
    -- <https://docs.aws.amazon.com/managed-blockchain/latest/ethereum-dev/tagging-resources.html Tagging Resources>
    -- in the /Amazon Managed Blockchain Ethereum Developer Guide/, or
    -- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/tagging-resources.html Tagging Resources>
    -- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the network.
    name :: Prelude.Maybe Prelude.Text,
    -- | Attributes of the blockchain framework that the network uses.
    frameworkAttributes :: Prelude.Maybe NetworkFrameworkAttributes,
    -- | The VPC endpoint service name of the VPC endpoint service of the
    -- network. Members use the VPC endpoint service name to create a VPC
    -- endpoint to access network resources.
    vpcEndpointServiceName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the network. For more information
    -- about ARNs and their format, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /Amazon Web Services General Reference/.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the network was created.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The voting rules for the network to decide if a proposal is accepted.
    votingPolicy :: Prelude.Maybe VotingPolicy,
    -- | The current status of the network.
    status :: Prelude.Maybe NetworkStatus,
    -- | The unique identifier of the network.
    id :: Prelude.Maybe Prelude.Text,
    -- | Attributes of the blockchain framework for the network.
    description :: Prelude.Maybe Prelude.Text,
    -- | The version of the blockchain framework that the network uses.
    frameworkVersion :: Prelude.Maybe Prelude.Text,
    -- | The blockchain framework that the network uses.
    framework :: Prelude.Maybe Framework
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
-- 'tags', 'network_tags' - Tags assigned to the network. Each tag consists of a key and optional
-- value.
--
-- For more information about tags, see
-- <https://docs.aws.amazon.com/managed-blockchain/latest/ethereum-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Ethereum Developer Guide/, or
-- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
--
-- 'name', 'network_name' - The name of the network.
--
-- 'frameworkAttributes', 'network_frameworkAttributes' - Attributes of the blockchain framework that the network uses.
--
-- 'vpcEndpointServiceName', 'network_vpcEndpointServiceName' - The VPC endpoint service name of the VPC endpoint service of the
-- network. Members use the VPC endpoint service name to create a VPC
-- endpoint to access network resources.
--
-- 'arn', 'network_arn' - The Amazon Resource Name (ARN) of the network. For more information
-- about ARNs and their format, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
--
-- 'creationDate', 'network_creationDate' - The date and time that the network was created.
--
-- 'votingPolicy', 'network_votingPolicy' - The voting rules for the network to decide if a proposal is accepted.
--
-- 'status', 'network_status' - The current status of the network.
--
-- 'id', 'network_id' - The unique identifier of the network.
--
-- 'description', 'network_description' - Attributes of the blockchain framework for the network.
--
-- 'frameworkVersion', 'network_frameworkVersion' - The version of the blockchain framework that the network uses.
--
-- 'framework', 'network_framework' - The blockchain framework that the network uses.
newNetwork ::
  Network
newNetwork =
  Network'
    { tags = Prelude.Nothing,
      name = Prelude.Nothing,
      frameworkAttributes = Prelude.Nothing,
      vpcEndpointServiceName = Prelude.Nothing,
      arn = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      votingPolicy = Prelude.Nothing,
      status = Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing,
      frameworkVersion = Prelude.Nothing,
      framework = Prelude.Nothing
    }

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

-- | The name of the network.
network_name :: Lens.Lens' Network (Prelude.Maybe Prelude.Text)
network_name = Lens.lens (\Network' {name} -> name) (\s@Network' {} a -> s {name = a} :: Network)

-- | Attributes of the blockchain framework that the network uses.
network_frameworkAttributes :: Lens.Lens' Network (Prelude.Maybe NetworkFrameworkAttributes)
network_frameworkAttributes = Lens.lens (\Network' {frameworkAttributes} -> frameworkAttributes) (\s@Network' {} a -> s {frameworkAttributes = a} :: Network)

-- | The VPC endpoint service name of the VPC endpoint service of the
-- network. Members use the VPC endpoint service name to create a VPC
-- endpoint to access network resources.
network_vpcEndpointServiceName :: Lens.Lens' Network (Prelude.Maybe Prelude.Text)
network_vpcEndpointServiceName = Lens.lens (\Network' {vpcEndpointServiceName} -> vpcEndpointServiceName) (\s@Network' {} a -> s {vpcEndpointServiceName = a} :: Network)

-- | The Amazon Resource Name (ARN) of the network. For more information
-- about ARNs and their format, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
network_arn :: Lens.Lens' Network (Prelude.Maybe Prelude.Text)
network_arn = Lens.lens (\Network' {arn} -> arn) (\s@Network' {} a -> s {arn = a} :: Network)

-- | The date and time that the network was created.
network_creationDate :: Lens.Lens' Network (Prelude.Maybe Prelude.UTCTime)
network_creationDate = Lens.lens (\Network' {creationDate} -> creationDate) (\s@Network' {} a -> s {creationDate = a} :: Network) Prelude.. Lens.mapping Data._Time

-- | The voting rules for the network to decide if a proposal is accepted.
network_votingPolicy :: Lens.Lens' Network (Prelude.Maybe VotingPolicy)
network_votingPolicy = Lens.lens (\Network' {votingPolicy} -> votingPolicy) (\s@Network' {} a -> s {votingPolicy = a} :: Network)

-- | The current status of the network.
network_status :: Lens.Lens' Network (Prelude.Maybe NetworkStatus)
network_status = Lens.lens (\Network' {status} -> status) (\s@Network' {} a -> s {status = a} :: Network)

-- | The unique identifier of the network.
network_id :: Lens.Lens' Network (Prelude.Maybe Prelude.Text)
network_id = Lens.lens (\Network' {id} -> id) (\s@Network' {} a -> s {id = a} :: Network)

-- | Attributes of the blockchain framework for the network.
network_description :: Lens.Lens' Network (Prelude.Maybe Prelude.Text)
network_description = Lens.lens (\Network' {description} -> description) (\s@Network' {} a -> s {description = a} :: Network)

-- | The version of the blockchain framework that the network uses.
network_frameworkVersion :: Lens.Lens' Network (Prelude.Maybe Prelude.Text)
network_frameworkVersion = Lens.lens (\Network' {frameworkVersion} -> frameworkVersion) (\s@Network' {} a -> s {frameworkVersion = a} :: Network)

-- | The blockchain framework that the network uses.
network_framework :: Lens.Lens' Network (Prelude.Maybe Framework)
network_framework = Lens.lens (\Network' {framework} -> framework) (\s@Network' {} a -> s {framework = a} :: Network)

instance Data.FromJSON Network where
  parseJSON =
    Data.withObject
      "Network"
      ( \x ->
          Network'
            Prelude.<$> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "FrameworkAttributes")
            Prelude.<*> (x Data..:? "VpcEndpointServiceName")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "VotingPolicy")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "FrameworkVersion")
            Prelude.<*> (x Data..:? "Framework")
      )

instance Prelude.Hashable Network where
  hashWithSalt _salt Network' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` frameworkAttributes
      `Prelude.hashWithSalt` vpcEndpointServiceName
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` votingPolicy
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` frameworkVersion
      `Prelude.hashWithSalt` framework

instance Prelude.NFData Network where
  rnf Network' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf frameworkAttributes
      `Prelude.seq` Prelude.rnf vpcEndpointServiceName
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf votingPolicy
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf frameworkVersion
      `Prelude.seq` Prelude.rnf framework
