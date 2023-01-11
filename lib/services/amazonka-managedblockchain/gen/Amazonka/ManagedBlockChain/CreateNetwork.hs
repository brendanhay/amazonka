{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ManagedBlockChain.CreateNetwork
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new blockchain network using Amazon Managed Blockchain.
--
-- Applies only to Hyperledger Fabric.
module Amazonka.ManagedBlockChain.CreateNetwork
  ( -- * Creating a Request
    CreateNetwork (..),
    newCreateNetwork,

    -- * Request Lenses
    createNetwork_description,
    createNetwork_frameworkConfiguration,
    createNetwork_tags,
    createNetwork_clientRequestToken,
    createNetwork_name,
    createNetwork_framework,
    createNetwork_frameworkVersion,
    createNetwork_votingPolicy,
    createNetwork_memberConfiguration,

    -- * Destructuring the Response
    CreateNetworkResponse (..),
    newCreateNetworkResponse,

    -- * Response Lenses
    createNetworkResponse_memberId,
    createNetworkResponse_networkId,
    createNetworkResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ManagedBlockChain.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateNetwork' smart constructor.
data CreateNetwork = CreateNetwork'
  { -- | An optional description for the network.
    description :: Prelude.Maybe Prelude.Text,
    -- | Configuration properties of the blockchain framework relevant to the
    -- network configuration.
    frameworkConfiguration :: Prelude.Maybe NetworkFrameworkConfiguration,
    -- | Tags to assign to the network. Each tag consists of a key and optional
    -- value.
    --
    -- When specifying tags during creation, you can specify multiple key-value
    -- pairs in a single request, with an overall maximum of 50 tags added to
    -- each resource.
    --
    -- For more information about tags, see
    -- <https://docs.aws.amazon.com/managed-blockchain/latest/ethereum-dev/tagging-resources.html Tagging Resources>
    -- in the /Amazon Managed Blockchain Ethereum Developer Guide/, or
    -- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/tagging-resources.html Tagging Resources>
    -- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | This is a unique, case-sensitive identifier that you provide to ensure
    -- the idempotency of the operation. An idempotent operation completes no
    -- more than once. This identifier is required only if you make a service
    -- request directly using an HTTP client. It is generated automatically if
    -- you use an Amazon Web Services SDK or the Amazon Web Services CLI.
    clientRequestToken :: Prelude.Text,
    -- | The name of the network.
    name :: Prelude.Text,
    -- | The blockchain framework that the network uses.
    framework :: Framework,
    -- | The version of the blockchain framework that the network uses.
    frameworkVersion :: Prelude.Text,
    -- | The voting rules used by the network to determine if a proposal is
    -- approved.
    votingPolicy :: VotingPolicy,
    -- | Configuration properties for the first member within the network.
    memberConfiguration :: MemberConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNetwork' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createNetwork_description' - An optional description for the network.
--
-- 'frameworkConfiguration', 'createNetwork_frameworkConfiguration' - Configuration properties of the blockchain framework relevant to the
-- network configuration.
--
-- 'tags', 'createNetwork_tags' - Tags to assign to the network. Each tag consists of a key and optional
-- value.
--
-- When specifying tags during creation, you can specify multiple key-value
-- pairs in a single request, with an overall maximum of 50 tags added to
-- each resource.
--
-- For more information about tags, see
-- <https://docs.aws.amazon.com/managed-blockchain/latest/ethereum-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Ethereum Developer Guide/, or
-- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
--
-- 'clientRequestToken', 'createNetwork_clientRequestToken' - This is a unique, case-sensitive identifier that you provide to ensure
-- the idempotency of the operation. An idempotent operation completes no
-- more than once. This identifier is required only if you make a service
-- request directly using an HTTP client. It is generated automatically if
-- you use an Amazon Web Services SDK or the Amazon Web Services CLI.
--
-- 'name', 'createNetwork_name' - The name of the network.
--
-- 'framework', 'createNetwork_framework' - The blockchain framework that the network uses.
--
-- 'frameworkVersion', 'createNetwork_frameworkVersion' - The version of the blockchain framework that the network uses.
--
-- 'votingPolicy', 'createNetwork_votingPolicy' - The voting rules used by the network to determine if a proposal is
-- approved.
--
-- 'memberConfiguration', 'createNetwork_memberConfiguration' - Configuration properties for the first member within the network.
newCreateNetwork ::
  -- | 'clientRequestToken'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'framework'
  Framework ->
  -- | 'frameworkVersion'
  Prelude.Text ->
  -- | 'votingPolicy'
  VotingPolicy ->
  -- | 'memberConfiguration'
  MemberConfiguration ->
  CreateNetwork
newCreateNetwork
  pClientRequestToken_
  pName_
  pFramework_
  pFrameworkVersion_
  pVotingPolicy_
  pMemberConfiguration_ =
    CreateNetwork'
      { description = Prelude.Nothing,
        frameworkConfiguration = Prelude.Nothing,
        tags = Prelude.Nothing,
        clientRequestToken = pClientRequestToken_,
        name = pName_,
        framework = pFramework_,
        frameworkVersion = pFrameworkVersion_,
        votingPolicy = pVotingPolicy_,
        memberConfiguration = pMemberConfiguration_
      }

-- | An optional description for the network.
createNetwork_description :: Lens.Lens' CreateNetwork (Prelude.Maybe Prelude.Text)
createNetwork_description = Lens.lens (\CreateNetwork' {description} -> description) (\s@CreateNetwork' {} a -> s {description = a} :: CreateNetwork)

-- | Configuration properties of the blockchain framework relevant to the
-- network configuration.
createNetwork_frameworkConfiguration :: Lens.Lens' CreateNetwork (Prelude.Maybe NetworkFrameworkConfiguration)
createNetwork_frameworkConfiguration = Lens.lens (\CreateNetwork' {frameworkConfiguration} -> frameworkConfiguration) (\s@CreateNetwork' {} a -> s {frameworkConfiguration = a} :: CreateNetwork)

-- | Tags to assign to the network. Each tag consists of a key and optional
-- value.
--
-- When specifying tags during creation, you can specify multiple key-value
-- pairs in a single request, with an overall maximum of 50 tags added to
-- each resource.
--
-- For more information about tags, see
-- <https://docs.aws.amazon.com/managed-blockchain/latest/ethereum-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Ethereum Developer Guide/, or
-- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
createNetwork_tags :: Lens.Lens' CreateNetwork (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createNetwork_tags = Lens.lens (\CreateNetwork' {tags} -> tags) (\s@CreateNetwork' {} a -> s {tags = a} :: CreateNetwork) Prelude.. Lens.mapping Lens.coerced

-- | This is a unique, case-sensitive identifier that you provide to ensure
-- the idempotency of the operation. An idempotent operation completes no
-- more than once. This identifier is required only if you make a service
-- request directly using an HTTP client. It is generated automatically if
-- you use an Amazon Web Services SDK or the Amazon Web Services CLI.
createNetwork_clientRequestToken :: Lens.Lens' CreateNetwork Prelude.Text
createNetwork_clientRequestToken = Lens.lens (\CreateNetwork' {clientRequestToken} -> clientRequestToken) (\s@CreateNetwork' {} a -> s {clientRequestToken = a} :: CreateNetwork)

-- | The name of the network.
createNetwork_name :: Lens.Lens' CreateNetwork Prelude.Text
createNetwork_name = Lens.lens (\CreateNetwork' {name} -> name) (\s@CreateNetwork' {} a -> s {name = a} :: CreateNetwork)

-- | The blockchain framework that the network uses.
createNetwork_framework :: Lens.Lens' CreateNetwork Framework
createNetwork_framework = Lens.lens (\CreateNetwork' {framework} -> framework) (\s@CreateNetwork' {} a -> s {framework = a} :: CreateNetwork)

-- | The version of the blockchain framework that the network uses.
createNetwork_frameworkVersion :: Lens.Lens' CreateNetwork Prelude.Text
createNetwork_frameworkVersion = Lens.lens (\CreateNetwork' {frameworkVersion} -> frameworkVersion) (\s@CreateNetwork' {} a -> s {frameworkVersion = a} :: CreateNetwork)

-- | The voting rules used by the network to determine if a proposal is
-- approved.
createNetwork_votingPolicy :: Lens.Lens' CreateNetwork VotingPolicy
createNetwork_votingPolicy = Lens.lens (\CreateNetwork' {votingPolicy} -> votingPolicy) (\s@CreateNetwork' {} a -> s {votingPolicy = a} :: CreateNetwork)

-- | Configuration properties for the first member within the network.
createNetwork_memberConfiguration :: Lens.Lens' CreateNetwork MemberConfiguration
createNetwork_memberConfiguration = Lens.lens (\CreateNetwork' {memberConfiguration} -> memberConfiguration) (\s@CreateNetwork' {} a -> s {memberConfiguration = a} :: CreateNetwork)

instance Core.AWSRequest CreateNetwork where
  type
    AWSResponse CreateNetwork =
      CreateNetworkResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateNetworkResponse'
            Prelude.<$> (x Data..?> "MemberId")
            Prelude.<*> (x Data..?> "NetworkId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateNetwork where
  hashWithSalt _salt CreateNetwork' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` frameworkConfiguration
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` framework
      `Prelude.hashWithSalt` frameworkVersion
      `Prelude.hashWithSalt` votingPolicy
      `Prelude.hashWithSalt` memberConfiguration

instance Prelude.NFData CreateNetwork where
  rnf CreateNetwork' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf frameworkConfiguration
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf framework
      `Prelude.seq` Prelude.rnf frameworkVersion
      `Prelude.seq` Prelude.rnf votingPolicy
      `Prelude.seq` Prelude.rnf memberConfiguration

instance Data.ToHeaders CreateNetwork where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateNetwork where
  toJSON CreateNetwork' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("FrameworkConfiguration" Data..=)
              Prelude.<$> frameworkConfiguration,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("ClientRequestToken" Data..= clientRequestToken),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Framework" Data..= framework),
            Prelude.Just
              ("FrameworkVersion" Data..= frameworkVersion),
            Prelude.Just ("VotingPolicy" Data..= votingPolicy),
            Prelude.Just
              ("MemberConfiguration" Data..= memberConfiguration)
          ]
      )

instance Data.ToPath CreateNetwork where
  toPath = Prelude.const "/networks"

instance Data.ToQuery CreateNetwork where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateNetworkResponse' smart constructor.
data CreateNetworkResponse = CreateNetworkResponse'
  { -- | The unique identifier for the first member within the network.
    memberId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the network.
    networkId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNetworkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memberId', 'createNetworkResponse_memberId' - The unique identifier for the first member within the network.
--
-- 'networkId', 'createNetworkResponse_networkId' - The unique identifier for the network.
--
-- 'httpStatus', 'createNetworkResponse_httpStatus' - The response's http status code.
newCreateNetworkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateNetworkResponse
newCreateNetworkResponse pHttpStatus_ =
  CreateNetworkResponse'
    { memberId = Prelude.Nothing,
      networkId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier for the first member within the network.
createNetworkResponse_memberId :: Lens.Lens' CreateNetworkResponse (Prelude.Maybe Prelude.Text)
createNetworkResponse_memberId = Lens.lens (\CreateNetworkResponse' {memberId} -> memberId) (\s@CreateNetworkResponse' {} a -> s {memberId = a} :: CreateNetworkResponse)

-- | The unique identifier for the network.
createNetworkResponse_networkId :: Lens.Lens' CreateNetworkResponse (Prelude.Maybe Prelude.Text)
createNetworkResponse_networkId = Lens.lens (\CreateNetworkResponse' {networkId} -> networkId) (\s@CreateNetworkResponse' {} a -> s {networkId = a} :: CreateNetworkResponse)

-- | The response's http status code.
createNetworkResponse_httpStatus :: Lens.Lens' CreateNetworkResponse Prelude.Int
createNetworkResponse_httpStatus = Lens.lens (\CreateNetworkResponse' {httpStatus} -> httpStatus) (\s@CreateNetworkResponse' {} a -> s {httpStatus = a} :: CreateNetworkResponse)

instance Prelude.NFData CreateNetworkResponse where
  rnf CreateNetworkResponse' {..} =
    Prelude.rnf memberId
      `Prelude.seq` Prelude.rnf networkId
      `Prelude.seq` Prelude.rnf httpStatus
