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
-- Module      : Amazonka.ManagedBlockChain.Types.Member
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.Member where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ManagedBlockChain.Types.MemberFrameworkAttributes
import Amazonka.ManagedBlockChain.Types.MemberLogPublishingConfiguration
import Amazonka.ManagedBlockChain.Types.MemberStatus
import qualified Amazonka.Prelude as Prelude

-- | Member configuration properties.
--
-- Applies only to Hyperledger Fabric.
--
-- /See:/ 'newMember' smart constructor.
data Member = Member'
  { -- | The Amazon Resource Name (ARN) of the member. For more information about
    -- ARNs and their format, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /Amazon Web Services General Reference/.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the member was created.
    creationDate :: Prelude.Maybe Data.ISO8601,
    -- | An optional description for the member.
    description :: Prelude.Maybe Prelude.Text,
    -- | Attributes relevant to a member for the blockchain framework that the
    -- Managed Blockchain network uses.
    frameworkAttributes :: Prelude.Maybe MemberFrameworkAttributes,
    -- | The unique identifier of the member.
    id :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the customer managed key in Key
    -- Management Service (KMS) that the member uses for encryption at rest. If
    -- the value of this parameter is @\"AWS Owned KMS Key\"@, the member uses
    -- an Amazon Web Services owned KMS key for encryption. This parameter is
    -- inherited by the nodes that this member owns.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/managed-blockchain-encryption-at-rest.html Encryption at Rest>
    -- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | Configuration properties for logging events associated with a member.
    logPublishingConfiguration :: Prelude.Maybe MemberLogPublishingConfiguration,
    -- | The name of the member.
    name :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the network to which the member belongs.
    networkId :: Prelude.Maybe Prelude.Text,
    -- | The status of a member.
    --
    -- -   @CREATING@ - The Amazon Web Services account is in the process of
    --     creating a member.
    --
    -- -   @AVAILABLE@ - The member has been created and can participate in the
    --     network.
    --
    -- -   @CREATE_FAILED@ - The Amazon Web Services account attempted to
    --     create a member and creation failed.
    --
    -- -   @UPDATING@ - The member is in the process of being updated.
    --
    -- -   @DELETING@ - The member and all associated resources are in the
    --     process of being deleted. Either the Amazon Web Services account
    --     that owns the member deleted it, or the member is being deleted as
    --     the result of an @APPROVED@ @PROPOSAL@ to remove the member.
    --
    -- -   @DELETED@ - The member can no longer participate on the network and
    --     all associated resources are deleted. Either the Amazon Web Services
    --     account that owns the member deleted it, or the member is being
    --     deleted as the result of an @APPROVED@ @PROPOSAL@ to remove the
    --     member.
    --
    -- -   @INACCESSIBLE_ENCRYPTION_KEY@ - The member is impaired and might not
    --     function as expected because it cannot access the specified customer
    --     managed key in KMS for encryption at rest. Either the KMS key was
    --     disabled or deleted, or the grants on the key were revoked.
    --
    --     The effect of disabling or deleting a key or of revoking a grant
    --     isn\'t immediate. It might take some time for the member resource to
    --     discover that the key is inaccessible. When a resource is in this
    --     state, we recommend deleting and recreating the resource.
    status :: Prelude.Maybe MemberStatus,
    -- | Tags assigned to the member. Tags consist of a key and optional value.
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
-- Create a value of 'Member' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'member_arn' - The Amazon Resource Name (ARN) of the member. For more information about
-- ARNs and their format, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
--
-- 'creationDate', 'member_creationDate' - The date and time that the member was created.
--
-- 'description', 'member_description' - An optional description for the member.
--
-- 'frameworkAttributes', 'member_frameworkAttributes' - Attributes relevant to a member for the blockchain framework that the
-- Managed Blockchain network uses.
--
-- 'id', 'member_id' - The unique identifier of the member.
--
-- 'kmsKeyArn', 'member_kmsKeyArn' - The Amazon Resource Name (ARN) of the customer managed key in Key
-- Management Service (KMS) that the member uses for encryption at rest. If
-- the value of this parameter is @\"AWS Owned KMS Key\"@, the member uses
-- an Amazon Web Services owned KMS key for encryption. This parameter is
-- inherited by the nodes that this member owns.
--
-- For more information, see
-- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/managed-blockchain-encryption-at-rest.html Encryption at Rest>
-- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
--
-- 'logPublishingConfiguration', 'member_logPublishingConfiguration' - Configuration properties for logging events associated with a member.
--
-- 'name', 'member_name' - The name of the member.
--
-- 'networkId', 'member_networkId' - The unique identifier of the network to which the member belongs.
--
-- 'status', 'member_status' - The status of a member.
--
-- -   @CREATING@ - The Amazon Web Services account is in the process of
--     creating a member.
--
-- -   @AVAILABLE@ - The member has been created and can participate in the
--     network.
--
-- -   @CREATE_FAILED@ - The Amazon Web Services account attempted to
--     create a member and creation failed.
--
-- -   @UPDATING@ - The member is in the process of being updated.
--
-- -   @DELETING@ - The member and all associated resources are in the
--     process of being deleted. Either the Amazon Web Services account
--     that owns the member deleted it, or the member is being deleted as
--     the result of an @APPROVED@ @PROPOSAL@ to remove the member.
--
-- -   @DELETED@ - The member can no longer participate on the network and
--     all associated resources are deleted. Either the Amazon Web Services
--     account that owns the member deleted it, or the member is being
--     deleted as the result of an @APPROVED@ @PROPOSAL@ to remove the
--     member.
--
-- -   @INACCESSIBLE_ENCRYPTION_KEY@ - The member is impaired and might not
--     function as expected because it cannot access the specified customer
--     managed key in KMS for encryption at rest. Either the KMS key was
--     disabled or deleted, or the grants on the key were revoked.
--
--     The effect of disabling or deleting a key or of revoking a grant
--     isn\'t immediate. It might take some time for the member resource to
--     discover that the key is inaccessible. When a resource is in this
--     state, we recommend deleting and recreating the resource.
--
-- 'tags', 'member_tags' - Tags assigned to the member. Tags consist of a key and optional value.
--
-- For more information about tags, see
-- <https://docs.aws.amazon.com/managed-blockchain/latest/ethereum-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Ethereum Developer Guide/, or
-- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
newMember ::
  Member
newMember =
  Member'
    { arn = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      description = Prelude.Nothing,
      frameworkAttributes = Prelude.Nothing,
      id = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing,
      logPublishingConfiguration = Prelude.Nothing,
      name = Prelude.Nothing,
      networkId = Prelude.Nothing,
      status = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the member. For more information about
-- ARNs and their format, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
member_arn :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_arn = Lens.lens (\Member' {arn} -> arn) (\s@Member' {} a -> s {arn = a} :: Member)

-- | The date and time that the member was created.
member_creationDate :: Lens.Lens' Member (Prelude.Maybe Prelude.UTCTime)
member_creationDate = Lens.lens (\Member' {creationDate} -> creationDate) (\s@Member' {} a -> s {creationDate = a} :: Member) Prelude.. Lens.mapping Data._Time

-- | An optional description for the member.
member_description :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_description = Lens.lens (\Member' {description} -> description) (\s@Member' {} a -> s {description = a} :: Member)

-- | Attributes relevant to a member for the blockchain framework that the
-- Managed Blockchain network uses.
member_frameworkAttributes :: Lens.Lens' Member (Prelude.Maybe MemberFrameworkAttributes)
member_frameworkAttributes = Lens.lens (\Member' {frameworkAttributes} -> frameworkAttributes) (\s@Member' {} a -> s {frameworkAttributes = a} :: Member)

-- | The unique identifier of the member.
member_id :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_id = Lens.lens (\Member' {id} -> id) (\s@Member' {} a -> s {id = a} :: Member)

-- | The Amazon Resource Name (ARN) of the customer managed key in Key
-- Management Service (KMS) that the member uses for encryption at rest. If
-- the value of this parameter is @\"AWS Owned KMS Key\"@, the member uses
-- an Amazon Web Services owned KMS key for encryption. This parameter is
-- inherited by the nodes that this member owns.
--
-- For more information, see
-- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/managed-blockchain-encryption-at-rest.html Encryption at Rest>
-- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
member_kmsKeyArn :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_kmsKeyArn = Lens.lens (\Member' {kmsKeyArn} -> kmsKeyArn) (\s@Member' {} a -> s {kmsKeyArn = a} :: Member)

-- | Configuration properties for logging events associated with a member.
member_logPublishingConfiguration :: Lens.Lens' Member (Prelude.Maybe MemberLogPublishingConfiguration)
member_logPublishingConfiguration = Lens.lens (\Member' {logPublishingConfiguration} -> logPublishingConfiguration) (\s@Member' {} a -> s {logPublishingConfiguration = a} :: Member)

-- | The name of the member.
member_name :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_name = Lens.lens (\Member' {name} -> name) (\s@Member' {} a -> s {name = a} :: Member)

-- | The unique identifier of the network to which the member belongs.
member_networkId :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_networkId = Lens.lens (\Member' {networkId} -> networkId) (\s@Member' {} a -> s {networkId = a} :: Member)

-- | The status of a member.
--
-- -   @CREATING@ - The Amazon Web Services account is in the process of
--     creating a member.
--
-- -   @AVAILABLE@ - The member has been created and can participate in the
--     network.
--
-- -   @CREATE_FAILED@ - The Amazon Web Services account attempted to
--     create a member and creation failed.
--
-- -   @UPDATING@ - The member is in the process of being updated.
--
-- -   @DELETING@ - The member and all associated resources are in the
--     process of being deleted. Either the Amazon Web Services account
--     that owns the member deleted it, or the member is being deleted as
--     the result of an @APPROVED@ @PROPOSAL@ to remove the member.
--
-- -   @DELETED@ - The member can no longer participate on the network and
--     all associated resources are deleted. Either the Amazon Web Services
--     account that owns the member deleted it, or the member is being
--     deleted as the result of an @APPROVED@ @PROPOSAL@ to remove the
--     member.
--
-- -   @INACCESSIBLE_ENCRYPTION_KEY@ - The member is impaired and might not
--     function as expected because it cannot access the specified customer
--     managed key in KMS for encryption at rest. Either the KMS key was
--     disabled or deleted, or the grants on the key were revoked.
--
--     The effect of disabling or deleting a key or of revoking a grant
--     isn\'t immediate. It might take some time for the member resource to
--     discover that the key is inaccessible. When a resource is in this
--     state, we recommend deleting and recreating the resource.
member_status :: Lens.Lens' Member (Prelude.Maybe MemberStatus)
member_status = Lens.lens (\Member' {status} -> status) (\s@Member' {} a -> s {status = a} :: Member)

-- | Tags assigned to the member. Tags consist of a key and optional value.
--
-- For more information about tags, see
-- <https://docs.aws.amazon.com/managed-blockchain/latest/ethereum-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Ethereum Developer Guide/, or
-- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
member_tags :: Lens.Lens' Member (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
member_tags = Lens.lens (\Member' {tags} -> tags) (\s@Member' {} a -> s {tags = a} :: Member) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Member where
  parseJSON =
    Data.withObject
      "Member"
      ( \x ->
          Member'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "FrameworkAttributes")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "KmsKeyArn")
            Prelude.<*> (x Data..:? "LogPublishingConfiguration")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "NetworkId")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Member where
  hashWithSalt _salt Member' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` frameworkAttributes
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` logPublishingConfiguration
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` networkId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` tags

instance Prelude.NFData Member where
  rnf Member' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf frameworkAttributes
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf logPublishingConfiguration
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf networkId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf tags
