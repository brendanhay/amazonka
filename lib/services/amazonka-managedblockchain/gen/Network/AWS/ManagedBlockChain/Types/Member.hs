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
-- Module      : Network.AWS.ManagedBlockChain.Types.Member
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ManagedBlockChain.Types.Member where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.ManagedBlockChain.Types.MemberFrameworkAttributes
import Network.AWS.ManagedBlockChain.Types.MemberLogPublishingConfiguration
import Network.AWS.ManagedBlockChain.Types.MemberStatus
import qualified Network.AWS.Prelude as Prelude

-- | Member configuration properties.
--
-- Applies only to Hyperledger Fabric.
--
-- /See:/ 'newMember' smart constructor.
data Member = Member'
  { -- | The status of a member.
    --
    -- -   @CREATING@ - The AWS account is in the process of creating a member.
    --
    -- -   @AVAILABLE@ - The member has been created and can participate in the
    --     network.
    --
    -- -   @CREATE_FAILED@ - The AWS account attempted to create a member and
    --     creation failed.
    --
    -- -   @UPDATING@ - The member is in the process of being updated.
    --
    -- -   @DELETING@ - The member and all associated resources are in the
    --     process of being deleted. Either the AWS account that owns the
    --     member deleted it, or the member is being deleted as the result of
    --     an @APPROVED@ @PROPOSAL@ to remove the member.
    --
    -- -   @DELETED@ - The member can no longer participate on the network and
    --     all associated resources are deleted. Either the AWS account that
    --     owns the member deleted it, or the member is being deleted as the
    --     result of an @APPROVED@ @PROPOSAL@ to remove the member.
    --
    -- -   @INACCESSIBLE_ENCRYPTION_KEY@ - The member is impaired and might not
    --     function as expected because it cannot access the specified customer
    --     managed key in AWS KMS for encryption at rest. Either the KMS key
    --     was disabled or deleted, or the grants on the key were revoked.
    --
    --     The effect of disabling or deleting a key, or revoking a grant is
    --     not immediate. The member resource might take some time to find that
    --     the key is inaccessible. When a resource is in this state, we
    --     recommend deleting and recreating the resource.
    status :: Prelude.Maybe MemberStatus,
    -- | The Amazon Resource Name (ARN) of the customer managed key in AWS Key
    -- Management Service (AWS KMS) that the member uses for encryption at
    -- rest. If the value of this parameter is @\"AWS Owned KMS Key\"@, the
    -- member uses an AWS owned KMS key for encryption. This parameter is
    -- inherited by the nodes that this member owns.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | Configuration properties for logging events associated with a member.
    logPublishingConfiguration :: Prelude.Maybe MemberLogPublishingConfiguration,
    -- | The Amazon Resource Name (ARN) of the member. For more information about
    -- ARNs and their format, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the network to which the member belongs.
    networkId :: Prelude.Maybe Prelude.Text,
    -- | The name of the member.
    name :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the member.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the member was created.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | Attributes relevant to a member for the blockchain framework that the
    -- Managed Blockchain network uses.
    frameworkAttributes :: Prelude.Maybe MemberFrameworkAttributes,
    -- | An optional description for the member.
    description :: Prelude.Maybe Prelude.Text,
    -- | Tags assigned to the member. Tags consist of a key and optional value.
    -- For more information about tags, see
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
-- 'status', 'member_status' - The status of a member.
--
-- -   @CREATING@ - The AWS account is in the process of creating a member.
--
-- -   @AVAILABLE@ - The member has been created and can participate in the
--     network.
--
-- -   @CREATE_FAILED@ - The AWS account attempted to create a member and
--     creation failed.
--
-- -   @UPDATING@ - The member is in the process of being updated.
--
-- -   @DELETING@ - The member and all associated resources are in the
--     process of being deleted. Either the AWS account that owns the
--     member deleted it, or the member is being deleted as the result of
--     an @APPROVED@ @PROPOSAL@ to remove the member.
--
-- -   @DELETED@ - The member can no longer participate on the network and
--     all associated resources are deleted. Either the AWS account that
--     owns the member deleted it, or the member is being deleted as the
--     result of an @APPROVED@ @PROPOSAL@ to remove the member.
--
-- -   @INACCESSIBLE_ENCRYPTION_KEY@ - The member is impaired and might not
--     function as expected because it cannot access the specified customer
--     managed key in AWS KMS for encryption at rest. Either the KMS key
--     was disabled or deleted, or the grants on the key were revoked.
--
--     The effect of disabling or deleting a key, or revoking a grant is
--     not immediate. The member resource might take some time to find that
--     the key is inaccessible. When a resource is in this state, we
--     recommend deleting and recreating the resource.
--
-- 'kmsKeyArn', 'member_kmsKeyArn' - The Amazon Resource Name (ARN) of the customer managed key in AWS Key
-- Management Service (AWS KMS) that the member uses for encryption at
-- rest. If the value of this parameter is @\"AWS Owned KMS Key\"@, the
-- member uses an AWS owned KMS key for encryption. This parameter is
-- inherited by the nodes that this member owns.
--
-- 'logPublishingConfiguration', 'member_logPublishingConfiguration' - Configuration properties for logging events associated with a member.
--
-- 'arn', 'member_arn' - The Amazon Resource Name (ARN) of the member. For more information about
-- ARNs and their format, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'networkId', 'member_networkId' - The unique identifier of the network to which the member belongs.
--
-- 'name', 'member_name' - The name of the member.
--
-- 'id', 'member_id' - The unique identifier of the member.
--
-- 'creationDate', 'member_creationDate' - The date and time that the member was created.
--
-- 'frameworkAttributes', 'member_frameworkAttributes' - Attributes relevant to a member for the blockchain framework that the
-- Managed Blockchain network uses.
--
-- 'description', 'member_description' - An optional description for the member.
--
-- 'tags', 'member_tags' - Tags assigned to the member. Tags consist of a key and optional value.
-- For more information about tags, see
-- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
newMember ::
  Member
newMember =
  Member'
    { status = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing,
      logPublishingConfiguration = Prelude.Nothing,
      arn = Prelude.Nothing,
      networkId = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      frameworkAttributes = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The status of a member.
--
-- -   @CREATING@ - The AWS account is in the process of creating a member.
--
-- -   @AVAILABLE@ - The member has been created and can participate in the
--     network.
--
-- -   @CREATE_FAILED@ - The AWS account attempted to create a member and
--     creation failed.
--
-- -   @UPDATING@ - The member is in the process of being updated.
--
-- -   @DELETING@ - The member and all associated resources are in the
--     process of being deleted. Either the AWS account that owns the
--     member deleted it, or the member is being deleted as the result of
--     an @APPROVED@ @PROPOSAL@ to remove the member.
--
-- -   @DELETED@ - The member can no longer participate on the network and
--     all associated resources are deleted. Either the AWS account that
--     owns the member deleted it, or the member is being deleted as the
--     result of an @APPROVED@ @PROPOSAL@ to remove the member.
--
-- -   @INACCESSIBLE_ENCRYPTION_KEY@ - The member is impaired and might not
--     function as expected because it cannot access the specified customer
--     managed key in AWS KMS for encryption at rest. Either the KMS key
--     was disabled or deleted, or the grants on the key were revoked.
--
--     The effect of disabling or deleting a key, or revoking a grant is
--     not immediate. The member resource might take some time to find that
--     the key is inaccessible. When a resource is in this state, we
--     recommend deleting and recreating the resource.
member_status :: Lens.Lens' Member (Prelude.Maybe MemberStatus)
member_status = Lens.lens (\Member' {status} -> status) (\s@Member' {} a -> s {status = a} :: Member)

-- | The Amazon Resource Name (ARN) of the customer managed key in AWS Key
-- Management Service (AWS KMS) that the member uses for encryption at
-- rest. If the value of this parameter is @\"AWS Owned KMS Key\"@, the
-- member uses an AWS owned KMS key for encryption. This parameter is
-- inherited by the nodes that this member owns.
member_kmsKeyArn :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_kmsKeyArn = Lens.lens (\Member' {kmsKeyArn} -> kmsKeyArn) (\s@Member' {} a -> s {kmsKeyArn = a} :: Member)

-- | Configuration properties for logging events associated with a member.
member_logPublishingConfiguration :: Lens.Lens' Member (Prelude.Maybe MemberLogPublishingConfiguration)
member_logPublishingConfiguration = Lens.lens (\Member' {logPublishingConfiguration} -> logPublishingConfiguration) (\s@Member' {} a -> s {logPublishingConfiguration = a} :: Member)

-- | The Amazon Resource Name (ARN) of the member. For more information about
-- ARNs and their format, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
member_arn :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_arn = Lens.lens (\Member' {arn} -> arn) (\s@Member' {} a -> s {arn = a} :: Member)

-- | The unique identifier of the network to which the member belongs.
member_networkId :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_networkId = Lens.lens (\Member' {networkId} -> networkId) (\s@Member' {} a -> s {networkId = a} :: Member)

-- | The name of the member.
member_name :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_name = Lens.lens (\Member' {name} -> name) (\s@Member' {} a -> s {name = a} :: Member)

-- | The unique identifier of the member.
member_id :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_id = Lens.lens (\Member' {id} -> id) (\s@Member' {} a -> s {id = a} :: Member)

-- | The date and time that the member was created.
member_creationDate :: Lens.Lens' Member (Prelude.Maybe Prelude.UTCTime)
member_creationDate = Lens.lens (\Member' {creationDate} -> creationDate) (\s@Member' {} a -> s {creationDate = a} :: Member) Prelude.. Lens.mapping Core._Time

-- | Attributes relevant to a member for the blockchain framework that the
-- Managed Blockchain network uses.
member_frameworkAttributes :: Lens.Lens' Member (Prelude.Maybe MemberFrameworkAttributes)
member_frameworkAttributes = Lens.lens (\Member' {frameworkAttributes} -> frameworkAttributes) (\s@Member' {} a -> s {frameworkAttributes = a} :: Member)

-- | An optional description for the member.
member_description :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_description = Lens.lens (\Member' {description} -> description) (\s@Member' {} a -> s {description = a} :: Member)

-- | Tags assigned to the member. Tags consist of a key and optional value.
-- For more information about tags, see
-- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
member_tags :: Lens.Lens' Member (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
member_tags = Lens.lens (\Member' {tags} -> tags) (\s@Member' {} a -> s {tags = a} :: Member) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Member where
  parseJSON =
    Core.withObject
      "Member"
      ( \x ->
          Member'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "KmsKeyArn")
            Prelude.<*> (x Core..:? "LogPublishingConfiguration")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "NetworkId")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "CreationDate")
            Prelude.<*> (x Core..:? "FrameworkAttributes")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Member

instance Prelude.NFData Member
