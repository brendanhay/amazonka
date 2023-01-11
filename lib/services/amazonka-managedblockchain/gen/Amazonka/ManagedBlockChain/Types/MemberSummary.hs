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
-- Module      : Amazonka.ManagedBlockChain.Types.MemberSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.MemberSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ManagedBlockChain.Types.MemberStatus
import qualified Amazonka.Prelude as Prelude

-- | A summary of configuration properties for a member.
--
-- Applies only to Hyperledger Fabric.
--
-- /See:/ 'newMemberSummary' smart constructor.
data MemberSummary = MemberSummary'
  { -- | The Amazon Resource Name (ARN) of the member. For more information about
    -- ARNs and their format, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /Amazon Web Services General Reference/.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the member was created.
    creationDate :: Prelude.Maybe Data.ISO8601,
    -- | An optional description of the member.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the member.
    id :: Prelude.Maybe Prelude.Text,
    -- | An indicator of whether the member is owned by your Amazon Web Services
    -- account or a different Amazon Web Services account.
    isOwned :: Prelude.Maybe Prelude.Bool,
    -- | The name of the member.
    name :: Prelude.Maybe Prelude.Text,
    -- | The status of the member.
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
    --     managed key in Key Management Service (KMS) for encryption at rest.
    --     Either the KMS key was disabled or deleted, or the grants on the key
    --     were revoked.
    --
    --     The effect of disabling or deleting a key or of revoking a grant
    --     isn\'t immediate. It might take some time for the member resource to
    --     discover that the key is inaccessible. When a resource is in this
    --     state, we recommend deleting and recreating the resource.
    status :: Prelude.Maybe MemberStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MemberSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'memberSummary_arn' - The Amazon Resource Name (ARN) of the member. For more information about
-- ARNs and their format, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
--
-- 'creationDate', 'memberSummary_creationDate' - The date and time that the member was created.
--
-- 'description', 'memberSummary_description' - An optional description of the member.
--
-- 'id', 'memberSummary_id' - The unique identifier of the member.
--
-- 'isOwned', 'memberSummary_isOwned' - An indicator of whether the member is owned by your Amazon Web Services
-- account or a different Amazon Web Services account.
--
-- 'name', 'memberSummary_name' - The name of the member.
--
-- 'status', 'memberSummary_status' - The status of the member.
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
--     managed key in Key Management Service (KMS) for encryption at rest.
--     Either the KMS key was disabled or deleted, or the grants on the key
--     were revoked.
--
--     The effect of disabling or deleting a key or of revoking a grant
--     isn\'t immediate. It might take some time for the member resource to
--     discover that the key is inaccessible. When a resource is in this
--     state, we recommend deleting and recreating the resource.
newMemberSummary ::
  MemberSummary
newMemberSummary =
  MemberSummary'
    { arn = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      isOwned = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the member. For more information about
-- ARNs and their format, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
memberSummary_arn :: Lens.Lens' MemberSummary (Prelude.Maybe Prelude.Text)
memberSummary_arn = Lens.lens (\MemberSummary' {arn} -> arn) (\s@MemberSummary' {} a -> s {arn = a} :: MemberSummary)

-- | The date and time that the member was created.
memberSummary_creationDate :: Lens.Lens' MemberSummary (Prelude.Maybe Prelude.UTCTime)
memberSummary_creationDate = Lens.lens (\MemberSummary' {creationDate} -> creationDate) (\s@MemberSummary' {} a -> s {creationDate = a} :: MemberSummary) Prelude.. Lens.mapping Data._Time

-- | An optional description of the member.
memberSummary_description :: Lens.Lens' MemberSummary (Prelude.Maybe Prelude.Text)
memberSummary_description = Lens.lens (\MemberSummary' {description} -> description) (\s@MemberSummary' {} a -> s {description = a} :: MemberSummary)

-- | The unique identifier of the member.
memberSummary_id :: Lens.Lens' MemberSummary (Prelude.Maybe Prelude.Text)
memberSummary_id = Lens.lens (\MemberSummary' {id} -> id) (\s@MemberSummary' {} a -> s {id = a} :: MemberSummary)

-- | An indicator of whether the member is owned by your Amazon Web Services
-- account or a different Amazon Web Services account.
memberSummary_isOwned :: Lens.Lens' MemberSummary (Prelude.Maybe Prelude.Bool)
memberSummary_isOwned = Lens.lens (\MemberSummary' {isOwned} -> isOwned) (\s@MemberSummary' {} a -> s {isOwned = a} :: MemberSummary)

-- | The name of the member.
memberSummary_name :: Lens.Lens' MemberSummary (Prelude.Maybe Prelude.Text)
memberSummary_name = Lens.lens (\MemberSummary' {name} -> name) (\s@MemberSummary' {} a -> s {name = a} :: MemberSummary)

-- | The status of the member.
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
--     managed key in Key Management Service (KMS) for encryption at rest.
--     Either the KMS key was disabled or deleted, or the grants on the key
--     were revoked.
--
--     The effect of disabling or deleting a key or of revoking a grant
--     isn\'t immediate. It might take some time for the member resource to
--     discover that the key is inaccessible. When a resource is in this
--     state, we recommend deleting and recreating the resource.
memberSummary_status :: Lens.Lens' MemberSummary (Prelude.Maybe MemberStatus)
memberSummary_status = Lens.lens (\MemberSummary' {status} -> status) (\s@MemberSummary' {} a -> s {status = a} :: MemberSummary)

instance Data.FromJSON MemberSummary where
  parseJSON =
    Data.withObject
      "MemberSummary"
      ( \x ->
          MemberSummary'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "IsOwned")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable MemberSummary where
  hashWithSalt _salt MemberSummary' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` isOwned
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData MemberSummary where
  rnf MemberSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf isOwned
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
