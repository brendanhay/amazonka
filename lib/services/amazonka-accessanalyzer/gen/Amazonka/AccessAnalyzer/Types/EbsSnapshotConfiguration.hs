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
-- Module      : Amazonka.AccessAnalyzer.Types.EbsSnapshotConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.EbsSnapshotConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The proposed access control configuration for an Amazon EBS volume
-- snapshot. You can propose a configuration for a new Amazon EBS volume
-- snapshot or an Amazon EBS volume snapshot that you own by specifying the
-- user IDs, groups, and optional KMS encryption key. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifySnapshotAttribute.html ModifySnapshotAttribute>.
--
-- /See:/ 'newEbsSnapshotConfiguration' smart constructor.
data EbsSnapshotConfiguration = EbsSnapshotConfiguration'
  { -- | The groups that have access to the Amazon EBS volume snapshot. If the
    -- value @all@ is specified, then the Amazon EBS volume snapshot is public.
    --
    -- -   If the configuration is for an existing Amazon EBS volume snapshot
    --     and you do not specify the @groups@, then the access preview uses
    --     the existing shared @groups@ for the snapshot.
    --
    -- -   If the access preview is for a new resource and you do not specify
    --     the @groups@, then the access preview considers the snapshot without
    --     any @groups@.
    --
    -- -   To propose deletion of existing shared @groups@, you can specify an
    --     empty list for @groups@.
    groups :: Prelude.Maybe [Prelude.Text],
    -- | The KMS key identifier for an encrypted Amazon EBS volume snapshot. The
    -- KMS key identifier is the key ARN, key ID, alias ARN, or alias name for
    -- the KMS key.
    --
    -- -   If the configuration is for an existing Amazon EBS volume snapshot
    --     and you do not specify the @kmsKeyId@, or you specify an empty
    --     string, then the access preview uses the existing @kmsKeyId@ of the
    --     snapshot.
    --
    -- -   If the access preview is for a new resource and you do not specify
    --     the @kmsKeyId@, the access preview considers the snapshot as
    --     unencrypted.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the Amazon Web Services accounts that have access to the
    -- Amazon EBS volume snapshot.
    --
    -- -   If the configuration is for an existing Amazon EBS volume snapshot
    --     and you do not specify the @userIds@, then the access preview uses
    --     the existing shared @userIds@ for the snapshot.
    --
    -- -   If the access preview is for a new resource and you do not specify
    --     the @userIds@, then the access preview considers the snapshot
    --     without any @userIds@.
    --
    -- -   To propose deletion of existing shared @accountIds@, you can specify
    --     an empty list for @userIds@.
    userIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EbsSnapshotConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groups', 'ebsSnapshotConfiguration_groups' - The groups that have access to the Amazon EBS volume snapshot. If the
-- value @all@ is specified, then the Amazon EBS volume snapshot is public.
--
-- -   If the configuration is for an existing Amazon EBS volume snapshot
--     and you do not specify the @groups@, then the access preview uses
--     the existing shared @groups@ for the snapshot.
--
-- -   If the access preview is for a new resource and you do not specify
--     the @groups@, then the access preview considers the snapshot without
--     any @groups@.
--
-- -   To propose deletion of existing shared @groups@, you can specify an
--     empty list for @groups@.
--
-- 'kmsKeyId', 'ebsSnapshotConfiguration_kmsKeyId' - The KMS key identifier for an encrypted Amazon EBS volume snapshot. The
-- KMS key identifier is the key ARN, key ID, alias ARN, or alias name for
-- the KMS key.
--
-- -   If the configuration is for an existing Amazon EBS volume snapshot
--     and you do not specify the @kmsKeyId@, or you specify an empty
--     string, then the access preview uses the existing @kmsKeyId@ of the
--     snapshot.
--
-- -   If the access preview is for a new resource and you do not specify
--     the @kmsKeyId@, the access preview considers the snapshot as
--     unencrypted.
--
-- 'userIds', 'ebsSnapshotConfiguration_userIds' - The IDs of the Amazon Web Services accounts that have access to the
-- Amazon EBS volume snapshot.
--
-- -   If the configuration is for an existing Amazon EBS volume snapshot
--     and you do not specify the @userIds@, then the access preview uses
--     the existing shared @userIds@ for the snapshot.
--
-- -   If the access preview is for a new resource and you do not specify
--     the @userIds@, then the access preview considers the snapshot
--     without any @userIds@.
--
-- -   To propose deletion of existing shared @accountIds@, you can specify
--     an empty list for @userIds@.
newEbsSnapshotConfiguration ::
  EbsSnapshotConfiguration
newEbsSnapshotConfiguration =
  EbsSnapshotConfiguration'
    { groups = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      userIds = Prelude.Nothing
    }

-- | The groups that have access to the Amazon EBS volume snapshot. If the
-- value @all@ is specified, then the Amazon EBS volume snapshot is public.
--
-- -   If the configuration is for an existing Amazon EBS volume snapshot
--     and you do not specify the @groups@, then the access preview uses
--     the existing shared @groups@ for the snapshot.
--
-- -   If the access preview is for a new resource and you do not specify
--     the @groups@, then the access preview considers the snapshot without
--     any @groups@.
--
-- -   To propose deletion of existing shared @groups@, you can specify an
--     empty list for @groups@.
ebsSnapshotConfiguration_groups :: Lens.Lens' EbsSnapshotConfiguration (Prelude.Maybe [Prelude.Text])
ebsSnapshotConfiguration_groups = Lens.lens (\EbsSnapshotConfiguration' {groups} -> groups) (\s@EbsSnapshotConfiguration' {} a -> s {groups = a} :: EbsSnapshotConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The KMS key identifier for an encrypted Amazon EBS volume snapshot. The
-- KMS key identifier is the key ARN, key ID, alias ARN, or alias name for
-- the KMS key.
--
-- -   If the configuration is for an existing Amazon EBS volume snapshot
--     and you do not specify the @kmsKeyId@, or you specify an empty
--     string, then the access preview uses the existing @kmsKeyId@ of the
--     snapshot.
--
-- -   If the access preview is for a new resource and you do not specify
--     the @kmsKeyId@, the access preview considers the snapshot as
--     unencrypted.
ebsSnapshotConfiguration_kmsKeyId :: Lens.Lens' EbsSnapshotConfiguration (Prelude.Maybe Prelude.Text)
ebsSnapshotConfiguration_kmsKeyId = Lens.lens (\EbsSnapshotConfiguration' {kmsKeyId} -> kmsKeyId) (\s@EbsSnapshotConfiguration' {} a -> s {kmsKeyId = a} :: EbsSnapshotConfiguration)

-- | The IDs of the Amazon Web Services accounts that have access to the
-- Amazon EBS volume snapshot.
--
-- -   If the configuration is for an existing Amazon EBS volume snapshot
--     and you do not specify the @userIds@, then the access preview uses
--     the existing shared @userIds@ for the snapshot.
--
-- -   If the access preview is for a new resource and you do not specify
--     the @userIds@, then the access preview considers the snapshot
--     without any @userIds@.
--
-- -   To propose deletion of existing shared @accountIds@, you can specify
--     an empty list for @userIds@.
ebsSnapshotConfiguration_userIds :: Lens.Lens' EbsSnapshotConfiguration (Prelude.Maybe [Prelude.Text])
ebsSnapshotConfiguration_userIds = Lens.lens (\EbsSnapshotConfiguration' {userIds} -> userIds) (\s@EbsSnapshotConfiguration' {} a -> s {userIds = a} :: EbsSnapshotConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON EbsSnapshotConfiguration where
  parseJSON =
    Data.withObject
      "EbsSnapshotConfiguration"
      ( \x ->
          EbsSnapshotConfiguration'
            Prelude.<$> (x Data..:? "groups" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "kmsKeyId")
            Prelude.<*> (x Data..:? "userIds" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable EbsSnapshotConfiguration where
  hashWithSalt _salt EbsSnapshotConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` groups
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` userIds

instance Prelude.NFData EbsSnapshotConfiguration where
  rnf EbsSnapshotConfiguration' {..} =
    Prelude.rnf groups
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf userIds

instance Data.ToJSON EbsSnapshotConfiguration where
  toJSON EbsSnapshotConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("groups" Data..=) Prelude.<$> groups,
            ("kmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("userIds" Data..=) Prelude.<$> userIds
          ]
      )
