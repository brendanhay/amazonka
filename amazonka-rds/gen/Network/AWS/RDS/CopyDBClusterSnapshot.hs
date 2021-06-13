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
-- Module      : Network.AWS.RDS.CopyDBClusterSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies a snapshot of a DB cluster.
--
-- To copy a DB cluster snapshot from a shared manual DB cluster snapshot,
-- @SourceDBClusterSnapshotIdentifier@ must be the Amazon Resource Name
-- (ARN) of the shared DB cluster snapshot.
--
-- You can copy an encrypted DB cluster snapshot from another AWS Region.
-- In that case, the AWS Region where you call the @CopyDBClusterSnapshot@
-- action is the destination AWS Region for the encrypted DB cluster
-- snapshot to be copied to. To copy an encrypted DB cluster snapshot from
-- another AWS Region, you must provide the following values:
--
-- -   @KmsKeyId@ - The AWS Key Management System (AWS KMS) key identifier
--     for the key to use to encrypt the copy of the DB cluster snapshot in
--     the destination AWS Region.
--
-- -   @PreSignedUrl@ - A URL that contains a Signature Version 4 signed
--     request for the @CopyDBClusterSnapshot@ action to be called in the
--     source AWS Region where the DB cluster snapshot is copied from. The
--     pre-signed URL must be a valid request for the
--     @CopyDBClusterSnapshot@ API action that can be executed in the
--     source AWS Region that contains the encrypted DB cluster snapshot to
--     be copied.
--
--     The pre-signed URL request must contain the following parameter
--     values:
--
--     -   @KmsKeyId@ - The AWS KMS key identifier for the customer master
--         key (CMK) to use to encrypt the copy of the DB cluster snapshot
--         in the destination AWS Region. This is the same identifier for
--         both the @CopyDBClusterSnapshot@ action that is called in the
--         destination AWS Region, and the action contained in the
--         pre-signed URL.
--
--     -   @DestinationRegion@ - The name of the AWS Region that the DB
--         cluster snapshot is to be created in.
--
--     -   @SourceDBClusterSnapshotIdentifier@ - The DB cluster snapshot
--         identifier for the encrypted DB cluster snapshot to be copied.
--         This identifier must be in the Amazon Resource Name (ARN) format
--         for the source AWS Region. For example, if you are copying an
--         encrypted DB cluster snapshot from the us-west-2 AWS Region,
--         then your @SourceDBClusterSnapshotIdentifier@ looks like the
--         following example:
--         @arn:aws:rds:us-west-2:123456789012:cluster-snapshot:aurora-cluster1-snapshot-20161115@.
--
--     To learn how to generate a Signature Version 4 signed request, see
--     <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (AWS Signature Version 4)>
--     and
--     <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process>.
--
--     If you are using an AWS SDK tool or the AWS CLI, you can specify
--     @SourceRegion@ (or @--source-region@ for the AWS CLI) instead of
--     specifying @PreSignedUrl@ manually. Specifying @SourceRegion@
--     autogenerates a pre-signed URL that is a valid request for the
--     operation that can be executed in the source AWS Region.
--
-- -   @TargetDBClusterSnapshotIdentifier@ - The identifier for the new
--     copy of the DB cluster snapshot in the destination AWS Region.
--
-- -   @SourceDBClusterSnapshotIdentifier@ - The DB cluster snapshot
--     identifier for the encrypted DB cluster snapshot to be copied. This
--     identifier must be in the ARN format for the source AWS Region and
--     is the same value as the @SourceDBClusterSnapshotIdentifier@ in the
--     pre-signed URL.
--
-- To cancel the copy operation once it is in progress, delete the target
-- DB cluster snapshot identified by @TargetDBClusterSnapshotIdentifier@
-- while that DB cluster snapshot is in \"copying\" status.
--
-- For more information on copying encrypted DB cluster snapshots from one
-- AWS Region to another, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_CopySnapshot.html Copying a Snapshot>
-- in the /Amazon Aurora User Guide./
--
-- For more information on Amazon Aurora, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?>
-- in the /Amazon Aurora User Guide./
--
-- This action only applies to Aurora DB clusters.
module Network.AWS.RDS.CopyDBClusterSnapshot
  ( -- * Creating a Request
    CopyDBClusterSnapshot (..),
    newCopyDBClusterSnapshot,

    -- * Request Lenses
    copyDBClusterSnapshot_kmsKeyId,
    copyDBClusterSnapshot_copyTags,
    copyDBClusterSnapshot_tags,
    copyDBClusterSnapshot_preSignedUrl,
    copyDBClusterSnapshot_sourceDBClusterSnapshotIdentifier,
    copyDBClusterSnapshot_targetDBClusterSnapshotIdentifier,

    -- * Destructuring the Response
    CopyDBClusterSnapshotResponse (..),
    newCopyDBClusterSnapshotResponse,

    -- * Response Lenses
    copyDBClusterSnapshotResponse_dbClusterSnapshot,
    copyDBClusterSnapshotResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newCopyDBClusterSnapshot' smart constructor.
data CopyDBClusterSnapshot = CopyDBClusterSnapshot'
  { -- | The AWS KMS key identifier for an encrypted DB cluster snapshot. The AWS
    -- KMS key identifier is the key ARN, key ID, alias ARN, or alias name for
    -- the AWS KMS customer master key (CMK).
    --
    -- If you copy an encrypted DB cluster snapshot from your AWS account, you
    -- can specify a value for @KmsKeyId@ to encrypt the copy with a new AWS
    -- KMS CMK. If you don\'t specify a value for @KmsKeyId@, then the copy of
    -- the DB cluster snapshot is encrypted with the same AWS KMS key as the
    -- source DB cluster snapshot.
    --
    -- If you copy an encrypted DB cluster snapshot that is shared from another
    -- AWS account, then you must specify a value for @KmsKeyId@.
    --
    -- To copy an encrypted DB cluster snapshot to another AWS Region, you must
    -- set @KmsKeyId@ to the AWS KMS key identifier you want to use to encrypt
    -- the copy of the DB cluster snapshot in the destination AWS Region. AWS
    -- KMS CMKs are specific to the AWS Region that they are created in, and
    -- you can\'t use CMKs from one AWS Region in another AWS Region.
    --
    -- If you copy an unencrypted DB cluster snapshot and specify a value for
    -- the @KmsKeyId@ parameter, an error is returned.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to copy all tags from the source DB
    -- cluster snapshot to the target DB cluster snapshot. By default, tags are
    -- not copied.
    copyTags :: Prelude.Maybe Prelude.Bool,
    tags :: Prelude.Maybe [Tag],
    -- | The URL that contains a Signature Version 4 signed request for the
    -- @CopyDBClusterSnapshot@ API action in the AWS Region that contains the
    -- source DB cluster snapshot to copy. The @PreSignedUrl@ parameter must be
    -- used when copying an encrypted DB cluster snapshot from another AWS
    -- Region. Don\'t specify @PreSignedUrl@ when you are copying an encrypted
    -- DB cluster snapshot in the same AWS Region.
    --
    -- The pre-signed URL must be a valid request for the
    -- @CopyDBClusterSnapshot@ API action that can be executed in the source
    -- AWS Region that contains the encrypted DB cluster snapshot to be copied.
    -- The pre-signed URL request must contain the following parameter values:
    --
    -- -   @KmsKeyId@ - The AWS KMS key identifier for the customer master key
    --     (CMK) to use to encrypt the copy of the DB cluster snapshot in the
    --     destination AWS Region. This is the same identifier for both the
    --     @CopyDBClusterSnapshot@ action that is called in the destination AWS
    --     Region, and the action contained in the pre-signed URL.
    --
    -- -   @DestinationRegion@ - The name of the AWS Region that the DB cluster
    --     snapshot is to be created in.
    --
    -- -   @SourceDBClusterSnapshotIdentifier@ - The DB cluster snapshot
    --     identifier for the encrypted DB cluster snapshot to be copied. This
    --     identifier must be in the Amazon Resource Name (ARN) format for the
    --     source AWS Region. For example, if you are copying an encrypted DB
    --     cluster snapshot from the us-west-2 AWS Region, then your
    --     @SourceDBClusterSnapshotIdentifier@ looks like the following
    --     example:
    --     @arn:aws:rds:us-west-2:123456789012:cluster-snapshot:aurora-cluster1-snapshot-20161115@.
    --
    -- To learn how to generate a Signature Version 4 signed request, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (AWS Signature Version 4)>
    -- and
    -- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process>.
    --
    -- If you are using an AWS SDK tool or the AWS CLI, you can specify
    -- @SourceRegion@ (or @--source-region@ for the AWS CLI) instead of
    -- specifying @PreSignedUrl@ manually. Specifying @SourceRegion@
    -- autogenerates a pre-signed URL that is a valid request for the operation
    -- that can be executed in the source AWS Region.
    preSignedUrl :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the DB cluster snapshot to copy. This parameter isn\'t
    -- case-sensitive.
    --
    -- You can\'t copy an encrypted, shared DB cluster snapshot from one AWS
    -- Region to another.
    --
    -- Constraints:
    --
    -- -   Must specify a valid system snapshot in the \"available\" state.
    --
    -- -   If the source snapshot is in the same AWS Region as the copy,
    --     specify a valid DB snapshot identifier.
    --
    -- -   If the source snapshot is in a different AWS Region than the copy,
    --     specify a valid DB cluster snapshot ARN. For more information, go to
    --     <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_CopySnapshot.html#USER_CopySnapshot.AcrossRegions Copying Snapshots Across AWS Regions>
    --     in the /Amazon Aurora User Guide./
    --
    -- Example: @my-cluster-snapshot1@
    sourceDBClusterSnapshotIdentifier :: Prelude.Text,
    -- | The identifier of the new DB cluster snapshot to create from the source
    -- DB cluster snapshot. This parameter isn\'t case-sensitive.
    --
    -- Constraints:
    --
    -- -   Must contain from 1 to 63 letters, numbers, or hyphens.
    --
    -- -   First character must be a letter.
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens.
    --
    -- Example: @my-cluster-snapshot2@
    targetDBClusterSnapshotIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopyDBClusterSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'copyDBClusterSnapshot_kmsKeyId' - The AWS KMS key identifier for an encrypted DB cluster snapshot. The AWS
-- KMS key identifier is the key ARN, key ID, alias ARN, or alias name for
-- the AWS KMS customer master key (CMK).
--
-- If you copy an encrypted DB cluster snapshot from your AWS account, you
-- can specify a value for @KmsKeyId@ to encrypt the copy with a new AWS
-- KMS CMK. If you don\'t specify a value for @KmsKeyId@, then the copy of
-- the DB cluster snapshot is encrypted with the same AWS KMS key as the
-- source DB cluster snapshot.
--
-- If you copy an encrypted DB cluster snapshot that is shared from another
-- AWS account, then you must specify a value for @KmsKeyId@.
--
-- To copy an encrypted DB cluster snapshot to another AWS Region, you must
-- set @KmsKeyId@ to the AWS KMS key identifier you want to use to encrypt
-- the copy of the DB cluster snapshot in the destination AWS Region. AWS
-- KMS CMKs are specific to the AWS Region that they are created in, and
-- you can\'t use CMKs from one AWS Region in another AWS Region.
--
-- If you copy an unencrypted DB cluster snapshot and specify a value for
-- the @KmsKeyId@ parameter, an error is returned.
--
-- 'copyTags', 'copyDBClusterSnapshot_copyTags' - A value that indicates whether to copy all tags from the source DB
-- cluster snapshot to the target DB cluster snapshot. By default, tags are
-- not copied.
--
-- 'tags', 'copyDBClusterSnapshot_tags' - Undocumented member.
--
-- 'preSignedUrl', 'copyDBClusterSnapshot_preSignedUrl' - The URL that contains a Signature Version 4 signed request for the
-- @CopyDBClusterSnapshot@ API action in the AWS Region that contains the
-- source DB cluster snapshot to copy. The @PreSignedUrl@ parameter must be
-- used when copying an encrypted DB cluster snapshot from another AWS
-- Region. Don\'t specify @PreSignedUrl@ when you are copying an encrypted
-- DB cluster snapshot in the same AWS Region.
--
-- The pre-signed URL must be a valid request for the
-- @CopyDBClusterSnapshot@ API action that can be executed in the source
-- AWS Region that contains the encrypted DB cluster snapshot to be copied.
-- The pre-signed URL request must contain the following parameter values:
--
-- -   @KmsKeyId@ - The AWS KMS key identifier for the customer master key
--     (CMK) to use to encrypt the copy of the DB cluster snapshot in the
--     destination AWS Region. This is the same identifier for both the
--     @CopyDBClusterSnapshot@ action that is called in the destination AWS
--     Region, and the action contained in the pre-signed URL.
--
-- -   @DestinationRegion@ - The name of the AWS Region that the DB cluster
--     snapshot is to be created in.
--
-- -   @SourceDBClusterSnapshotIdentifier@ - The DB cluster snapshot
--     identifier for the encrypted DB cluster snapshot to be copied. This
--     identifier must be in the Amazon Resource Name (ARN) format for the
--     source AWS Region. For example, if you are copying an encrypted DB
--     cluster snapshot from the us-west-2 AWS Region, then your
--     @SourceDBClusterSnapshotIdentifier@ looks like the following
--     example:
--     @arn:aws:rds:us-west-2:123456789012:cluster-snapshot:aurora-cluster1-snapshot-20161115@.
--
-- To learn how to generate a Signature Version 4 signed request, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (AWS Signature Version 4)>
-- and
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process>.
--
-- If you are using an AWS SDK tool or the AWS CLI, you can specify
-- @SourceRegion@ (or @--source-region@ for the AWS CLI) instead of
-- specifying @PreSignedUrl@ manually. Specifying @SourceRegion@
-- autogenerates a pre-signed URL that is a valid request for the operation
-- that can be executed in the source AWS Region.
--
-- 'sourceDBClusterSnapshotIdentifier', 'copyDBClusterSnapshot_sourceDBClusterSnapshotIdentifier' - The identifier of the DB cluster snapshot to copy. This parameter isn\'t
-- case-sensitive.
--
-- You can\'t copy an encrypted, shared DB cluster snapshot from one AWS
-- Region to another.
--
-- Constraints:
--
-- -   Must specify a valid system snapshot in the \"available\" state.
--
-- -   If the source snapshot is in the same AWS Region as the copy,
--     specify a valid DB snapshot identifier.
--
-- -   If the source snapshot is in a different AWS Region than the copy,
--     specify a valid DB cluster snapshot ARN. For more information, go to
--     <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_CopySnapshot.html#USER_CopySnapshot.AcrossRegions Copying Snapshots Across AWS Regions>
--     in the /Amazon Aurora User Guide./
--
-- Example: @my-cluster-snapshot1@
--
-- 'targetDBClusterSnapshotIdentifier', 'copyDBClusterSnapshot_targetDBClusterSnapshotIdentifier' - The identifier of the new DB cluster snapshot to create from the source
-- DB cluster snapshot. This parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens.
--
-- -   First character must be a letter.
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens.
--
-- Example: @my-cluster-snapshot2@
newCopyDBClusterSnapshot ::
  -- | 'sourceDBClusterSnapshotIdentifier'
  Prelude.Text ->
  -- | 'targetDBClusterSnapshotIdentifier'
  Prelude.Text ->
  CopyDBClusterSnapshot
newCopyDBClusterSnapshot
  pSourceDBClusterSnapshotIdentifier_
  pTargetDBClusterSnapshotIdentifier_ =
    CopyDBClusterSnapshot'
      { kmsKeyId = Prelude.Nothing,
        copyTags = Prelude.Nothing,
        tags = Prelude.Nothing,
        preSignedUrl = Prelude.Nothing,
        sourceDBClusterSnapshotIdentifier =
          pSourceDBClusterSnapshotIdentifier_,
        targetDBClusterSnapshotIdentifier =
          pTargetDBClusterSnapshotIdentifier_
      }

-- | The AWS KMS key identifier for an encrypted DB cluster snapshot. The AWS
-- KMS key identifier is the key ARN, key ID, alias ARN, or alias name for
-- the AWS KMS customer master key (CMK).
--
-- If you copy an encrypted DB cluster snapshot from your AWS account, you
-- can specify a value for @KmsKeyId@ to encrypt the copy with a new AWS
-- KMS CMK. If you don\'t specify a value for @KmsKeyId@, then the copy of
-- the DB cluster snapshot is encrypted with the same AWS KMS key as the
-- source DB cluster snapshot.
--
-- If you copy an encrypted DB cluster snapshot that is shared from another
-- AWS account, then you must specify a value for @KmsKeyId@.
--
-- To copy an encrypted DB cluster snapshot to another AWS Region, you must
-- set @KmsKeyId@ to the AWS KMS key identifier you want to use to encrypt
-- the copy of the DB cluster snapshot in the destination AWS Region. AWS
-- KMS CMKs are specific to the AWS Region that they are created in, and
-- you can\'t use CMKs from one AWS Region in another AWS Region.
--
-- If you copy an unencrypted DB cluster snapshot and specify a value for
-- the @KmsKeyId@ parameter, an error is returned.
copyDBClusterSnapshot_kmsKeyId :: Lens.Lens' CopyDBClusterSnapshot (Prelude.Maybe Prelude.Text)
copyDBClusterSnapshot_kmsKeyId = Lens.lens (\CopyDBClusterSnapshot' {kmsKeyId} -> kmsKeyId) (\s@CopyDBClusterSnapshot' {} a -> s {kmsKeyId = a} :: CopyDBClusterSnapshot)

-- | A value that indicates whether to copy all tags from the source DB
-- cluster snapshot to the target DB cluster snapshot. By default, tags are
-- not copied.
copyDBClusterSnapshot_copyTags :: Lens.Lens' CopyDBClusterSnapshot (Prelude.Maybe Prelude.Bool)
copyDBClusterSnapshot_copyTags = Lens.lens (\CopyDBClusterSnapshot' {copyTags} -> copyTags) (\s@CopyDBClusterSnapshot' {} a -> s {copyTags = a} :: CopyDBClusterSnapshot)

-- | Undocumented member.
copyDBClusterSnapshot_tags :: Lens.Lens' CopyDBClusterSnapshot (Prelude.Maybe [Tag])
copyDBClusterSnapshot_tags = Lens.lens (\CopyDBClusterSnapshot' {tags} -> tags) (\s@CopyDBClusterSnapshot' {} a -> s {tags = a} :: CopyDBClusterSnapshot) Prelude.. Lens.mapping Lens._Coerce

-- | The URL that contains a Signature Version 4 signed request for the
-- @CopyDBClusterSnapshot@ API action in the AWS Region that contains the
-- source DB cluster snapshot to copy. The @PreSignedUrl@ parameter must be
-- used when copying an encrypted DB cluster snapshot from another AWS
-- Region. Don\'t specify @PreSignedUrl@ when you are copying an encrypted
-- DB cluster snapshot in the same AWS Region.
--
-- The pre-signed URL must be a valid request for the
-- @CopyDBClusterSnapshot@ API action that can be executed in the source
-- AWS Region that contains the encrypted DB cluster snapshot to be copied.
-- The pre-signed URL request must contain the following parameter values:
--
-- -   @KmsKeyId@ - The AWS KMS key identifier for the customer master key
--     (CMK) to use to encrypt the copy of the DB cluster snapshot in the
--     destination AWS Region. This is the same identifier for both the
--     @CopyDBClusterSnapshot@ action that is called in the destination AWS
--     Region, and the action contained in the pre-signed URL.
--
-- -   @DestinationRegion@ - The name of the AWS Region that the DB cluster
--     snapshot is to be created in.
--
-- -   @SourceDBClusterSnapshotIdentifier@ - The DB cluster snapshot
--     identifier for the encrypted DB cluster snapshot to be copied. This
--     identifier must be in the Amazon Resource Name (ARN) format for the
--     source AWS Region. For example, if you are copying an encrypted DB
--     cluster snapshot from the us-west-2 AWS Region, then your
--     @SourceDBClusterSnapshotIdentifier@ looks like the following
--     example:
--     @arn:aws:rds:us-west-2:123456789012:cluster-snapshot:aurora-cluster1-snapshot-20161115@.
--
-- To learn how to generate a Signature Version 4 signed request, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (AWS Signature Version 4)>
-- and
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process>.
--
-- If you are using an AWS SDK tool or the AWS CLI, you can specify
-- @SourceRegion@ (or @--source-region@ for the AWS CLI) instead of
-- specifying @PreSignedUrl@ manually. Specifying @SourceRegion@
-- autogenerates a pre-signed URL that is a valid request for the operation
-- that can be executed in the source AWS Region.
copyDBClusterSnapshot_preSignedUrl :: Lens.Lens' CopyDBClusterSnapshot (Prelude.Maybe Prelude.Text)
copyDBClusterSnapshot_preSignedUrl = Lens.lens (\CopyDBClusterSnapshot' {preSignedUrl} -> preSignedUrl) (\s@CopyDBClusterSnapshot' {} a -> s {preSignedUrl = a} :: CopyDBClusterSnapshot)

-- | The identifier of the DB cluster snapshot to copy. This parameter isn\'t
-- case-sensitive.
--
-- You can\'t copy an encrypted, shared DB cluster snapshot from one AWS
-- Region to another.
--
-- Constraints:
--
-- -   Must specify a valid system snapshot in the \"available\" state.
--
-- -   If the source snapshot is in the same AWS Region as the copy,
--     specify a valid DB snapshot identifier.
--
-- -   If the source snapshot is in a different AWS Region than the copy,
--     specify a valid DB cluster snapshot ARN. For more information, go to
--     <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_CopySnapshot.html#USER_CopySnapshot.AcrossRegions Copying Snapshots Across AWS Regions>
--     in the /Amazon Aurora User Guide./
--
-- Example: @my-cluster-snapshot1@
copyDBClusterSnapshot_sourceDBClusterSnapshotIdentifier :: Lens.Lens' CopyDBClusterSnapshot Prelude.Text
copyDBClusterSnapshot_sourceDBClusterSnapshotIdentifier = Lens.lens (\CopyDBClusterSnapshot' {sourceDBClusterSnapshotIdentifier} -> sourceDBClusterSnapshotIdentifier) (\s@CopyDBClusterSnapshot' {} a -> s {sourceDBClusterSnapshotIdentifier = a} :: CopyDBClusterSnapshot)

-- | The identifier of the new DB cluster snapshot to create from the source
-- DB cluster snapshot. This parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens.
--
-- -   First character must be a letter.
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens.
--
-- Example: @my-cluster-snapshot2@
copyDBClusterSnapshot_targetDBClusterSnapshotIdentifier :: Lens.Lens' CopyDBClusterSnapshot Prelude.Text
copyDBClusterSnapshot_targetDBClusterSnapshotIdentifier = Lens.lens (\CopyDBClusterSnapshot' {targetDBClusterSnapshotIdentifier} -> targetDBClusterSnapshotIdentifier) (\s@CopyDBClusterSnapshot' {} a -> s {targetDBClusterSnapshotIdentifier = a} :: CopyDBClusterSnapshot)

instance Core.AWSRequest CopyDBClusterSnapshot where
  type
    AWSResponse CopyDBClusterSnapshot =
      CopyDBClusterSnapshotResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CopyDBClusterSnapshotResult"
      ( \s h x ->
          CopyDBClusterSnapshotResponse'
            Prelude.<$> (x Core..@? "DBClusterSnapshot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CopyDBClusterSnapshot

instance Prelude.NFData CopyDBClusterSnapshot

instance Core.ToHeaders CopyDBClusterSnapshot where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CopyDBClusterSnapshot where
  toPath = Prelude.const "/"

instance Core.ToQuery CopyDBClusterSnapshot where
  toQuery CopyDBClusterSnapshot' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CopyDBClusterSnapshot" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "KmsKeyId" Core.=: kmsKeyId,
        "CopyTags" Core.=: copyTags,
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "Tag" Prelude.<$> tags),
        "PreSignedUrl" Core.=: preSignedUrl,
        "SourceDBClusterSnapshotIdentifier"
          Core.=: sourceDBClusterSnapshotIdentifier,
        "TargetDBClusterSnapshotIdentifier"
          Core.=: targetDBClusterSnapshotIdentifier
      ]

-- | /See:/ 'newCopyDBClusterSnapshotResponse' smart constructor.
data CopyDBClusterSnapshotResponse = CopyDBClusterSnapshotResponse'
  { dbClusterSnapshot :: Prelude.Maybe DBClusterSnapshot,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopyDBClusterSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterSnapshot', 'copyDBClusterSnapshotResponse_dbClusterSnapshot' - Undocumented member.
--
-- 'httpStatus', 'copyDBClusterSnapshotResponse_httpStatus' - The response's http status code.
newCopyDBClusterSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CopyDBClusterSnapshotResponse
newCopyDBClusterSnapshotResponse pHttpStatus_ =
  CopyDBClusterSnapshotResponse'
    { dbClusterSnapshot =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
copyDBClusterSnapshotResponse_dbClusterSnapshot :: Lens.Lens' CopyDBClusterSnapshotResponse (Prelude.Maybe DBClusterSnapshot)
copyDBClusterSnapshotResponse_dbClusterSnapshot = Lens.lens (\CopyDBClusterSnapshotResponse' {dbClusterSnapshot} -> dbClusterSnapshot) (\s@CopyDBClusterSnapshotResponse' {} a -> s {dbClusterSnapshot = a} :: CopyDBClusterSnapshotResponse)

-- | The response's http status code.
copyDBClusterSnapshotResponse_httpStatus :: Lens.Lens' CopyDBClusterSnapshotResponse Prelude.Int
copyDBClusterSnapshotResponse_httpStatus = Lens.lens (\CopyDBClusterSnapshotResponse' {httpStatus} -> httpStatus) (\s@CopyDBClusterSnapshotResponse' {} a -> s {httpStatus = a} :: CopyDBClusterSnapshotResponse)

instance Prelude.NFData CopyDBClusterSnapshotResponse
