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
-- Module      : Network.AWS.RDS.CopyDBSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified DB snapshot. The source DB snapshot must be in the
-- @available@ state.
--
-- You can copy a snapshot from one AWS Region to another. In that case,
-- the AWS Region where you call the @CopyDBSnapshot@ action is the
-- destination AWS Region for the DB snapshot copy.
--
-- For more information about copying snapshots, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_CopySnapshot.html#USER_CopyDBSnapshot Copying a DB Snapshot>
-- in the /Amazon RDS User Guide./
module Network.AWS.RDS.CopyDBSnapshot
  ( -- * Creating a Request
    CopyDBSnapshot (..),
    newCopyDBSnapshot,

    -- * Request Lenses
    copyDBSnapshot_optionGroupName,
    copyDBSnapshot_kmsKeyId,
    copyDBSnapshot_copyTags,
    copyDBSnapshot_tags,
    copyDBSnapshot_preSignedUrl,
    copyDBSnapshot_targetCustomAvailabilityZone,
    copyDBSnapshot_sourceDBSnapshotIdentifier,
    copyDBSnapshot_targetDBSnapshotIdentifier,

    -- * Destructuring the Response
    CopyDBSnapshotResponse (..),
    newCopyDBSnapshotResponse,

    -- * Response Lenses
    copyDBSnapshotResponse_dbSnapshot,
    copyDBSnapshotResponse_httpStatus,
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
-- /See:/ 'newCopyDBSnapshot' smart constructor.
data CopyDBSnapshot = CopyDBSnapshot'
  { -- | The name of an option group to associate with the copy of the snapshot.
    --
    -- Specify this option if you are copying a snapshot from one AWS Region to
    -- another, and your DB instance uses a nondefault option group. If your
    -- source DB instance uses Transparent Data Encryption for Oracle or
    -- Microsoft SQL Server, you must specify this option when copying across
    -- AWS Regions. For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_CopySnapshot.html#USER_CopySnapshot.Options Option group considerations>
    -- in the /Amazon RDS User Guide./
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | The AWS KMS key identifier for an encrypted DB snapshot. The AWS KMS key
    -- identifier is the key ARN, key ID, alias ARN, or alias name for the AWS
    -- KMS customer master key (CMK).
    --
    -- If you copy an encrypted DB snapshot from your AWS account, you can
    -- specify a value for this parameter to encrypt the copy with a new AWS
    -- KMS CMK. If you don\'t specify a value for this parameter, then the copy
    -- of the DB snapshot is encrypted with the same AWS KMS key as the source
    -- DB snapshot.
    --
    -- If you copy an encrypted DB snapshot that is shared from another AWS
    -- account, then you must specify a value for this parameter.
    --
    -- If you specify this parameter when you copy an unencrypted snapshot, the
    -- copy is encrypted.
    --
    -- If you copy an encrypted snapshot to a different AWS Region, then you
    -- must specify a AWS KMS key identifier for the destination AWS Region.
    -- AWS KMS CMKs are specific to the AWS Region that they are created in,
    -- and you can\'t use CMKs from one AWS Region in another AWS Region.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to copy all tags from the source DB
    -- snapshot to the target DB snapshot. By default, tags are not copied.
    copyTags :: Prelude.Maybe Prelude.Bool,
    tags :: Prelude.Maybe [Tag],
    -- | The URL that contains a Signature Version 4 signed request for the
    -- @CopyDBSnapshot@ API action in the source AWS Region that contains the
    -- source DB snapshot to copy.
    --
    -- You must specify this parameter when you copy an encrypted DB snapshot
    -- from another AWS Region by using the Amazon RDS API. Don\'t specify
    -- @PreSignedUrl@ when you are copying an encrypted DB snapshot in the same
    -- AWS Region.
    --
    -- The presigned URL must be a valid request for the @CopyDBSnapshot@ API
    -- action that can be executed in the source AWS Region that contains the
    -- encrypted DB snapshot to be copied. The presigned URL request must
    -- contain the following parameter values:
    --
    -- -   @DestinationRegion@ - The AWS Region that the encrypted DB snapshot
    --     is copied to. This AWS Region is the same one where the
    --     @CopyDBSnapshot@ action is called that contains this presigned URL.
    --
    --     For example, if you copy an encrypted DB snapshot from the us-west-2
    --     AWS Region to the us-east-1 AWS Region, then you call the
    --     @CopyDBSnapshot@ action in the us-east-1 AWS Region and provide a
    --     presigned URL that contains a call to the @CopyDBSnapshot@ action in
    --     the us-west-2 AWS Region. For this example, the @DestinationRegion@
    --     in the presigned URL must be set to the us-east-1 AWS Region.
    --
    -- -   @KmsKeyId@ - The AWS KMS key identifier for the customer master key
    --     (CMK) to use to encrypt the copy of the DB snapshot in the
    --     destination AWS Region. This is the same identifier for both the
    --     @CopyDBSnapshot@ action that is called in the destination AWS
    --     Region, and the action contained in the presigned URL.
    --
    -- -   @SourceDBSnapshotIdentifier@ - The DB snapshot identifier for the
    --     encrypted snapshot to be copied. This identifier must be in the
    --     Amazon Resource Name (ARN) format for the source AWS Region. For
    --     example, if you are copying an encrypted DB snapshot from the
    --     us-west-2 AWS Region, then your @SourceDBSnapshotIdentifier@ looks
    --     like the following example:
    --     @arn:aws:rds:us-west-2:123456789012:snapshot:mysql-instance1-snapshot-20161115@.
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
    -- | The external custom Availability Zone (CAZ) identifier for the target
    -- CAZ.
    --
    -- Example: @rds-caz-aiqhTgQv@.
    targetCustomAvailabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the source DB snapshot.
    --
    -- If the source snapshot is in the same AWS Region as the copy, specify a
    -- valid DB snapshot identifier. For example, you might specify
    -- @rds:mysql-instance1-snapshot-20130805@.
    --
    -- If the source snapshot is in a different AWS Region than the copy,
    -- specify a valid DB snapshot ARN. For example, you might specify
    -- @arn:aws:rds:us-west-2:123456789012:snapshot:mysql-instance1-snapshot-20130805@.
    --
    -- If you are copying from a shared manual DB snapshot, this parameter must
    -- be the Amazon Resource Name (ARN) of the shared DB snapshot.
    --
    -- If you are copying an encrypted snapshot this parameter must be in the
    -- ARN format for the source AWS Region, and must match the
    -- @SourceDBSnapshotIdentifier@ in the @PreSignedUrl@ parameter.
    --
    -- Constraints:
    --
    -- -   Must specify a valid system snapshot in the \"available\" state.
    --
    -- Example: @rds:mydb-2012-04-02-00-01@
    --
    -- Example:
    -- @arn:aws:rds:us-west-2:123456789012:snapshot:mysql-instance1-snapshot-20130805@
    sourceDBSnapshotIdentifier :: Prelude.Text,
    -- | The identifier for the copy of the snapshot.
    --
    -- Constraints:
    --
    -- -   Can\'t be null, empty, or blank
    --
    -- -   Must contain from 1 to 255 letters, numbers, or hyphens
    --
    -- -   First character must be a letter
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens
    --
    -- Example: @my-db-snapshot@
    targetDBSnapshotIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopyDBSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'optionGroupName', 'copyDBSnapshot_optionGroupName' - The name of an option group to associate with the copy of the snapshot.
--
-- Specify this option if you are copying a snapshot from one AWS Region to
-- another, and your DB instance uses a nondefault option group. If your
-- source DB instance uses Transparent Data Encryption for Oracle or
-- Microsoft SQL Server, you must specify this option when copying across
-- AWS Regions. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_CopySnapshot.html#USER_CopySnapshot.Options Option group considerations>
-- in the /Amazon RDS User Guide./
--
-- 'kmsKeyId', 'copyDBSnapshot_kmsKeyId' - The AWS KMS key identifier for an encrypted DB snapshot. The AWS KMS key
-- identifier is the key ARN, key ID, alias ARN, or alias name for the AWS
-- KMS customer master key (CMK).
--
-- If you copy an encrypted DB snapshot from your AWS account, you can
-- specify a value for this parameter to encrypt the copy with a new AWS
-- KMS CMK. If you don\'t specify a value for this parameter, then the copy
-- of the DB snapshot is encrypted with the same AWS KMS key as the source
-- DB snapshot.
--
-- If you copy an encrypted DB snapshot that is shared from another AWS
-- account, then you must specify a value for this parameter.
--
-- If you specify this parameter when you copy an unencrypted snapshot, the
-- copy is encrypted.
--
-- If you copy an encrypted snapshot to a different AWS Region, then you
-- must specify a AWS KMS key identifier for the destination AWS Region.
-- AWS KMS CMKs are specific to the AWS Region that they are created in,
-- and you can\'t use CMKs from one AWS Region in another AWS Region.
--
-- 'copyTags', 'copyDBSnapshot_copyTags' - A value that indicates whether to copy all tags from the source DB
-- snapshot to the target DB snapshot. By default, tags are not copied.
--
-- 'tags', 'copyDBSnapshot_tags' - Undocumented member.
--
-- 'preSignedUrl', 'copyDBSnapshot_preSignedUrl' - The URL that contains a Signature Version 4 signed request for the
-- @CopyDBSnapshot@ API action in the source AWS Region that contains the
-- source DB snapshot to copy.
--
-- You must specify this parameter when you copy an encrypted DB snapshot
-- from another AWS Region by using the Amazon RDS API. Don\'t specify
-- @PreSignedUrl@ when you are copying an encrypted DB snapshot in the same
-- AWS Region.
--
-- The presigned URL must be a valid request for the @CopyDBSnapshot@ API
-- action that can be executed in the source AWS Region that contains the
-- encrypted DB snapshot to be copied. The presigned URL request must
-- contain the following parameter values:
--
-- -   @DestinationRegion@ - The AWS Region that the encrypted DB snapshot
--     is copied to. This AWS Region is the same one where the
--     @CopyDBSnapshot@ action is called that contains this presigned URL.
--
--     For example, if you copy an encrypted DB snapshot from the us-west-2
--     AWS Region to the us-east-1 AWS Region, then you call the
--     @CopyDBSnapshot@ action in the us-east-1 AWS Region and provide a
--     presigned URL that contains a call to the @CopyDBSnapshot@ action in
--     the us-west-2 AWS Region. For this example, the @DestinationRegion@
--     in the presigned URL must be set to the us-east-1 AWS Region.
--
-- -   @KmsKeyId@ - The AWS KMS key identifier for the customer master key
--     (CMK) to use to encrypt the copy of the DB snapshot in the
--     destination AWS Region. This is the same identifier for both the
--     @CopyDBSnapshot@ action that is called in the destination AWS
--     Region, and the action contained in the presigned URL.
--
-- -   @SourceDBSnapshotIdentifier@ - The DB snapshot identifier for the
--     encrypted snapshot to be copied. This identifier must be in the
--     Amazon Resource Name (ARN) format for the source AWS Region. For
--     example, if you are copying an encrypted DB snapshot from the
--     us-west-2 AWS Region, then your @SourceDBSnapshotIdentifier@ looks
--     like the following example:
--     @arn:aws:rds:us-west-2:123456789012:snapshot:mysql-instance1-snapshot-20161115@.
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
-- 'targetCustomAvailabilityZone', 'copyDBSnapshot_targetCustomAvailabilityZone' - The external custom Availability Zone (CAZ) identifier for the target
-- CAZ.
--
-- Example: @rds-caz-aiqhTgQv@.
--
-- 'sourceDBSnapshotIdentifier', 'copyDBSnapshot_sourceDBSnapshotIdentifier' - The identifier for the source DB snapshot.
--
-- If the source snapshot is in the same AWS Region as the copy, specify a
-- valid DB snapshot identifier. For example, you might specify
-- @rds:mysql-instance1-snapshot-20130805@.
--
-- If the source snapshot is in a different AWS Region than the copy,
-- specify a valid DB snapshot ARN. For example, you might specify
-- @arn:aws:rds:us-west-2:123456789012:snapshot:mysql-instance1-snapshot-20130805@.
--
-- If you are copying from a shared manual DB snapshot, this parameter must
-- be the Amazon Resource Name (ARN) of the shared DB snapshot.
--
-- If you are copying an encrypted snapshot this parameter must be in the
-- ARN format for the source AWS Region, and must match the
-- @SourceDBSnapshotIdentifier@ in the @PreSignedUrl@ parameter.
--
-- Constraints:
--
-- -   Must specify a valid system snapshot in the \"available\" state.
--
-- Example: @rds:mydb-2012-04-02-00-01@
--
-- Example:
-- @arn:aws:rds:us-west-2:123456789012:snapshot:mysql-instance1-snapshot-20130805@
--
-- 'targetDBSnapshotIdentifier', 'copyDBSnapshot_targetDBSnapshotIdentifier' - The identifier for the copy of the snapshot.
--
-- Constraints:
--
-- -   Can\'t be null, empty, or blank
--
-- -   Must contain from 1 to 255 letters, numbers, or hyphens
--
-- -   First character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
--
-- Example: @my-db-snapshot@
newCopyDBSnapshot ::
  -- | 'sourceDBSnapshotIdentifier'
  Prelude.Text ->
  -- | 'targetDBSnapshotIdentifier'
  Prelude.Text ->
  CopyDBSnapshot
newCopyDBSnapshot
  pSourceDBSnapshotIdentifier_
  pTargetDBSnapshotIdentifier_ =
    CopyDBSnapshot'
      { optionGroupName = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        copyTags = Prelude.Nothing,
        tags = Prelude.Nothing,
        preSignedUrl = Prelude.Nothing,
        targetCustomAvailabilityZone = Prelude.Nothing,
        sourceDBSnapshotIdentifier =
          pSourceDBSnapshotIdentifier_,
        targetDBSnapshotIdentifier =
          pTargetDBSnapshotIdentifier_
      }

-- | The name of an option group to associate with the copy of the snapshot.
--
-- Specify this option if you are copying a snapshot from one AWS Region to
-- another, and your DB instance uses a nondefault option group. If your
-- source DB instance uses Transparent Data Encryption for Oracle or
-- Microsoft SQL Server, you must specify this option when copying across
-- AWS Regions. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_CopySnapshot.html#USER_CopySnapshot.Options Option group considerations>
-- in the /Amazon RDS User Guide./
copyDBSnapshot_optionGroupName :: Lens.Lens' CopyDBSnapshot (Prelude.Maybe Prelude.Text)
copyDBSnapshot_optionGroupName = Lens.lens (\CopyDBSnapshot' {optionGroupName} -> optionGroupName) (\s@CopyDBSnapshot' {} a -> s {optionGroupName = a} :: CopyDBSnapshot)

-- | The AWS KMS key identifier for an encrypted DB snapshot. The AWS KMS key
-- identifier is the key ARN, key ID, alias ARN, or alias name for the AWS
-- KMS customer master key (CMK).
--
-- If you copy an encrypted DB snapshot from your AWS account, you can
-- specify a value for this parameter to encrypt the copy with a new AWS
-- KMS CMK. If you don\'t specify a value for this parameter, then the copy
-- of the DB snapshot is encrypted with the same AWS KMS key as the source
-- DB snapshot.
--
-- If you copy an encrypted DB snapshot that is shared from another AWS
-- account, then you must specify a value for this parameter.
--
-- If you specify this parameter when you copy an unencrypted snapshot, the
-- copy is encrypted.
--
-- If you copy an encrypted snapshot to a different AWS Region, then you
-- must specify a AWS KMS key identifier for the destination AWS Region.
-- AWS KMS CMKs are specific to the AWS Region that they are created in,
-- and you can\'t use CMKs from one AWS Region in another AWS Region.
copyDBSnapshot_kmsKeyId :: Lens.Lens' CopyDBSnapshot (Prelude.Maybe Prelude.Text)
copyDBSnapshot_kmsKeyId = Lens.lens (\CopyDBSnapshot' {kmsKeyId} -> kmsKeyId) (\s@CopyDBSnapshot' {} a -> s {kmsKeyId = a} :: CopyDBSnapshot)

-- | A value that indicates whether to copy all tags from the source DB
-- snapshot to the target DB snapshot. By default, tags are not copied.
copyDBSnapshot_copyTags :: Lens.Lens' CopyDBSnapshot (Prelude.Maybe Prelude.Bool)
copyDBSnapshot_copyTags = Lens.lens (\CopyDBSnapshot' {copyTags} -> copyTags) (\s@CopyDBSnapshot' {} a -> s {copyTags = a} :: CopyDBSnapshot)

-- | Undocumented member.
copyDBSnapshot_tags :: Lens.Lens' CopyDBSnapshot (Prelude.Maybe [Tag])
copyDBSnapshot_tags = Lens.lens (\CopyDBSnapshot' {tags} -> tags) (\s@CopyDBSnapshot' {} a -> s {tags = a} :: CopyDBSnapshot) Prelude.. Lens.mapping Lens._Coerce

-- | The URL that contains a Signature Version 4 signed request for the
-- @CopyDBSnapshot@ API action in the source AWS Region that contains the
-- source DB snapshot to copy.
--
-- You must specify this parameter when you copy an encrypted DB snapshot
-- from another AWS Region by using the Amazon RDS API. Don\'t specify
-- @PreSignedUrl@ when you are copying an encrypted DB snapshot in the same
-- AWS Region.
--
-- The presigned URL must be a valid request for the @CopyDBSnapshot@ API
-- action that can be executed in the source AWS Region that contains the
-- encrypted DB snapshot to be copied. The presigned URL request must
-- contain the following parameter values:
--
-- -   @DestinationRegion@ - The AWS Region that the encrypted DB snapshot
--     is copied to. This AWS Region is the same one where the
--     @CopyDBSnapshot@ action is called that contains this presigned URL.
--
--     For example, if you copy an encrypted DB snapshot from the us-west-2
--     AWS Region to the us-east-1 AWS Region, then you call the
--     @CopyDBSnapshot@ action in the us-east-1 AWS Region and provide a
--     presigned URL that contains a call to the @CopyDBSnapshot@ action in
--     the us-west-2 AWS Region. For this example, the @DestinationRegion@
--     in the presigned URL must be set to the us-east-1 AWS Region.
--
-- -   @KmsKeyId@ - The AWS KMS key identifier for the customer master key
--     (CMK) to use to encrypt the copy of the DB snapshot in the
--     destination AWS Region. This is the same identifier for both the
--     @CopyDBSnapshot@ action that is called in the destination AWS
--     Region, and the action contained in the presigned URL.
--
-- -   @SourceDBSnapshotIdentifier@ - The DB snapshot identifier for the
--     encrypted snapshot to be copied. This identifier must be in the
--     Amazon Resource Name (ARN) format for the source AWS Region. For
--     example, if you are copying an encrypted DB snapshot from the
--     us-west-2 AWS Region, then your @SourceDBSnapshotIdentifier@ looks
--     like the following example:
--     @arn:aws:rds:us-west-2:123456789012:snapshot:mysql-instance1-snapshot-20161115@.
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
copyDBSnapshot_preSignedUrl :: Lens.Lens' CopyDBSnapshot (Prelude.Maybe Prelude.Text)
copyDBSnapshot_preSignedUrl = Lens.lens (\CopyDBSnapshot' {preSignedUrl} -> preSignedUrl) (\s@CopyDBSnapshot' {} a -> s {preSignedUrl = a} :: CopyDBSnapshot)

-- | The external custom Availability Zone (CAZ) identifier for the target
-- CAZ.
--
-- Example: @rds-caz-aiqhTgQv@.
copyDBSnapshot_targetCustomAvailabilityZone :: Lens.Lens' CopyDBSnapshot (Prelude.Maybe Prelude.Text)
copyDBSnapshot_targetCustomAvailabilityZone = Lens.lens (\CopyDBSnapshot' {targetCustomAvailabilityZone} -> targetCustomAvailabilityZone) (\s@CopyDBSnapshot' {} a -> s {targetCustomAvailabilityZone = a} :: CopyDBSnapshot)

-- | The identifier for the source DB snapshot.
--
-- If the source snapshot is in the same AWS Region as the copy, specify a
-- valid DB snapshot identifier. For example, you might specify
-- @rds:mysql-instance1-snapshot-20130805@.
--
-- If the source snapshot is in a different AWS Region than the copy,
-- specify a valid DB snapshot ARN. For example, you might specify
-- @arn:aws:rds:us-west-2:123456789012:snapshot:mysql-instance1-snapshot-20130805@.
--
-- If you are copying from a shared manual DB snapshot, this parameter must
-- be the Amazon Resource Name (ARN) of the shared DB snapshot.
--
-- If you are copying an encrypted snapshot this parameter must be in the
-- ARN format for the source AWS Region, and must match the
-- @SourceDBSnapshotIdentifier@ in the @PreSignedUrl@ parameter.
--
-- Constraints:
--
-- -   Must specify a valid system snapshot in the \"available\" state.
--
-- Example: @rds:mydb-2012-04-02-00-01@
--
-- Example:
-- @arn:aws:rds:us-west-2:123456789012:snapshot:mysql-instance1-snapshot-20130805@
copyDBSnapshot_sourceDBSnapshotIdentifier :: Lens.Lens' CopyDBSnapshot Prelude.Text
copyDBSnapshot_sourceDBSnapshotIdentifier = Lens.lens (\CopyDBSnapshot' {sourceDBSnapshotIdentifier} -> sourceDBSnapshotIdentifier) (\s@CopyDBSnapshot' {} a -> s {sourceDBSnapshotIdentifier = a} :: CopyDBSnapshot)

-- | The identifier for the copy of the snapshot.
--
-- Constraints:
--
-- -   Can\'t be null, empty, or blank
--
-- -   Must contain from 1 to 255 letters, numbers, or hyphens
--
-- -   First character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
--
-- Example: @my-db-snapshot@
copyDBSnapshot_targetDBSnapshotIdentifier :: Lens.Lens' CopyDBSnapshot Prelude.Text
copyDBSnapshot_targetDBSnapshotIdentifier = Lens.lens (\CopyDBSnapshot' {targetDBSnapshotIdentifier} -> targetDBSnapshotIdentifier) (\s@CopyDBSnapshot' {} a -> s {targetDBSnapshotIdentifier = a} :: CopyDBSnapshot)

instance Core.AWSRequest CopyDBSnapshot where
  type
    AWSResponse CopyDBSnapshot =
      CopyDBSnapshotResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CopyDBSnapshotResult"
      ( \s h x ->
          CopyDBSnapshotResponse'
            Prelude.<$> (x Core..@? "DBSnapshot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CopyDBSnapshot

instance Prelude.NFData CopyDBSnapshot

instance Core.ToHeaders CopyDBSnapshot where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CopyDBSnapshot where
  toPath = Prelude.const "/"

instance Core.ToQuery CopyDBSnapshot where
  toQuery CopyDBSnapshot' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CopyDBSnapshot" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "OptionGroupName" Core.=: optionGroupName,
        "KmsKeyId" Core.=: kmsKeyId,
        "CopyTags" Core.=: copyTags,
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "Tag" Prelude.<$> tags),
        "PreSignedUrl" Core.=: preSignedUrl,
        "TargetCustomAvailabilityZone"
          Core.=: targetCustomAvailabilityZone,
        "SourceDBSnapshotIdentifier"
          Core.=: sourceDBSnapshotIdentifier,
        "TargetDBSnapshotIdentifier"
          Core.=: targetDBSnapshotIdentifier
      ]

-- | /See:/ 'newCopyDBSnapshotResponse' smart constructor.
data CopyDBSnapshotResponse = CopyDBSnapshotResponse'
  { dbSnapshot :: Prelude.Maybe DBSnapshot,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopyDBSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbSnapshot', 'copyDBSnapshotResponse_dbSnapshot' - Undocumented member.
--
-- 'httpStatus', 'copyDBSnapshotResponse_httpStatus' - The response's http status code.
newCopyDBSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CopyDBSnapshotResponse
newCopyDBSnapshotResponse pHttpStatus_ =
  CopyDBSnapshotResponse'
    { dbSnapshot =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
copyDBSnapshotResponse_dbSnapshot :: Lens.Lens' CopyDBSnapshotResponse (Prelude.Maybe DBSnapshot)
copyDBSnapshotResponse_dbSnapshot = Lens.lens (\CopyDBSnapshotResponse' {dbSnapshot} -> dbSnapshot) (\s@CopyDBSnapshotResponse' {} a -> s {dbSnapshot = a} :: CopyDBSnapshotResponse)

-- | The response's http status code.
copyDBSnapshotResponse_httpStatus :: Lens.Lens' CopyDBSnapshotResponse Prelude.Int
copyDBSnapshotResponse_httpStatus = Lens.lens (\CopyDBSnapshotResponse' {httpStatus} -> httpStatus) (\s@CopyDBSnapshotResponse' {} a -> s {httpStatus = a} :: CopyDBSnapshotResponse)

instance Prelude.NFData CopyDBSnapshotResponse
