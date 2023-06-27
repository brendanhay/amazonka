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
-- Module      : Amazonka.RDS.CopyDBClusterSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- You can copy an encrypted DB cluster snapshot from another Amazon Web
-- Services Region. In that case, the Amazon Web Services Region where you
-- call the @CopyDBClusterSnapshot@ operation is the destination Amazon Web
-- Services Region for the encrypted DB cluster snapshot to be copied to.
-- To copy an encrypted DB cluster snapshot from another Amazon Web
-- Services Region, you must provide the following values:
--
-- -   @KmsKeyId@ - The Amazon Web Services Key Management System (Amazon
--     Web Services KMS) key identifier for the key to use to encrypt the
--     copy of the DB cluster snapshot in the destination Amazon Web
--     Services Region.
--
-- -   @TargetDBClusterSnapshotIdentifier@ - The identifier for the new
--     copy of the DB cluster snapshot in the destination Amazon Web
--     Services Region.
--
-- -   @SourceDBClusterSnapshotIdentifier@ - The DB cluster snapshot
--     identifier for the encrypted DB cluster snapshot to be copied. This
--     identifier must be in the ARN format for the source Amazon Web
--     Services Region and is the same value as the
--     @SourceDBClusterSnapshotIdentifier@ in the presigned URL.
--
-- To cancel the copy operation once it is in progress, delete the target
-- DB cluster snapshot identified by @TargetDBClusterSnapshotIdentifier@
-- while that DB cluster snapshot is in \"copying\" status.
--
-- For more information on copying encrypted Amazon Aurora DB cluster
-- snapshots from one Amazon Web Services Region to another, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_CopySnapshot.html Copying a Snapshot>
-- in the /Amazon Aurora User Guide/.
--
-- For more information on Amazon Aurora DB clusters, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What is Amazon Aurora?>
-- in the /Amazon Aurora User Guide/.
--
-- For more information on Multi-AZ DB clusters, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/multi-az-db-clusters-concepts.html Multi-AZ DB cluster deployments>
-- in the /Amazon RDS User Guide/.
module Amazonka.RDS.CopyDBClusterSnapshot
  ( -- * Creating a Request
    CopyDBClusterSnapshot (..),
    newCopyDBClusterSnapshot,

    -- * Request Lenses
    copyDBClusterSnapshot_copyTags,
    copyDBClusterSnapshot_destinationRegion,
    copyDBClusterSnapshot_kmsKeyId,
    copyDBClusterSnapshot_preSignedUrl,
    copyDBClusterSnapshot_tags,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newCopyDBClusterSnapshot' smart constructor.
data CopyDBClusterSnapshot = CopyDBClusterSnapshot'
  { -- | A value that indicates whether to copy all tags from the source DB
    -- cluster snapshot to the target DB cluster snapshot. By default, tags are
    -- not copied.
    copyTags :: Prelude.Maybe Prelude.Bool,
    -- | Pseudo-parameter used when populating the @PreSignedUrl@ of a
    -- cross-region @CopyDBClusterSnapshot@ request. To replicate from region
    -- @SRC@ to region @DST@, send a request to region @DST@. In that request,
    -- pass a @PreSignedUrl@ for region @SRC@ with @DestinationRegion@ set to
    -- region @DST@.
    destinationRegion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services KMS key identifier for an encrypted DB cluster
    -- snapshot. The Amazon Web Services KMS key identifier is the key ARN, key
    -- ID, alias ARN, or alias name for the Amazon Web Services KMS key.
    --
    -- If you copy an encrypted DB cluster snapshot from your Amazon Web
    -- Services account, you can specify a value for @KmsKeyId@ to encrypt the
    -- copy with a new KMS key. If you don\'t specify a value for @KmsKeyId@,
    -- then the copy of the DB cluster snapshot is encrypted with the same KMS
    -- key as the source DB cluster snapshot.
    --
    -- If you copy an encrypted DB cluster snapshot that is shared from another
    -- Amazon Web Services account, then you must specify a value for
    -- @KmsKeyId@.
    --
    -- To copy an encrypted DB cluster snapshot to another Amazon Web Services
    -- Region, you must set @KmsKeyId@ to the Amazon Web Services KMS key
    -- identifier you want to use to encrypt the copy of the DB cluster
    -- snapshot in the destination Amazon Web Services Region. KMS keys are
    -- specific to the Amazon Web Services Region that they are created in, and
    -- you can\'t use KMS keys from one Amazon Web Services Region in another
    -- Amazon Web Services Region.
    --
    -- If you copy an unencrypted DB cluster snapshot and specify a value for
    -- the @KmsKeyId@ parameter, an error is returned.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | When you are copying a DB cluster snapshot from one Amazon Web Services
    -- GovCloud (US) Region to another, the URL that contains a Signature
    -- Version 4 signed request for the @CopyDBClusterSnapshot@ API operation
    -- in the Amazon Web Services Region that contains the source DB cluster
    -- snapshot to copy. Use the @PreSignedUrl@ parameter when copying an
    -- encrypted DB cluster snapshot from another Amazon Web Services Region.
    -- Don\'t specify @PreSignedUrl@ when copying an encrypted DB cluster
    -- snapshot in the same Amazon Web Services Region.
    --
    -- This setting applies only to Amazon Web Services GovCloud (US) Regions.
    -- It\'s ignored in other Amazon Web Services Regions.
    --
    -- The presigned URL must be a valid request for the
    -- @CopyDBClusterSnapshot@ API operation that can run in the source Amazon
    -- Web Services Region that contains the encrypted DB cluster snapshot to
    -- copy. The presigned URL request must contain the following parameter
    -- values:
    --
    -- -   @KmsKeyId@ - The KMS key identifier for the KMS key to use to
    --     encrypt the copy of the DB cluster snapshot in the destination
    --     Amazon Web Services Region. This is the same identifier for both the
    --     @CopyDBClusterSnapshot@ operation that is called in the destination
    --     Amazon Web Services Region, and the operation contained in the
    --     presigned URL.
    --
    -- -   @DestinationRegion@ - The name of the Amazon Web Services Region
    --     that the DB cluster snapshot is to be created in.
    --
    -- -   @SourceDBClusterSnapshotIdentifier@ - The DB cluster snapshot
    --     identifier for the encrypted DB cluster snapshot to be copied. This
    --     identifier must be in the Amazon Resource Name (ARN) format for the
    --     source Amazon Web Services Region. For example, if you are copying
    --     an encrypted DB cluster snapshot from the us-west-2 Amazon Web
    --     Services Region, then your @SourceDBClusterSnapshotIdentifier@ looks
    --     like the following example:
    --     @arn:aws:rds:us-west-2:123456789012:cluster-snapshot:aurora-cluster1-snapshot-20161115@.
    --
    -- To learn how to generate a Signature Version 4 signed request, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (Amazon Web Services Signature Version 4)>
    -- and
    -- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process>.
    --
    -- If you are using an Amazon Web Services SDK tool or the CLI, you can
    -- specify @SourceRegion@ (or @--source-region@ for the CLI) instead of
    -- specifying @PreSignedUrl@ manually. Specifying @SourceRegion@
    -- autogenerates a presigned URL that is a valid request for the operation
    -- that can run in the source Amazon Web Services Region.
    preSignedUrl :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe [Tag],
    -- | The identifier of the DB cluster snapshot to copy. This parameter isn\'t
    -- case-sensitive.
    --
    -- You can\'t copy an encrypted, shared DB cluster snapshot from one Amazon
    -- Web Services Region to another.
    --
    -- Constraints:
    --
    -- -   Must specify a valid system snapshot in the \"available\" state.
    --
    -- -   If the source snapshot is in the same Amazon Web Services Region as
    --     the copy, specify a valid DB snapshot identifier.
    --
    -- -   If the source snapshot is in a different Amazon Web Services Region
    --     than the copy, specify a valid DB cluster snapshot ARN. For more
    --     information, go to
    --     <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_CopySnapshot.html#USER_CopySnapshot.AcrossRegions Copying Snapshots Across Amazon Web Services Regions>
    --     in the /Amazon Aurora User Guide/.
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
-- 'copyTags', 'copyDBClusterSnapshot_copyTags' - A value that indicates whether to copy all tags from the source DB
-- cluster snapshot to the target DB cluster snapshot. By default, tags are
-- not copied.
--
-- 'destinationRegion', 'copyDBClusterSnapshot_destinationRegion' - Pseudo-parameter used when populating the @PreSignedUrl@ of a
-- cross-region @CopyDBClusterSnapshot@ request. To replicate from region
-- @SRC@ to region @DST@, send a request to region @DST@. In that request,
-- pass a @PreSignedUrl@ for region @SRC@ with @DestinationRegion@ set to
-- region @DST@.
--
-- 'kmsKeyId', 'copyDBClusterSnapshot_kmsKeyId' - The Amazon Web Services KMS key identifier for an encrypted DB cluster
-- snapshot. The Amazon Web Services KMS key identifier is the key ARN, key
-- ID, alias ARN, or alias name for the Amazon Web Services KMS key.
--
-- If you copy an encrypted DB cluster snapshot from your Amazon Web
-- Services account, you can specify a value for @KmsKeyId@ to encrypt the
-- copy with a new KMS key. If you don\'t specify a value for @KmsKeyId@,
-- then the copy of the DB cluster snapshot is encrypted with the same KMS
-- key as the source DB cluster snapshot.
--
-- If you copy an encrypted DB cluster snapshot that is shared from another
-- Amazon Web Services account, then you must specify a value for
-- @KmsKeyId@.
--
-- To copy an encrypted DB cluster snapshot to another Amazon Web Services
-- Region, you must set @KmsKeyId@ to the Amazon Web Services KMS key
-- identifier you want to use to encrypt the copy of the DB cluster
-- snapshot in the destination Amazon Web Services Region. KMS keys are
-- specific to the Amazon Web Services Region that they are created in, and
-- you can\'t use KMS keys from one Amazon Web Services Region in another
-- Amazon Web Services Region.
--
-- If you copy an unencrypted DB cluster snapshot and specify a value for
-- the @KmsKeyId@ parameter, an error is returned.
--
-- 'preSignedUrl', 'copyDBClusterSnapshot_preSignedUrl' - When you are copying a DB cluster snapshot from one Amazon Web Services
-- GovCloud (US) Region to another, the URL that contains a Signature
-- Version 4 signed request for the @CopyDBClusterSnapshot@ API operation
-- in the Amazon Web Services Region that contains the source DB cluster
-- snapshot to copy. Use the @PreSignedUrl@ parameter when copying an
-- encrypted DB cluster snapshot from another Amazon Web Services Region.
-- Don\'t specify @PreSignedUrl@ when copying an encrypted DB cluster
-- snapshot in the same Amazon Web Services Region.
--
-- This setting applies only to Amazon Web Services GovCloud (US) Regions.
-- It\'s ignored in other Amazon Web Services Regions.
--
-- The presigned URL must be a valid request for the
-- @CopyDBClusterSnapshot@ API operation that can run in the source Amazon
-- Web Services Region that contains the encrypted DB cluster snapshot to
-- copy. The presigned URL request must contain the following parameter
-- values:
--
-- -   @KmsKeyId@ - The KMS key identifier for the KMS key to use to
--     encrypt the copy of the DB cluster snapshot in the destination
--     Amazon Web Services Region. This is the same identifier for both the
--     @CopyDBClusterSnapshot@ operation that is called in the destination
--     Amazon Web Services Region, and the operation contained in the
--     presigned URL.
--
-- -   @DestinationRegion@ - The name of the Amazon Web Services Region
--     that the DB cluster snapshot is to be created in.
--
-- -   @SourceDBClusterSnapshotIdentifier@ - The DB cluster snapshot
--     identifier for the encrypted DB cluster snapshot to be copied. This
--     identifier must be in the Amazon Resource Name (ARN) format for the
--     source Amazon Web Services Region. For example, if you are copying
--     an encrypted DB cluster snapshot from the us-west-2 Amazon Web
--     Services Region, then your @SourceDBClusterSnapshotIdentifier@ looks
--     like the following example:
--     @arn:aws:rds:us-west-2:123456789012:cluster-snapshot:aurora-cluster1-snapshot-20161115@.
--
-- To learn how to generate a Signature Version 4 signed request, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (Amazon Web Services Signature Version 4)>
-- and
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process>.
--
-- If you are using an Amazon Web Services SDK tool or the CLI, you can
-- specify @SourceRegion@ (or @--source-region@ for the CLI) instead of
-- specifying @PreSignedUrl@ manually. Specifying @SourceRegion@
-- autogenerates a presigned URL that is a valid request for the operation
-- that can run in the source Amazon Web Services Region.
--
-- 'tags', 'copyDBClusterSnapshot_tags' - Undocumented member.
--
-- 'sourceDBClusterSnapshotIdentifier', 'copyDBClusterSnapshot_sourceDBClusterSnapshotIdentifier' - The identifier of the DB cluster snapshot to copy. This parameter isn\'t
-- case-sensitive.
--
-- You can\'t copy an encrypted, shared DB cluster snapshot from one Amazon
-- Web Services Region to another.
--
-- Constraints:
--
-- -   Must specify a valid system snapshot in the \"available\" state.
--
-- -   If the source snapshot is in the same Amazon Web Services Region as
--     the copy, specify a valid DB snapshot identifier.
--
-- -   If the source snapshot is in a different Amazon Web Services Region
--     than the copy, specify a valid DB cluster snapshot ARN. For more
--     information, go to
--     <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_CopySnapshot.html#USER_CopySnapshot.AcrossRegions Copying Snapshots Across Amazon Web Services Regions>
--     in the /Amazon Aurora User Guide/.
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
      { copyTags = Prelude.Nothing,
        destinationRegion = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        preSignedUrl = Prelude.Nothing,
        tags = Prelude.Nothing,
        sourceDBClusterSnapshotIdentifier =
          pSourceDBClusterSnapshotIdentifier_,
        targetDBClusterSnapshotIdentifier =
          pTargetDBClusterSnapshotIdentifier_
      }

-- | A value that indicates whether to copy all tags from the source DB
-- cluster snapshot to the target DB cluster snapshot. By default, tags are
-- not copied.
copyDBClusterSnapshot_copyTags :: Lens.Lens' CopyDBClusterSnapshot (Prelude.Maybe Prelude.Bool)
copyDBClusterSnapshot_copyTags = Lens.lens (\CopyDBClusterSnapshot' {copyTags} -> copyTags) (\s@CopyDBClusterSnapshot' {} a -> s {copyTags = a} :: CopyDBClusterSnapshot)

-- | Pseudo-parameter used when populating the @PreSignedUrl@ of a
-- cross-region @CopyDBClusterSnapshot@ request. To replicate from region
-- @SRC@ to region @DST@, send a request to region @DST@. In that request,
-- pass a @PreSignedUrl@ for region @SRC@ with @DestinationRegion@ set to
-- region @DST@.
copyDBClusterSnapshot_destinationRegion :: Lens.Lens' CopyDBClusterSnapshot (Prelude.Maybe Prelude.Text)
copyDBClusterSnapshot_destinationRegion = Lens.lens (\CopyDBClusterSnapshot' {destinationRegion} -> destinationRegion) (\s@CopyDBClusterSnapshot' {} a -> s {destinationRegion = a} :: CopyDBClusterSnapshot)

-- | The Amazon Web Services KMS key identifier for an encrypted DB cluster
-- snapshot. The Amazon Web Services KMS key identifier is the key ARN, key
-- ID, alias ARN, or alias name for the Amazon Web Services KMS key.
--
-- If you copy an encrypted DB cluster snapshot from your Amazon Web
-- Services account, you can specify a value for @KmsKeyId@ to encrypt the
-- copy with a new KMS key. If you don\'t specify a value for @KmsKeyId@,
-- then the copy of the DB cluster snapshot is encrypted with the same KMS
-- key as the source DB cluster snapshot.
--
-- If you copy an encrypted DB cluster snapshot that is shared from another
-- Amazon Web Services account, then you must specify a value for
-- @KmsKeyId@.
--
-- To copy an encrypted DB cluster snapshot to another Amazon Web Services
-- Region, you must set @KmsKeyId@ to the Amazon Web Services KMS key
-- identifier you want to use to encrypt the copy of the DB cluster
-- snapshot in the destination Amazon Web Services Region. KMS keys are
-- specific to the Amazon Web Services Region that they are created in, and
-- you can\'t use KMS keys from one Amazon Web Services Region in another
-- Amazon Web Services Region.
--
-- If you copy an unencrypted DB cluster snapshot and specify a value for
-- the @KmsKeyId@ parameter, an error is returned.
copyDBClusterSnapshot_kmsKeyId :: Lens.Lens' CopyDBClusterSnapshot (Prelude.Maybe Prelude.Text)
copyDBClusterSnapshot_kmsKeyId = Lens.lens (\CopyDBClusterSnapshot' {kmsKeyId} -> kmsKeyId) (\s@CopyDBClusterSnapshot' {} a -> s {kmsKeyId = a} :: CopyDBClusterSnapshot)

-- | When you are copying a DB cluster snapshot from one Amazon Web Services
-- GovCloud (US) Region to another, the URL that contains a Signature
-- Version 4 signed request for the @CopyDBClusterSnapshot@ API operation
-- in the Amazon Web Services Region that contains the source DB cluster
-- snapshot to copy. Use the @PreSignedUrl@ parameter when copying an
-- encrypted DB cluster snapshot from another Amazon Web Services Region.
-- Don\'t specify @PreSignedUrl@ when copying an encrypted DB cluster
-- snapshot in the same Amazon Web Services Region.
--
-- This setting applies only to Amazon Web Services GovCloud (US) Regions.
-- It\'s ignored in other Amazon Web Services Regions.
--
-- The presigned URL must be a valid request for the
-- @CopyDBClusterSnapshot@ API operation that can run in the source Amazon
-- Web Services Region that contains the encrypted DB cluster snapshot to
-- copy. The presigned URL request must contain the following parameter
-- values:
--
-- -   @KmsKeyId@ - The KMS key identifier for the KMS key to use to
--     encrypt the copy of the DB cluster snapshot in the destination
--     Amazon Web Services Region. This is the same identifier for both the
--     @CopyDBClusterSnapshot@ operation that is called in the destination
--     Amazon Web Services Region, and the operation contained in the
--     presigned URL.
--
-- -   @DestinationRegion@ - The name of the Amazon Web Services Region
--     that the DB cluster snapshot is to be created in.
--
-- -   @SourceDBClusterSnapshotIdentifier@ - The DB cluster snapshot
--     identifier for the encrypted DB cluster snapshot to be copied. This
--     identifier must be in the Amazon Resource Name (ARN) format for the
--     source Amazon Web Services Region. For example, if you are copying
--     an encrypted DB cluster snapshot from the us-west-2 Amazon Web
--     Services Region, then your @SourceDBClusterSnapshotIdentifier@ looks
--     like the following example:
--     @arn:aws:rds:us-west-2:123456789012:cluster-snapshot:aurora-cluster1-snapshot-20161115@.
--
-- To learn how to generate a Signature Version 4 signed request, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (Amazon Web Services Signature Version 4)>
-- and
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process>.
--
-- If you are using an Amazon Web Services SDK tool or the CLI, you can
-- specify @SourceRegion@ (or @--source-region@ for the CLI) instead of
-- specifying @PreSignedUrl@ manually. Specifying @SourceRegion@
-- autogenerates a presigned URL that is a valid request for the operation
-- that can run in the source Amazon Web Services Region.
copyDBClusterSnapshot_preSignedUrl :: Lens.Lens' CopyDBClusterSnapshot (Prelude.Maybe Prelude.Text)
copyDBClusterSnapshot_preSignedUrl = Lens.lens (\CopyDBClusterSnapshot' {preSignedUrl} -> preSignedUrl) (\s@CopyDBClusterSnapshot' {} a -> s {preSignedUrl = a} :: CopyDBClusterSnapshot)

-- | Undocumented member.
copyDBClusterSnapshot_tags :: Lens.Lens' CopyDBClusterSnapshot (Prelude.Maybe [Tag])
copyDBClusterSnapshot_tags = Lens.lens (\CopyDBClusterSnapshot' {tags} -> tags) (\s@CopyDBClusterSnapshot' {} a -> s {tags = a} :: CopyDBClusterSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the DB cluster snapshot to copy. This parameter isn\'t
-- case-sensitive.
--
-- You can\'t copy an encrypted, shared DB cluster snapshot from one Amazon
-- Web Services Region to another.
--
-- Constraints:
--
-- -   Must specify a valid system snapshot in the \"available\" state.
--
-- -   If the source snapshot is in the same Amazon Web Services Region as
--     the copy, specify a valid DB snapshot identifier.
--
-- -   If the source snapshot is in a different Amazon Web Services Region
--     than the copy, specify a valid DB cluster snapshot ARN. For more
--     information, go to
--     <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_CopySnapshot.html#USER_CopySnapshot.AcrossRegions Copying Snapshots Across Amazon Web Services Regions>
--     in the /Amazon Aurora User Guide/.
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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CopyDBClusterSnapshotResult"
      ( \s h x ->
          CopyDBClusterSnapshotResponse'
            Prelude.<$> (x Data..@? "DBClusterSnapshot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CopyDBClusterSnapshot where
  hashWithSalt _salt CopyDBClusterSnapshot' {..} =
    _salt
      `Prelude.hashWithSalt` copyTags
      `Prelude.hashWithSalt` destinationRegion
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` preSignedUrl
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` sourceDBClusterSnapshotIdentifier
      `Prelude.hashWithSalt` targetDBClusterSnapshotIdentifier

instance Prelude.NFData CopyDBClusterSnapshot where
  rnf CopyDBClusterSnapshot' {..} =
    Prelude.rnf copyTags
      `Prelude.seq` Prelude.rnf destinationRegion
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf preSignedUrl
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf sourceDBClusterSnapshotIdentifier
      `Prelude.seq` Prelude.rnf targetDBClusterSnapshotIdentifier

instance Data.ToHeaders CopyDBClusterSnapshot where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CopyDBClusterSnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery CopyDBClusterSnapshot where
  toQuery CopyDBClusterSnapshot' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CopyDBClusterSnapshot" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "CopyTags" Data.=: copyTags,
        "DestinationRegion" Data.=: destinationRegion,
        "KmsKeyId" Data.=: kmsKeyId,
        "PreSignedUrl" Data.=: preSignedUrl,
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "Tag" Prelude.<$> tags),
        "SourceDBClusterSnapshotIdentifier"
          Data.=: sourceDBClusterSnapshotIdentifier,
        "TargetDBClusterSnapshotIdentifier"
          Data.=: targetDBClusterSnapshotIdentifier
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

instance Prelude.NFData CopyDBClusterSnapshotResponse where
  rnf CopyDBClusterSnapshotResponse' {..} =
    Prelude.rnf dbClusterSnapshot
      `Prelude.seq` Prelude.rnf httpStatus
