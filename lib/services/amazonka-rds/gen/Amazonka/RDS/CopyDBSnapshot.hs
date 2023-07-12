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
-- Module      : Amazonka.RDS.CopyDBSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified DB snapshot. The source DB snapshot must be in the
-- @available@ state.
--
-- You can copy a snapshot from one Amazon Web Services Region to another.
-- In that case, the Amazon Web Services Region where you call the
-- @CopyDBSnapshot@ operation is the destination Amazon Web Services Region
-- for the DB snapshot copy.
--
-- This command doesn\'t apply to RDS Custom.
--
-- For more information about copying snapshots, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_CopySnapshot.html#USER_CopyDBSnapshot Copying a DB Snapshot>
-- in the /Amazon RDS User Guide/.
module Amazonka.RDS.CopyDBSnapshot
  ( -- * Creating a Request
    CopyDBSnapshot (..),
    newCopyDBSnapshot,

    -- * Request Lenses
    copyDBSnapshot_copyOptionGroup,
    copyDBSnapshot_copyTags,
    copyDBSnapshot_destinationRegion,
    copyDBSnapshot_kmsKeyId,
    copyDBSnapshot_optionGroupName,
    copyDBSnapshot_preSignedUrl,
    copyDBSnapshot_tags,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newCopyDBSnapshot' smart constructor.
data CopyDBSnapshot = CopyDBSnapshot'
  { -- | A value that indicates whether to copy the DB option group associated
    -- with the source DB snapshot to the target Amazon Web Services account
    -- and associate with the target DB snapshot. The associated option group
    -- can be copied only with cross-account snapshot copy calls.
    copyOptionGroup :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether to copy all tags from the source DB
    -- snapshot to the target DB snapshot. By default, tags aren\'t copied.
    copyTags :: Prelude.Maybe Prelude.Bool,
    -- | Pseudo-parameter used when populating the @PreSignedUrl@ of a
    -- cross-region @CopyDBSnapshot@ request. To replicate from region @SRC@ to
    -- region @DST@, send a request to region @DST@. In that request, pass a
    -- @PreSignedUrl@ for region @SRC@ with @DestinationRegion@ set to region
    -- @DST@.
    destinationRegion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services KMS key identifier for an encrypted DB snapshot.
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the KMS key.
    --
    -- If you copy an encrypted DB snapshot from your Amazon Web Services
    -- account, you can specify a value for this parameter to encrypt the copy
    -- with a new KMS key. If you don\'t specify a value for this parameter,
    -- then the copy of the DB snapshot is encrypted with the same Amazon Web
    -- Services KMS key as the source DB snapshot.
    --
    -- If you copy an encrypted DB snapshot that is shared from another Amazon
    -- Web Services account, then you must specify a value for this parameter.
    --
    -- If you specify this parameter when you copy an unencrypted snapshot, the
    -- copy is encrypted.
    --
    -- If you copy an encrypted snapshot to a different Amazon Web Services
    -- Region, then you must specify an Amazon Web Services KMS key identifier
    -- for the destination Amazon Web Services Region. KMS keys are specific to
    -- the Amazon Web Services Region that they are created in, and you can\'t
    -- use KMS keys from one Amazon Web Services Region in another Amazon Web
    -- Services Region.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The name of an option group to associate with the copy of the snapshot.
    --
    -- Specify this option if you are copying a snapshot from one Amazon Web
    -- Services Region to another, and your DB instance uses a nondefault
    -- option group. If your source DB instance uses Transparent Data
    -- Encryption for Oracle or Microsoft SQL Server, you must specify this
    -- option when copying across Amazon Web Services Regions. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_CopySnapshot.html#USER_CopySnapshot.Options Option group considerations>
    -- in the /Amazon RDS User Guide/.
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | When you are copying a snapshot from one Amazon Web Services GovCloud
    -- (US) Region to another, the URL that contains a Signature Version 4
    -- signed request for the @CopyDBSnapshot@ API operation in the source
    -- Amazon Web Services Region that contains the source DB snapshot to copy.
    --
    -- This setting applies only to Amazon Web Services GovCloud (US) Regions.
    -- It\'s ignored in other Amazon Web Services Regions.
    --
    -- You must specify this parameter when you copy an encrypted DB snapshot
    -- from another Amazon Web Services Region by using the Amazon RDS API.
    -- Don\'t specify @PreSignedUrl@ when you are copying an encrypted DB
    -- snapshot in the same Amazon Web Services Region.
    --
    -- The presigned URL must be a valid request for the
    -- @CopyDBClusterSnapshot@ API operation that can run in the source Amazon
    -- Web Services Region that contains the encrypted DB cluster snapshot to
    -- copy. The presigned URL request must contain the following parameter
    -- values:
    --
    -- -   @DestinationRegion@ - The Amazon Web Services Region that the
    --     encrypted DB snapshot is copied to. This Amazon Web Services Region
    --     is the same one where the @CopyDBSnapshot@ operation is called that
    --     contains this presigned URL.
    --
    --     For example, if you copy an encrypted DB snapshot from the us-west-2
    --     Amazon Web Services Region to the us-east-1 Amazon Web Services
    --     Region, then you call the @CopyDBSnapshot@ operation in the
    --     us-east-1 Amazon Web Services Region and provide a presigned URL
    --     that contains a call to the @CopyDBSnapshot@ operation in the
    --     us-west-2 Amazon Web Services Region. For this example, the
    --     @DestinationRegion@ in the presigned URL must be set to the
    --     us-east-1 Amazon Web Services Region.
    --
    -- -   @KmsKeyId@ - The KMS key identifier for the KMS key to use to
    --     encrypt the copy of the DB snapshot in the destination Amazon Web
    --     Services Region. This is the same identifier for both the
    --     @CopyDBSnapshot@ operation that is called in the destination Amazon
    --     Web Services Region, and the operation contained in the presigned
    --     URL.
    --
    -- -   @SourceDBSnapshotIdentifier@ - The DB snapshot identifier for the
    --     encrypted snapshot to be copied. This identifier must be in the
    --     Amazon Resource Name (ARN) format for the source Amazon Web Services
    --     Region. For example, if you are copying an encrypted DB snapshot
    --     from the us-west-2 Amazon Web Services Region, then your
    --     @SourceDBSnapshotIdentifier@ looks like the following example:
    --     @arn:aws:rds:us-west-2:123456789012:snapshot:mysql-instance1-snapshot-20161115@.
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
    -- | The external custom Availability Zone (CAZ) identifier for the target
    -- CAZ.
    --
    -- Example: @rds-caz-aiqhTgQv@.
    targetCustomAvailabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the source DB snapshot.
    --
    -- If the source snapshot is in the same Amazon Web Services Region as the
    -- copy, specify a valid DB snapshot identifier. For example, you might
    -- specify @rds:mysql-instance1-snapshot-20130805@.
    --
    -- If the source snapshot is in a different Amazon Web Services Region than
    -- the copy, specify a valid DB snapshot ARN. For example, you might
    -- specify
    -- @arn:aws:rds:us-west-2:123456789012:snapshot:mysql-instance1-snapshot-20130805@.
    --
    -- If you are copying from a shared manual DB snapshot, this parameter must
    -- be the Amazon Resource Name (ARN) of the shared DB snapshot.
    --
    -- If you are copying an encrypted snapshot this parameter must be in the
    -- ARN format for the source Amazon Web Services Region.
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
-- 'copyOptionGroup', 'copyDBSnapshot_copyOptionGroup' - A value that indicates whether to copy the DB option group associated
-- with the source DB snapshot to the target Amazon Web Services account
-- and associate with the target DB snapshot. The associated option group
-- can be copied only with cross-account snapshot copy calls.
--
-- 'copyTags', 'copyDBSnapshot_copyTags' - A value that indicates whether to copy all tags from the source DB
-- snapshot to the target DB snapshot. By default, tags aren\'t copied.
--
-- 'destinationRegion', 'copyDBSnapshot_destinationRegion' - Pseudo-parameter used when populating the @PreSignedUrl@ of a
-- cross-region @CopyDBSnapshot@ request. To replicate from region @SRC@ to
-- region @DST@, send a request to region @DST@. In that request, pass a
-- @PreSignedUrl@ for region @SRC@ with @DestinationRegion@ set to region
-- @DST@.
--
-- 'kmsKeyId', 'copyDBSnapshot_kmsKeyId' - The Amazon Web Services KMS key identifier for an encrypted DB snapshot.
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key.
--
-- If you copy an encrypted DB snapshot from your Amazon Web Services
-- account, you can specify a value for this parameter to encrypt the copy
-- with a new KMS key. If you don\'t specify a value for this parameter,
-- then the copy of the DB snapshot is encrypted with the same Amazon Web
-- Services KMS key as the source DB snapshot.
--
-- If you copy an encrypted DB snapshot that is shared from another Amazon
-- Web Services account, then you must specify a value for this parameter.
--
-- If you specify this parameter when you copy an unencrypted snapshot, the
-- copy is encrypted.
--
-- If you copy an encrypted snapshot to a different Amazon Web Services
-- Region, then you must specify an Amazon Web Services KMS key identifier
-- for the destination Amazon Web Services Region. KMS keys are specific to
-- the Amazon Web Services Region that they are created in, and you can\'t
-- use KMS keys from one Amazon Web Services Region in another Amazon Web
-- Services Region.
--
-- 'optionGroupName', 'copyDBSnapshot_optionGroupName' - The name of an option group to associate with the copy of the snapshot.
--
-- Specify this option if you are copying a snapshot from one Amazon Web
-- Services Region to another, and your DB instance uses a nondefault
-- option group. If your source DB instance uses Transparent Data
-- Encryption for Oracle or Microsoft SQL Server, you must specify this
-- option when copying across Amazon Web Services Regions. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_CopySnapshot.html#USER_CopySnapshot.Options Option group considerations>
-- in the /Amazon RDS User Guide/.
--
-- 'preSignedUrl', 'copyDBSnapshot_preSignedUrl' - When you are copying a snapshot from one Amazon Web Services GovCloud
-- (US) Region to another, the URL that contains a Signature Version 4
-- signed request for the @CopyDBSnapshot@ API operation in the source
-- Amazon Web Services Region that contains the source DB snapshot to copy.
--
-- This setting applies only to Amazon Web Services GovCloud (US) Regions.
-- It\'s ignored in other Amazon Web Services Regions.
--
-- You must specify this parameter when you copy an encrypted DB snapshot
-- from another Amazon Web Services Region by using the Amazon RDS API.
-- Don\'t specify @PreSignedUrl@ when you are copying an encrypted DB
-- snapshot in the same Amazon Web Services Region.
--
-- The presigned URL must be a valid request for the
-- @CopyDBClusterSnapshot@ API operation that can run in the source Amazon
-- Web Services Region that contains the encrypted DB cluster snapshot to
-- copy. The presigned URL request must contain the following parameter
-- values:
--
-- -   @DestinationRegion@ - The Amazon Web Services Region that the
--     encrypted DB snapshot is copied to. This Amazon Web Services Region
--     is the same one where the @CopyDBSnapshot@ operation is called that
--     contains this presigned URL.
--
--     For example, if you copy an encrypted DB snapshot from the us-west-2
--     Amazon Web Services Region to the us-east-1 Amazon Web Services
--     Region, then you call the @CopyDBSnapshot@ operation in the
--     us-east-1 Amazon Web Services Region and provide a presigned URL
--     that contains a call to the @CopyDBSnapshot@ operation in the
--     us-west-2 Amazon Web Services Region. For this example, the
--     @DestinationRegion@ in the presigned URL must be set to the
--     us-east-1 Amazon Web Services Region.
--
-- -   @KmsKeyId@ - The KMS key identifier for the KMS key to use to
--     encrypt the copy of the DB snapshot in the destination Amazon Web
--     Services Region. This is the same identifier for both the
--     @CopyDBSnapshot@ operation that is called in the destination Amazon
--     Web Services Region, and the operation contained in the presigned
--     URL.
--
-- -   @SourceDBSnapshotIdentifier@ - The DB snapshot identifier for the
--     encrypted snapshot to be copied. This identifier must be in the
--     Amazon Resource Name (ARN) format for the source Amazon Web Services
--     Region. For example, if you are copying an encrypted DB snapshot
--     from the us-west-2 Amazon Web Services Region, then your
--     @SourceDBSnapshotIdentifier@ looks like the following example:
--     @arn:aws:rds:us-west-2:123456789012:snapshot:mysql-instance1-snapshot-20161115@.
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
-- 'tags', 'copyDBSnapshot_tags' - Undocumented member.
--
-- 'targetCustomAvailabilityZone', 'copyDBSnapshot_targetCustomAvailabilityZone' - The external custom Availability Zone (CAZ) identifier for the target
-- CAZ.
--
-- Example: @rds-caz-aiqhTgQv@.
--
-- 'sourceDBSnapshotIdentifier', 'copyDBSnapshot_sourceDBSnapshotIdentifier' - The identifier for the source DB snapshot.
--
-- If the source snapshot is in the same Amazon Web Services Region as the
-- copy, specify a valid DB snapshot identifier. For example, you might
-- specify @rds:mysql-instance1-snapshot-20130805@.
--
-- If the source snapshot is in a different Amazon Web Services Region than
-- the copy, specify a valid DB snapshot ARN. For example, you might
-- specify
-- @arn:aws:rds:us-west-2:123456789012:snapshot:mysql-instance1-snapshot-20130805@.
--
-- If you are copying from a shared manual DB snapshot, this parameter must
-- be the Amazon Resource Name (ARN) of the shared DB snapshot.
--
-- If you are copying an encrypted snapshot this parameter must be in the
-- ARN format for the source Amazon Web Services Region.
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
      { copyOptionGroup = Prelude.Nothing,
        copyTags = Prelude.Nothing,
        destinationRegion = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        optionGroupName = Prelude.Nothing,
        preSignedUrl = Prelude.Nothing,
        tags = Prelude.Nothing,
        targetCustomAvailabilityZone = Prelude.Nothing,
        sourceDBSnapshotIdentifier =
          pSourceDBSnapshotIdentifier_,
        targetDBSnapshotIdentifier =
          pTargetDBSnapshotIdentifier_
      }

-- | A value that indicates whether to copy the DB option group associated
-- with the source DB snapshot to the target Amazon Web Services account
-- and associate with the target DB snapshot. The associated option group
-- can be copied only with cross-account snapshot copy calls.
copyDBSnapshot_copyOptionGroup :: Lens.Lens' CopyDBSnapshot (Prelude.Maybe Prelude.Bool)
copyDBSnapshot_copyOptionGroup = Lens.lens (\CopyDBSnapshot' {copyOptionGroup} -> copyOptionGroup) (\s@CopyDBSnapshot' {} a -> s {copyOptionGroup = a} :: CopyDBSnapshot)

-- | A value that indicates whether to copy all tags from the source DB
-- snapshot to the target DB snapshot. By default, tags aren\'t copied.
copyDBSnapshot_copyTags :: Lens.Lens' CopyDBSnapshot (Prelude.Maybe Prelude.Bool)
copyDBSnapshot_copyTags = Lens.lens (\CopyDBSnapshot' {copyTags} -> copyTags) (\s@CopyDBSnapshot' {} a -> s {copyTags = a} :: CopyDBSnapshot)

-- | Pseudo-parameter used when populating the @PreSignedUrl@ of a
-- cross-region @CopyDBSnapshot@ request. To replicate from region @SRC@ to
-- region @DST@, send a request to region @DST@. In that request, pass a
-- @PreSignedUrl@ for region @SRC@ with @DestinationRegion@ set to region
-- @DST@.
copyDBSnapshot_destinationRegion :: Lens.Lens' CopyDBSnapshot (Prelude.Maybe Prelude.Text)
copyDBSnapshot_destinationRegion = Lens.lens (\CopyDBSnapshot' {destinationRegion} -> destinationRegion) (\s@CopyDBSnapshot' {} a -> s {destinationRegion = a} :: CopyDBSnapshot)

-- | The Amazon Web Services KMS key identifier for an encrypted DB snapshot.
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key.
--
-- If you copy an encrypted DB snapshot from your Amazon Web Services
-- account, you can specify a value for this parameter to encrypt the copy
-- with a new KMS key. If you don\'t specify a value for this parameter,
-- then the copy of the DB snapshot is encrypted with the same Amazon Web
-- Services KMS key as the source DB snapshot.
--
-- If you copy an encrypted DB snapshot that is shared from another Amazon
-- Web Services account, then you must specify a value for this parameter.
--
-- If you specify this parameter when you copy an unencrypted snapshot, the
-- copy is encrypted.
--
-- If you copy an encrypted snapshot to a different Amazon Web Services
-- Region, then you must specify an Amazon Web Services KMS key identifier
-- for the destination Amazon Web Services Region. KMS keys are specific to
-- the Amazon Web Services Region that they are created in, and you can\'t
-- use KMS keys from one Amazon Web Services Region in another Amazon Web
-- Services Region.
copyDBSnapshot_kmsKeyId :: Lens.Lens' CopyDBSnapshot (Prelude.Maybe Prelude.Text)
copyDBSnapshot_kmsKeyId = Lens.lens (\CopyDBSnapshot' {kmsKeyId} -> kmsKeyId) (\s@CopyDBSnapshot' {} a -> s {kmsKeyId = a} :: CopyDBSnapshot)

-- | The name of an option group to associate with the copy of the snapshot.
--
-- Specify this option if you are copying a snapshot from one Amazon Web
-- Services Region to another, and your DB instance uses a nondefault
-- option group. If your source DB instance uses Transparent Data
-- Encryption for Oracle or Microsoft SQL Server, you must specify this
-- option when copying across Amazon Web Services Regions. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_CopySnapshot.html#USER_CopySnapshot.Options Option group considerations>
-- in the /Amazon RDS User Guide/.
copyDBSnapshot_optionGroupName :: Lens.Lens' CopyDBSnapshot (Prelude.Maybe Prelude.Text)
copyDBSnapshot_optionGroupName = Lens.lens (\CopyDBSnapshot' {optionGroupName} -> optionGroupName) (\s@CopyDBSnapshot' {} a -> s {optionGroupName = a} :: CopyDBSnapshot)

-- | When you are copying a snapshot from one Amazon Web Services GovCloud
-- (US) Region to another, the URL that contains a Signature Version 4
-- signed request for the @CopyDBSnapshot@ API operation in the source
-- Amazon Web Services Region that contains the source DB snapshot to copy.
--
-- This setting applies only to Amazon Web Services GovCloud (US) Regions.
-- It\'s ignored in other Amazon Web Services Regions.
--
-- You must specify this parameter when you copy an encrypted DB snapshot
-- from another Amazon Web Services Region by using the Amazon RDS API.
-- Don\'t specify @PreSignedUrl@ when you are copying an encrypted DB
-- snapshot in the same Amazon Web Services Region.
--
-- The presigned URL must be a valid request for the
-- @CopyDBClusterSnapshot@ API operation that can run in the source Amazon
-- Web Services Region that contains the encrypted DB cluster snapshot to
-- copy. The presigned URL request must contain the following parameter
-- values:
--
-- -   @DestinationRegion@ - The Amazon Web Services Region that the
--     encrypted DB snapshot is copied to. This Amazon Web Services Region
--     is the same one where the @CopyDBSnapshot@ operation is called that
--     contains this presigned URL.
--
--     For example, if you copy an encrypted DB snapshot from the us-west-2
--     Amazon Web Services Region to the us-east-1 Amazon Web Services
--     Region, then you call the @CopyDBSnapshot@ operation in the
--     us-east-1 Amazon Web Services Region and provide a presigned URL
--     that contains a call to the @CopyDBSnapshot@ operation in the
--     us-west-2 Amazon Web Services Region. For this example, the
--     @DestinationRegion@ in the presigned URL must be set to the
--     us-east-1 Amazon Web Services Region.
--
-- -   @KmsKeyId@ - The KMS key identifier for the KMS key to use to
--     encrypt the copy of the DB snapshot in the destination Amazon Web
--     Services Region. This is the same identifier for both the
--     @CopyDBSnapshot@ operation that is called in the destination Amazon
--     Web Services Region, and the operation contained in the presigned
--     URL.
--
-- -   @SourceDBSnapshotIdentifier@ - The DB snapshot identifier for the
--     encrypted snapshot to be copied. This identifier must be in the
--     Amazon Resource Name (ARN) format for the source Amazon Web Services
--     Region. For example, if you are copying an encrypted DB snapshot
--     from the us-west-2 Amazon Web Services Region, then your
--     @SourceDBSnapshotIdentifier@ looks like the following example:
--     @arn:aws:rds:us-west-2:123456789012:snapshot:mysql-instance1-snapshot-20161115@.
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
copyDBSnapshot_preSignedUrl :: Lens.Lens' CopyDBSnapshot (Prelude.Maybe Prelude.Text)
copyDBSnapshot_preSignedUrl = Lens.lens (\CopyDBSnapshot' {preSignedUrl} -> preSignedUrl) (\s@CopyDBSnapshot' {} a -> s {preSignedUrl = a} :: CopyDBSnapshot)

-- | Undocumented member.
copyDBSnapshot_tags :: Lens.Lens' CopyDBSnapshot (Prelude.Maybe [Tag])
copyDBSnapshot_tags = Lens.lens (\CopyDBSnapshot' {tags} -> tags) (\s@CopyDBSnapshot' {} a -> s {tags = a} :: CopyDBSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The external custom Availability Zone (CAZ) identifier for the target
-- CAZ.
--
-- Example: @rds-caz-aiqhTgQv@.
copyDBSnapshot_targetCustomAvailabilityZone :: Lens.Lens' CopyDBSnapshot (Prelude.Maybe Prelude.Text)
copyDBSnapshot_targetCustomAvailabilityZone = Lens.lens (\CopyDBSnapshot' {targetCustomAvailabilityZone} -> targetCustomAvailabilityZone) (\s@CopyDBSnapshot' {} a -> s {targetCustomAvailabilityZone = a} :: CopyDBSnapshot)

-- | The identifier for the source DB snapshot.
--
-- If the source snapshot is in the same Amazon Web Services Region as the
-- copy, specify a valid DB snapshot identifier. For example, you might
-- specify @rds:mysql-instance1-snapshot-20130805@.
--
-- If the source snapshot is in a different Amazon Web Services Region than
-- the copy, specify a valid DB snapshot ARN. For example, you might
-- specify
-- @arn:aws:rds:us-west-2:123456789012:snapshot:mysql-instance1-snapshot-20130805@.
--
-- If you are copying from a shared manual DB snapshot, this parameter must
-- be the Amazon Resource Name (ARN) of the shared DB snapshot.
--
-- If you are copying an encrypted snapshot this parameter must be in the
-- ARN format for the source Amazon Web Services Region.
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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CopyDBSnapshotResult"
      ( \s h x ->
          CopyDBSnapshotResponse'
            Prelude.<$> (x Data..@? "DBSnapshot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CopyDBSnapshot where
  hashWithSalt _salt CopyDBSnapshot' {..} =
    _salt
      `Prelude.hashWithSalt` copyOptionGroup
      `Prelude.hashWithSalt` copyTags
      `Prelude.hashWithSalt` destinationRegion
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` optionGroupName
      `Prelude.hashWithSalt` preSignedUrl
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` targetCustomAvailabilityZone
      `Prelude.hashWithSalt` sourceDBSnapshotIdentifier
      `Prelude.hashWithSalt` targetDBSnapshotIdentifier

instance Prelude.NFData CopyDBSnapshot where
  rnf CopyDBSnapshot' {..} =
    Prelude.rnf copyOptionGroup
      `Prelude.seq` Prelude.rnf copyTags
      `Prelude.seq` Prelude.rnf destinationRegion
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf optionGroupName
      `Prelude.seq` Prelude.rnf preSignedUrl
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf targetCustomAvailabilityZone
      `Prelude.seq` Prelude.rnf sourceDBSnapshotIdentifier
      `Prelude.seq` Prelude.rnf targetDBSnapshotIdentifier

instance Data.ToHeaders CopyDBSnapshot where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CopyDBSnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery CopyDBSnapshot where
  toQuery CopyDBSnapshot' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CopyDBSnapshot" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "CopyOptionGroup" Data.=: copyOptionGroup,
        "CopyTags" Data.=: copyTags,
        "DestinationRegion" Data.=: destinationRegion,
        "KmsKeyId" Data.=: kmsKeyId,
        "OptionGroupName" Data.=: optionGroupName,
        "PreSignedUrl" Data.=: preSignedUrl,
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "Tag" Prelude.<$> tags),
        "TargetCustomAvailabilityZone"
          Data.=: targetCustomAvailabilityZone,
        "SourceDBSnapshotIdentifier"
          Data.=: sourceDBSnapshotIdentifier,
        "TargetDBSnapshotIdentifier"
          Data.=: targetDBSnapshotIdentifier
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

instance Prelude.NFData CopyDBSnapshotResponse where
  rnf CopyDBSnapshotResponse' {..} =
    Prelude.rnf dbSnapshot
      `Prelude.seq` Prelude.rnf httpStatus
