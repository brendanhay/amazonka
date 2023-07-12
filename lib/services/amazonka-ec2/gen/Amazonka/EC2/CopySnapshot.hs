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
-- Module      : Amazonka.EC2.CopySnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies a point-in-time snapshot of an EBS volume and stores it in Amazon
-- S3. You can copy a snapshot within the same Region, from one Region to
-- another, or from a Region to an Outpost. You can\'t copy a snapshot from
-- an Outpost to a Region, from one Outpost to another, or within the same
-- Outpost.
--
-- You can use the snapshot to create EBS volumes or Amazon Machine Images
-- (AMIs).
--
-- When copying snapshots to a Region, copies of encrypted EBS snapshots
-- remain encrypted. Copies of unencrypted snapshots remain unencrypted,
-- unless you enable encryption for the snapshot copy operation. By
-- default, encrypted snapshot copies use the default Key Management
-- Service (KMS) KMS key; however, you can specify a different KMS key. To
-- copy an encrypted snapshot that has been shared from another account,
-- you must have permissions for the KMS key used to encrypt the snapshot.
--
-- Snapshots copied to an Outpost are encrypted by default using the
-- default encryption key for the Region, or a different key that you
-- specify in the request using __KmsKeyId__. Outposts do not support
-- unencrypted snapshots. For more information,
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html#ami Amazon EBS local snapshots on Outposts>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- Snapshots created by copying another snapshot have an arbitrary volume
-- ID that should not be used for any purpose.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-copy-snapshot.html Copy an Amazon EBS snapshot>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.EC2.CopySnapshot
  ( -- * Creating a Request
    CopySnapshot (..),
    newCopySnapshot,

    -- * Request Lenses
    copySnapshot_description,
    copySnapshot_destinationOutpostArn,
    copySnapshot_destinationRegion,
    copySnapshot_dryRun,
    copySnapshot_encrypted,
    copySnapshot_kmsKeyId,
    copySnapshot_presignedUrl,
    copySnapshot_tagSpecifications,
    copySnapshot_sourceRegion,
    copySnapshot_sourceSnapshotId,

    -- * Destructuring the Response
    CopySnapshotResponse (..),
    newCopySnapshotResponse,

    -- * Response Lenses
    copySnapshotResponse_snapshotId,
    copySnapshotResponse_tags,
    copySnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCopySnapshot' smart constructor.
data CopySnapshot = CopySnapshot'
  { -- | A description for the EBS snapshot.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Outpost to which to copy the
    -- snapshot. Only specify this parameter when copying a snapshot from an
    -- Amazon Web Services Region to an Outpost. The snapshot must be in the
    -- Region for the destination Outpost. You cannot copy a snapshot from an
    -- Outpost to a Region, from one Outpost to another, or within the same
    -- Outpost.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html#copy-snapshots Copy snapshots from an Amazon Web Services Region to an Outpost>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    destinationOutpostArn :: Prelude.Maybe Prelude.Text,
    -- | The destination Region to use in the @PresignedUrl@ parameter of a
    -- snapshot copy operation. This parameter is only valid for specifying the
    -- destination Region in a @PresignedUrl@ parameter, where it is required.
    --
    -- The snapshot copy is sent to the regional endpoint that you sent the
    -- HTTP request to (for example, @ec2.us-east-1.amazonaws.com@). With the
    -- CLI, this is specified using the @--region@ parameter or the default
    -- Region in your Amazon Web Services configuration file.
    destinationRegion :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | To encrypt a copy of an unencrypted snapshot if encryption by default is
    -- not enabled, enable encryption using this parameter. Otherwise, omit
    -- this parameter. Encrypted snapshots are encrypted, even if you omit this
    -- parameter and encryption by default is not enabled. You cannot set this
    -- parameter to false. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS encryption>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of the Key Management Service (KMS) KMS key to use for
    -- Amazon EBS encryption. If this parameter is not specified, your KMS key
    -- for Amazon EBS is used. If @KmsKeyId@ is specified, the encrypted state
    -- must be @true@.
    --
    -- You can specify the KMS key using any of the following:
    --
    -- -   Key ID. For example, 1234abcd-12ab-34cd-56ef-1234567890ab.
    --
    -- -   Key alias. For example, alias\/ExampleAlias.
    --
    -- -   Key ARN. For example,
    --     arn:aws:kms:us-east-1:012345678910:key\/1234abcd-12ab-34cd-56ef-1234567890ab.
    --
    -- -   Alias ARN. For example,
    --     arn:aws:kms:us-east-1:012345678910:alias\/ExampleAlias.
    --
    -- Amazon Web Services authenticates the KMS key asynchronously. Therefore,
    -- if you specify an ID, alias, or ARN that is not valid, the action can
    -- appear to complete, but eventually fails.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | When you copy an encrypted source snapshot using the Amazon EC2 Query
    -- API, you must supply a pre-signed URL. This parameter is optional for
    -- unencrypted snapshots. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html Query requests>.
    --
    -- The @PresignedUrl@ should use the snapshot source endpoint, the
    -- @CopySnapshot@ action, and include the @SourceRegion@,
    -- @SourceSnapshotId@, and @DestinationRegion@ parameters. The
    -- @PresignedUrl@ must be signed using Amazon Web Services Signature
    -- Version 4. Because EBS snapshots are stored in Amazon S3, the signing
    -- algorithm for this parameter uses the same logic that is described in
    -- <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (Amazon Web Services Signature Version 4)>
    -- in the /Amazon Simple Storage Service API Reference/. An invalid or
    -- improperly signed @PresignedUrl@ will cause the copy operation to fail
    -- asynchronously, and the snapshot will move to an @error@ state.
    presignedUrl :: Prelude.Maybe Prelude.Text,
    -- | The tags to apply to the new snapshot.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The ID of the Region that contains the snapshot to be copied.
    sourceRegion :: Prelude.Text,
    -- | The ID of the EBS snapshot to copy.
    sourceSnapshotId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopySnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'copySnapshot_description' - A description for the EBS snapshot.
--
-- 'destinationOutpostArn', 'copySnapshot_destinationOutpostArn' - The Amazon Resource Name (ARN) of the Outpost to which to copy the
-- snapshot. Only specify this parameter when copying a snapshot from an
-- Amazon Web Services Region to an Outpost. The snapshot must be in the
-- Region for the destination Outpost. You cannot copy a snapshot from an
-- Outpost to a Region, from one Outpost to another, or within the same
-- Outpost.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html#copy-snapshots Copy snapshots from an Amazon Web Services Region to an Outpost>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'destinationRegion', 'copySnapshot_destinationRegion' - The destination Region to use in the @PresignedUrl@ parameter of a
-- snapshot copy operation. This parameter is only valid for specifying the
-- destination Region in a @PresignedUrl@ parameter, where it is required.
--
-- The snapshot copy is sent to the regional endpoint that you sent the
-- HTTP request to (for example, @ec2.us-east-1.amazonaws.com@). With the
-- CLI, this is specified using the @--region@ parameter or the default
-- Region in your Amazon Web Services configuration file.
--
-- 'dryRun', 'copySnapshot_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'encrypted', 'copySnapshot_encrypted' - To encrypt a copy of an unencrypted snapshot if encryption by default is
-- not enabled, enable encryption using this parameter. Otherwise, omit
-- this parameter. Encrypted snapshots are encrypted, even if you omit this
-- parameter and encryption by default is not enabled. You cannot set this
-- parameter to false. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'kmsKeyId', 'copySnapshot_kmsKeyId' - The identifier of the Key Management Service (KMS) KMS key to use for
-- Amazon EBS encryption. If this parameter is not specified, your KMS key
-- for Amazon EBS is used. If @KmsKeyId@ is specified, the encrypted state
-- must be @true@.
--
-- You can specify the KMS key using any of the following:
--
-- -   Key ID. For example, 1234abcd-12ab-34cd-56ef-1234567890ab.
--
-- -   Key alias. For example, alias\/ExampleAlias.
--
-- -   Key ARN. For example,
--     arn:aws:kms:us-east-1:012345678910:key\/1234abcd-12ab-34cd-56ef-1234567890ab.
--
-- -   Alias ARN. For example,
--     arn:aws:kms:us-east-1:012345678910:alias\/ExampleAlias.
--
-- Amazon Web Services authenticates the KMS key asynchronously. Therefore,
-- if you specify an ID, alias, or ARN that is not valid, the action can
-- appear to complete, but eventually fails.
--
-- 'presignedUrl', 'copySnapshot_presignedUrl' - When you copy an encrypted source snapshot using the Amazon EC2 Query
-- API, you must supply a pre-signed URL. This parameter is optional for
-- unencrypted snapshots. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html Query requests>.
--
-- The @PresignedUrl@ should use the snapshot source endpoint, the
-- @CopySnapshot@ action, and include the @SourceRegion@,
-- @SourceSnapshotId@, and @DestinationRegion@ parameters. The
-- @PresignedUrl@ must be signed using Amazon Web Services Signature
-- Version 4. Because EBS snapshots are stored in Amazon S3, the signing
-- algorithm for this parameter uses the same logic that is described in
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (Amazon Web Services Signature Version 4)>
-- in the /Amazon Simple Storage Service API Reference/. An invalid or
-- improperly signed @PresignedUrl@ will cause the copy operation to fail
-- asynchronously, and the snapshot will move to an @error@ state.
--
-- 'tagSpecifications', 'copySnapshot_tagSpecifications' - The tags to apply to the new snapshot.
--
-- 'sourceRegion', 'copySnapshot_sourceRegion' - The ID of the Region that contains the snapshot to be copied.
--
-- 'sourceSnapshotId', 'copySnapshot_sourceSnapshotId' - The ID of the EBS snapshot to copy.
newCopySnapshot ::
  -- | 'sourceRegion'
  Prelude.Text ->
  -- | 'sourceSnapshotId'
  Prelude.Text ->
  CopySnapshot
newCopySnapshot pSourceRegion_ pSourceSnapshotId_ =
  CopySnapshot'
    { description = Prelude.Nothing,
      destinationOutpostArn = Prelude.Nothing,
      destinationRegion = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      presignedUrl = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      sourceRegion = pSourceRegion_,
      sourceSnapshotId = pSourceSnapshotId_
    }

-- | A description for the EBS snapshot.
copySnapshot_description :: Lens.Lens' CopySnapshot (Prelude.Maybe Prelude.Text)
copySnapshot_description = Lens.lens (\CopySnapshot' {description} -> description) (\s@CopySnapshot' {} a -> s {description = a} :: CopySnapshot)

-- | The Amazon Resource Name (ARN) of the Outpost to which to copy the
-- snapshot. Only specify this parameter when copying a snapshot from an
-- Amazon Web Services Region to an Outpost. The snapshot must be in the
-- Region for the destination Outpost. You cannot copy a snapshot from an
-- Outpost to a Region, from one Outpost to another, or within the same
-- Outpost.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html#copy-snapshots Copy snapshots from an Amazon Web Services Region to an Outpost>
-- in the /Amazon Elastic Compute Cloud User Guide/.
copySnapshot_destinationOutpostArn :: Lens.Lens' CopySnapshot (Prelude.Maybe Prelude.Text)
copySnapshot_destinationOutpostArn = Lens.lens (\CopySnapshot' {destinationOutpostArn} -> destinationOutpostArn) (\s@CopySnapshot' {} a -> s {destinationOutpostArn = a} :: CopySnapshot)

-- | The destination Region to use in the @PresignedUrl@ parameter of a
-- snapshot copy operation. This parameter is only valid for specifying the
-- destination Region in a @PresignedUrl@ parameter, where it is required.
--
-- The snapshot copy is sent to the regional endpoint that you sent the
-- HTTP request to (for example, @ec2.us-east-1.amazonaws.com@). With the
-- CLI, this is specified using the @--region@ parameter or the default
-- Region in your Amazon Web Services configuration file.
copySnapshot_destinationRegion :: Lens.Lens' CopySnapshot (Prelude.Maybe Prelude.Text)
copySnapshot_destinationRegion = Lens.lens (\CopySnapshot' {destinationRegion} -> destinationRegion) (\s@CopySnapshot' {} a -> s {destinationRegion = a} :: CopySnapshot)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
copySnapshot_dryRun :: Lens.Lens' CopySnapshot (Prelude.Maybe Prelude.Bool)
copySnapshot_dryRun = Lens.lens (\CopySnapshot' {dryRun} -> dryRun) (\s@CopySnapshot' {} a -> s {dryRun = a} :: CopySnapshot)

-- | To encrypt a copy of an unencrypted snapshot if encryption by default is
-- not enabled, enable encryption using this parameter. Otherwise, omit
-- this parameter. Encrypted snapshots are encrypted, even if you omit this
-- parameter and encryption by default is not enabled. You cannot set this
-- parameter to false. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
copySnapshot_encrypted :: Lens.Lens' CopySnapshot (Prelude.Maybe Prelude.Bool)
copySnapshot_encrypted = Lens.lens (\CopySnapshot' {encrypted} -> encrypted) (\s@CopySnapshot' {} a -> s {encrypted = a} :: CopySnapshot)

-- | The identifier of the Key Management Service (KMS) KMS key to use for
-- Amazon EBS encryption. If this parameter is not specified, your KMS key
-- for Amazon EBS is used. If @KmsKeyId@ is specified, the encrypted state
-- must be @true@.
--
-- You can specify the KMS key using any of the following:
--
-- -   Key ID. For example, 1234abcd-12ab-34cd-56ef-1234567890ab.
--
-- -   Key alias. For example, alias\/ExampleAlias.
--
-- -   Key ARN. For example,
--     arn:aws:kms:us-east-1:012345678910:key\/1234abcd-12ab-34cd-56ef-1234567890ab.
--
-- -   Alias ARN. For example,
--     arn:aws:kms:us-east-1:012345678910:alias\/ExampleAlias.
--
-- Amazon Web Services authenticates the KMS key asynchronously. Therefore,
-- if you specify an ID, alias, or ARN that is not valid, the action can
-- appear to complete, but eventually fails.
copySnapshot_kmsKeyId :: Lens.Lens' CopySnapshot (Prelude.Maybe Prelude.Text)
copySnapshot_kmsKeyId = Lens.lens (\CopySnapshot' {kmsKeyId} -> kmsKeyId) (\s@CopySnapshot' {} a -> s {kmsKeyId = a} :: CopySnapshot)

-- | When you copy an encrypted source snapshot using the Amazon EC2 Query
-- API, you must supply a pre-signed URL. This parameter is optional for
-- unencrypted snapshots. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html Query requests>.
--
-- The @PresignedUrl@ should use the snapshot source endpoint, the
-- @CopySnapshot@ action, and include the @SourceRegion@,
-- @SourceSnapshotId@, and @DestinationRegion@ parameters. The
-- @PresignedUrl@ must be signed using Amazon Web Services Signature
-- Version 4. Because EBS snapshots are stored in Amazon S3, the signing
-- algorithm for this parameter uses the same logic that is described in
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (Amazon Web Services Signature Version 4)>
-- in the /Amazon Simple Storage Service API Reference/. An invalid or
-- improperly signed @PresignedUrl@ will cause the copy operation to fail
-- asynchronously, and the snapshot will move to an @error@ state.
copySnapshot_presignedUrl :: Lens.Lens' CopySnapshot (Prelude.Maybe Prelude.Text)
copySnapshot_presignedUrl = Lens.lens (\CopySnapshot' {presignedUrl} -> presignedUrl) (\s@CopySnapshot' {} a -> s {presignedUrl = a} :: CopySnapshot)

-- | The tags to apply to the new snapshot.
copySnapshot_tagSpecifications :: Lens.Lens' CopySnapshot (Prelude.Maybe [TagSpecification])
copySnapshot_tagSpecifications = Lens.lens (\CopySnapshot' {tagSpecifications} -> tagSpecifications) (\s@CopySnapshot' {} a -> s {tagSpecifications = a} :: CopySnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Region that contains the snapshot to be copied.
copySnapshot_sourceRegion :: Lens.Lens' CopySnapshot Prelude.Text
copySnapshot_sourceRegion = Lens.lens (\CopySnapshot' {sourceRegion} -> sourceRegion) (\s@CopySnapshot' {} a -> s {sourceRegion = a} :: CopySnapshot)

-- | The ID of the EBS snapshot to copy.
copySnapshot_sourceSnapshotId :: Lens.Lens' CopySnapshot Prelude.Text
copySnapshot_sourceSnapshotId = Lens.lens (\CopySnapshot' {sourceSnapshotId} -> sourceSnapshotId) (\s@CopySnapshot' {} a -> s {sourceSnapshotId = a} :: CopySnapshot)

instance Core.AWSRequest CopySnapshot where
  type AWSResponse CopySnapshot = CopySnapshotResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CopySnapshotResponse'
            Prelude.<$> (x Data..@? "snapshotId")
            Prelude.<*> ( x
                            Data..@? "tagSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CopySnapshot where
  hashWithSalt _salt CopySnapshot' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` destinationOutpostArn
      `Prelude.hashWithSalt` destinationRegion
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` presignedUrl
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` sourceRegion
      `Prelude.hashWithSalt` sourceSnapshotId

instance Prelude.NFData CopySnapshot where
  rnf CopySnapshot' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf destinationOutpostArn
      `Prelude.seq` Prelude.rnf destinationRegion
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf presignedUrl
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf sourceRegion
      `Prelude.seq` Prelude.rnf sourceSnapshotId

instance Data.ToHeaders CopySnapshot where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CopySnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery CopySnapshot where
  toQuery CopySnapshot' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CopySnapshot" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "Description" Data.=: description,
        "DestinationOutpostArn"
          Data.=: destinationOutpostArn,
        "DestinationRegion" Data.=: destinationRegion,
        "DryRun" Data.=: dryRun,
        "Encrypted" Data.=: encrypted,
        "KmsKeyId" Data.=: kmsKeyId,
        "PresignedUrl" Data.=: presignedUrl,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "SourceRegion" Data.=: sourceRegion,
        "SourceSnapshotId" Data.=: sourceSnapshotId
      ]

-- | /See:/ 'newCopySnapshotResponse' smart constructor.
data CopySnapshotResponse = CopySnapshotResponse'
  { -- | The ID of the new snapshot.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | Any tags applied to the new snapshot.
    tags :: Prelude.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopySnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshotId', 'copySnapshotResponse_snapshotId' - The ID of the new snapshot.
--
-- 'tags', 'copySnapshotResponse_tags' - Any tags applied to the new snapshot.
--
-- 'httpStatus', 'copySnapshotResponse_httpStatus' - The response's http status code.
newCopySnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CopySnapshotResponse
newCopySnapshotResponse pHttpStatus_ =
  CopySnapshotResponse'
    { snapshotId = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the new snapshot.
copySnapshotResponse_snapshotId :: Lens.Lens' CopySnapshotResponse (Prelude.Maybe Prelude.Text)
copySnapshotResponse_snapshotId = Lens.lens (\CopySnapshotResponse' {snapshotId} -> snapshotId) (\s@CopySnapshotResponse' {} a -> s {snapshotId = a} :: CopySnapshotResponse)

-- | Any tags applied to the new snapshot.
copySnapshotResponse_tags :: Lens.Lens' CopySnapshotResponse (Prelude.Maybe [Tag])
copySnapshotResponse_tags = Lens.lens (\CopySnapshotResponse' {tags} -> tags) (\s@CopySnapshotResponse' {} a -> s {tags = a} :: CopySnapshotResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
copySnapshotResponse_httpStatus :: Lens.Lens' CopySnapshotResponse Prelude.Int
copySnapshotResponse_httpStatus = Lens.lens (\CopySnapshotResponse' {httpStatus} -> httpStatus) (\s@CopySnapshotResponse' {} a -> s {httpStatus = a} :: CopySnapshotResponse)

instance Prelude.NFData CopySnapshotResponse where
  rnf CopySnapshotResponse' {..} =
    Prelude.rnf snapshotId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
