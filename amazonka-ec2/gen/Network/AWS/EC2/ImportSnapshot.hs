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
-- Module      : Network.AWS.EC2.ImportSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports a disk into an EBS snapshot.
module Network.AWS.EC2.ImportSnapshot
  ( -- * Creating a Request
    ImportSnapshot (..),
    newImportSnapshot,

    -- * Request Lenses
    importSnapshot_tagSpecifications,
    importSnapshot_dryRun,
    importSnapshot_encrypted,
    importSnapshot_roleName,
    importSnapshot_kmsKeyId,
    importSnapshot_clientData,
    importSnapshot_description,
    importSnapshot_clientToken,
    importSnapshot_diskContainer,

    -- * Destructuring the Response
    ImportSnapshotResponse (..),
    newImportSnapshotResponse,

    -- * Response Lenses
    importSnapshotResponse_snapshotTaskDetail,
    importSnapshotResponse_importTaskId,
    importSnapshotResponse_tags,
    importSnapshotResponse_description,
    importSnapshotResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newImportSnapshot' smart constructor.
data ImportSnapshot = ImportSnapshot'
  { -- | The tags to apply to the import snapshot task during creation.
    tagSpecifications :: Core.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | Specifies whether the destination snapshot of the imported image should
    -- be encrypted. The default CMK for EBS is used unless you specify a
    -- non-default AWS Key Management Service (AWS KMS) CMK using @KmsKeyId@.
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    encrypted :: Core.Maybe Core.Bool,
    -- | The name of the role to use when not using the default role,
    -- \'vmimport\'.
    roleName :: Core.Maybe Core.Text,
    -- | An identifier for the symmetric AWS Key Management Service (AWS KMS)
    -- customer master key (CMK) to use when creating the encrypted snapshot.
    -- This parameter is only required if you want to use a non-default CMK; if
    -- this parameter is not specified, the default CMK for EBS is used. If a
    -- @KmsKeyId@ is specified, the @Encrypted@ flag must also be set.
    --
    -- The CMK identifier may be provided in any of the following formats:
    --
    -- -   Key ID
    --
    -- -   Key alias. The alias ARN contains the @arn:aws:kms@ namespace,
    --     followed by the Region of the CMK, the AWS account ID of the CMK
    --     owner, the @alias@ namespace, and then the CMK alias. For example,
    --     arn:aws:kms:/us-east-1/:/012345678910/:alias\//ExampleAlias/.
    --
    -- -   ARN using key ID. The ID ARN contains the @arn:aws:kms@ namespace,
    --     followed by the Region of the CMK, the AWS account ID of the CMK
    --     owner, the @key@ namespace, and then the CMK ID. For example,
    --     arn:aws:kms:/us-east-1/:/012345678910/:key\//abcd1234-a123-456a-a12b-a123b4cd56ef/.
    --
    -- -   ARN using key alias. The alias ARN contains the @arn:aws:kms@
    --     namespace, followed by the Region of the CMK, the AWS account ID of
    --     the CMK owner, the @alias@ namespace, and then the CMK alias. For
    --     example,
    --     arn:aws:kms:/us-east-1/:/012345678910/:alias\//ExampleAlias/.
    --
    -- AWS parses @KmsKeyId@ asynchronously, meaning that the action you call
    -- may appear to complete even though you provided an invalid identifier.
    -- This action will eventually report failure.
    --
    -- The specified CMK must exist in the Region that the snapshot is being
    -- copied to.
    --
    -- Amazon EBS does not support asymmetric CMKs.
    kmsKeyId :: Core.Maybe Core.Text,
    -- | The client-specific data.
    clientData :: Core.Maybe ClientData,
    -- | The description string for the import snapshot task.
    description :: Core.Maybe Core.Text,
    -- | Token to enable idempotency for VM import requests.
    clientToken :: Core.Maybe Core.Text,
    -- | Information about the disk container.
    diskContainer :: Core.Maybe SnapshotDiskContainer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ImportSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSpecifications', 'importSnapshot_tagSpecifications' - The tags to apply to the import snapshot task during creation.
--
-- 'dryRun', 'importSnapshot_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'encrypted', 'importSnapshot_encrypted' - Specifies whether the destination snapshot of the imported image should
-- be encrypted. The default CMK for EBS is used unless you specify a
-- non-default AWS Key Management Service (AWS KMS) CMK using @KmsKeyId@.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'roleName', 'importSnapshot_roleName' - The name of the role to use when not using the default role,
-- \'vmimport\'.
--
-- 'kmsKeyId', 'importSnapshot_kmsKeyId' - An identifier for the symmetric AWS Key Management Service (AWS KMS)
-- customer master key (CMK) to use when creating the encrypted snapshot.
-- This parameter is only required if you want to use a non-default CMK; if
-- this parameter is not specified, the default CMK for EBS is used. If a
-- @KmsKeyId@ is specified, the @Encrypted@ flag must also be set.
--
-- The CMK identifier may be provided in any of the following formats:
--
-- -   Key ID
--
-- -   Key alias. The alias ARN contains the @arn:aws:kms@ namespace,
--     followed by the Region of the CMK, the AWS account ID of the CMK
--     owner, the @alias@ namespace, and then the CMK alias. For example,
--     arn:aws:kms:/us-east-1/:/012345678910/:alias\//ExampleAlias/.
--
-- -   ARN using key ID. The ID ARN contains the @arn:aws:kms@ namespace,
--     followed by the Region of the CMK, the AWS account ID of the CMK
--     owner, the @key@ namespace, and then the CMK ID. For example,
--     arn:aws:kms:/us-east-1/:/012345678910/:key\//abcd1234-a123-456a-a12b-a123b4cd56ef/.
--
-- -   ARN using key alias. The alias ARN contains the @arn:aws:kms@
--     namespace, followed by the Region of the CMK, the AWS account ID of
--     the CMK owner, the @alias@ namespace, and then the CMK alias. For
--     example,
--     arn:aws:kms:/us-east-1/:/012345678910/:alias\//ExampleAlias/.
--
-- AWS parses @KmsKeyId@ asynchronously, meaning that the action you call
-- may appear to complete even though you provided an invalid identifier.
-- This action will eventually report failure.
--
-- The specified CMK must exist in the Region that the snapshot is being
-- copied to.
--
-- Amazon EBS does not support asymmetric CMKs.
--
-- 'clientData', 'importSnapshot_clientData' - The client-specific data.
--
-- 'description', 'importSnapshot_description' - The description string for the import snapshot task.
--
-- 'clientToken', 'importSnapshot_clientToken' - Token to enable idempotency for VM import requests.
--
-- 'diskContainer', 'importSnapshot_diskContainer' - Information about the disk container.
newImportSnapshot ::
  ImportSnapshot
newImportSnapshot =
  ImportSnapshot'
    { tagSpecifications = Core.Nothing,
      dryRun = Core.Nothing,
      encrypted = Core.Nothing,
      roleName = Core.Nothing,
      kmsKeyId = Core.Nothing,
      clientData = Core.Nothing,
      description = Core.Nothing,
      clientToken = Core.Nothing,
      diskContainer = Core.Nothing
    }

-- | The tags to apply to the import snapshot task during creation.
importSnapshot_tagSpecifications :: Lens.Lens' ImportSnapshot (Core.Maybe [TagSpecification])
importSnapshot_tagSpecifications = Lens.lens (\ImportSnapshot' {tagSpecifications} -> tagSpecifications) (\s@ImportSnapshot' {} a -> s {tagSpecifications = a} :: ImportSnapshot) Core.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
importSnapshot_dryRun :: Lens.Lens' ImportSnapshot (Core.Maybe Core.Bool)
importSnapshot_dryRun = Lens.lens (\ImportSnapshot' {dryRun} -> dryRun) (\s@ImportSnapshot' {} a -> s {dryRun = a} :: ImportSnapshot)

-- | Specifies whether the destination snapshot of the imported image should
-- be encrypted. The default CMK for EBS is used unless you specify a
-- non-default AWS Key Management Service (AWS KMS) CMK using @KmsKeyId@.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
importSnapshot_encrypted :: Lens.Lens' ImportSnapshot (Core.Maybe Core.Bool)
importSnapshot_encrypted = Lens.lens (\ImportSnapshot' {encrypted} -> encrypted) (\s@ImportSnapshot' {} a -> s {encrypted = a} :: ImportSnapshot)

-- | The name of the role to use when not using the default role,
-- \'vmimport\'.
importSnapshot_roleName :: Lens.Lens' ImportSnapshot (Core.Maybe Core.Text)
importSnapshot_roleName = Lens.lens (\ImportSnapshot' {roleName} -> roleName) (\s@ImportSnapshot' {} a -> s {roleName = a} :: ImportSnapshot)

-- | An identifier for the symmetric AWS Key Management Service (AWS KMS)
-- customer master key (CMK) to use when creating the encrypted snapshot.
-- This parameter is only required if you want to use a non-default CMK; if
-- this parameter is not specified, the default CMK for EBS is used. If a
-- @KmsKeyId@ is specified, the @Encrypted@ flag must also be set.
--
-- The CMK identifier may be provided in any of the following formats:
--
-- -   Key ID
--
-- -   Key alias. The alias ARN contains the @arn:aws:kms@ namespace,
--     followed by the Region of the CMK, the AWS account ID of the CMK
--     owner, the @alias@ namespace, and then the CMK alias. For example,
--     arn:aws:kms:/us-east-1/:/012345678910/:alias\//ExampleAlias/.
--
-- -   ARN using key ID. The ID ARN contains the @arn:aws:kms@ namespace,
--     followed by the Region of the CMK, the AWS account ID of the CMK
--     owner, the @key@ namespace, and then the CMK ID. For example,
--     arn:aws:kms:/us-east-1/:/012345678910/:key\//abcd1234-a123-456a-a12b-a123b4cd56ef/.
--
-- -   ARN using key alias. The alias ARN contains the @arn:aws:kms@
--     namespace, followed by the Region of the CMK, the AWS account ID of
--     the CMK owner, the @alias@ namespace, and then the CMK alias. For
--     example,
--     arn:aws:kms:/us-east-1/:/012345678910/:alias\//ExampleAlias/.
--
-- AWS parses @KmsKeyId@ asynchronously, meaning that the action you call
-- may appear to complete even though you provided an invalid identifier.
-- This action will eventually report failure.
--
-- The specified CMK must exist in the Region that the snapshot is being
-- copied to.
--
-- Amazon EBS does not support asymmetric CMKs.
importSnapshot_kmsKeyId :: Lens.Lens' ImportSnapshot (Core.Maybe Core.Text)
importSnapshot_kmsKeyId = Lens.lens (\ImportSnapshot' {kmsKeyId} -> kmsKeyId) (\s@ImportSnapshot' {} a -> s {kmsKeyId = a} :: ImportSnapshot)

-- | The client-specific data.
importSnapshot_clientData :: Lens.Lens' ImportSnapshot (Core.Maybe ClientData)
importSnapshot_clientData = Lens.lens (\ImportSnapshot' {clientData} -> clientData) (\s@ImportSnapshot' {} a -> s {clientData = a} :: ImportSnapshot)

-- | The description string for the import snapshot task.
importSnapshot_description :: Lens.Lens' ImportSnapshot (Core.Maybe Core.Text)
importSnapshot_description = Lens.lens (\ImportSnapshot' {description} -> description) (\s@ImportSnapshot' {} a -> s {description = a} :: ImportSnapshot)

-- | Token to enable idempotency for VM import requests.
importSnapshot_clientToken :: Lens.Lens' ImportSnapshot (Core.Maybe Core.Text)
importSnapshot_clientToken = Lens.lens (\ImportSnapshot' {clientToken} -> clientToken) (\s@ImportSnapshot' {} a -> s {clientToken = a} :: ImportSnapshot)

-- | Information about the disk container.
importSnapshot_diskContainer :: Lens.Lens' ImportSnapshot (Core.Maybe SnapshotDiskContainer)
importSnapshot_diskContainer = Lens.lens (\ImportSnapshot' {diskContainer} -> diskContainer) (\s@ImportSnapshot' {} a -> s {diskContainer = a} :: ImportSnapshot)

instance Core.AWSRequest ImportSnapshot where
  type
    AWSResponse ImportSnapshot =
      ImportSnapshotResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ImportSnapshotResponse'
            Core.<$> (x Core..@? "snapshotTaskDetail")
            Core.<*> (x Core..@? "importTaskId")
            Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (x Core..@? "description")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ImportSnapshot

instance Core.NFData ImportSnapshot

instance Core.ToHeaders ImportSnapshot where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ImportSnapshot where
  toPath = Core.const "/"

instance Core.ToQuery ImportSnapshot where
  toQuery ImportSnapshot' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ImportSnapshot" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Core.<$> tagSpecifications
          ),
        "DryRun" Core.=: dryRun,
        "Encrypted" Core.=: encrypted,
        "RoleName" Core.=: roleName,
        "KmsKeyId" Core.=: kmsKeyId,
        "ClientData" Core.=: clientData,
        "Description" Core.=: description,
        "ClientToken" Core.=: clientToken,
        "DiskContainer" Core.=: diskContainer
      ]

-- | /See:/ 'newImportSnapshotResponse' smart constructor.
data ImportSnapshotResponse = ImportSnapshotResponse'
  { -- | Information about the import snapshot task.
    snapshotTaskDetail :: Core.Maybe SnapshotTaskDetail,
    -- | The ID of the import snapshot task.
    importTaskId :: Core.Maybe Core.Text,
    -- | Any tags assigned to the import snapshot task.
    tags :: Core.Maybe [Tag],
    -- | A description of the import snapshot task.
    description :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ImportSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshotTaskDetail', 'importSnapshotResponse_snapshotTaskDetail' - Information about the import snapshot task.
--
-- 'importTaskId', 'importSnapshotResponse_importTaskId' - The ID of the import snapshot task.
--
-- 'tags', 'importSnapshotResponse_tags' - Any tags assigned to the import snapshot task.
--
-- 'description', 'importSnapshotResponse_description' - A description of the import snapshot task.
--
-- 'httpStatus', 'importSnapshotResponse_httpStatus' - The response's http status code.
newImportSnapshotResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ImportSnapshotResponse
newImportSnapshotResponse pHttpStatus_ =
  ImportSnapshotResponse'
    { snapshotTaskDetail =
        Core.Nothing,
      importTaskId = Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the import snapshot task.
importSnapshotResponse_snapshotTaskDetail :: Lens.Lens' ImportSnapshotResponse (Core.Maybe SnapshotTaskDetail)
importSnapshotResponse_snapshotTaskDetail = Lens.lens (\ImportSnapshotResponse' {snapshotTaskDetail} -> snapshotTaskDetail) (\s@ImportSnapshotResponse' {} a -> s {snapshotTaskDetail = a} :: ImportSnapshotResponse)

-- | The ID of the import snapshot task.
importSnapshotResponse_importTaskId :: Lens.Lens' ImportSnapshotResponse (Core.Maybe Core.Text)
importSnapshotResponse_importTaskId = Lens.lens (\ImportSnapshotResponse' {importTaskId} -> importTaskId) (\s@ImportSnapshotResponse' {} a -> s {importTaskId = a} :: ImportSnapshotResponse)

-- | Any tags assigned to the import snapshot task.
importSnapshotResponse_tags :: Lens.Lens' ImportSnapshotResponse (Core.Maybe [Tag])
importSnapshotResponse_tags = Lens.lens (\ImportSnapshotResponse' {tags} -> tags) (\s@ImportSnapshotResponse' {} a -> s {tags = a} :: ImportSnapshotResponse) Core.. Lens.mapping Lens._Coerce

-- | A description of the import snapshot task.
importSnapshotResponse_description :: Lens.Lens' ImportSnapshotResponse (Core.Maybe Core.Text)
importSnapshotResponse_description = Lens.lens (\ImportSnapshotResponse' {description} -> description) (\s@ImportSnapshotResponse' {} a -> s {description = a} :: ImportSnapshotResponse)

-- | The response's http status code.
importSnapshotResponse_httpStatus :: Lens.Lens' ImportSnapshotResponse Core.Int
importSnapshotResponse_httpStatus = Lens.lens (\ImportSnapshotResponse' {httpStatus} -> httpStatus) (\s@ImportSnapshotResponse' {} a -> s {httpStatus = a} :: ImportSnapshotResponse)

instance Core.NFData ImportSnapshotResponse
