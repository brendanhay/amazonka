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
--
-- For more information, see
-- <https://docs.aws.amazon.com/vm-import/latest/userguide/vmimport-import-snapshot.html Importing a disk as a snapshot using VM Import\/Export>
-- in the /VM Import\/Export User Guide/.
module Network.AWS.EC2.ImportSnapshot
  ( -- * Creating a Request
    ImportSnapshot (..),
    newImportSnapshot,

    -- * Request Lenses
    importSnapshot_diskContainer,
    importSnapshot_clientToken,
    importSnapshot_roleName,
    importSnapshot_encrypted,
    importSnapshot_tagSpecifications,
    importSnapshot_kmsKeyId,
    importSnapshot_description,
    importSnapshot_dryRun,
    importSnapshot_clientData,

    -- * Destructuring the Response
    ImportSnapshotResponse (..),
    newImportSnapshotResponse,

    -- * Response Lenses
    importSnapshotResponse_snapshotTaskDetail,
    importSnapshotResponse_importTaskId,
    importSnapshotResponse_description,
    importSnapshotResponse_tags,
    importSnapshotResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newImportSnapshot' smart constructor.
data ImportSnapshot = ImportSnapshot'
  { -- | Information about the disk container.
    diskContainer :: Prelude.Maybe SnapshotDiskContainer,
    -- | Token to enable idempotency for VM import requests.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the role to use when not using the default role,
    -- \'vmimport\'.
    roleName :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the destination snapshot of the imported image should
    -- be encrypted. The default KMS key for EBS is used unless you specify a
    -- non-default KMS key using @KmsKeyId@. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The tags to apply to the import snapshot task during creation.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | An identifier for the symmetric KMS key to use when creating the
    -- encrypted snapshot. This parameter is only required if you want to use a
    -- non-default KMS key; if this parameter is not specified, the default KMS
    -- key for EBS is used. If a @KmsKeyId@ is specified, the @Encrypted@ flag
    -- must also be set.
    --
    -- The KMS key identifier may be provided in any of the following formats:
    --
    -- -   Key ID
    --
    -- -   Key alias. The alias ARN contains the @arn:aws:kms@ namespace,
    --     followed by the Region of the key, the Amazon Web Services account
    --     ID of the key owner, the @alias@ namespace, and then the key alias.
    --     For example,
    --     arn:aws:kms:/us-east-1/:/012345678910/:alias\//ExampleAlias/.
    --
    -- -   ARN using key ID. The ID ARN contains the @arn:aws:kms@ namespace,
    --     followed by the Region of the key, the Amazon Web Services account
    --     ID of the key owner, the @key@ namespace, and then the key ID. For
    --     example,
    --     arn:aws:kms:/us-east-1/:/012345678910/:key\//abcd1234-a123-456a-a12b-a123b4cd56ef/.
    --
    -- -   ARN using key alias. The alias ARN contains the @arn:aws:kms@
    --     namespace, followed by the Region of the key, the Amazon Web
    --     Services account ID of the key owner, the @alias@ namespace, and
    --     then the key alias. For example,
    --     arn:aws:kms:/us-east-1/:/012345678910/:alias\//ExampleAlias/.
    --
    -- Amazon Web Services parses @KmsKeyId@ asynchronously, meaning that the
    -- action you call may appear to complete even though you provided an
    -- invalid identifier. This action will eventually report failure.
    --
    -- The specified KMS key must exist in the Region that the snapshot is
    -- being copied to.
    --
    -- Amazon EBS does not support asymmetric KMS keys.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The description string for the import snapshot task.
    description :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The client-specific data.
    clientData :: Prelude.Maybe ClientData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'diskContainer', 'importSnapshot_diskContainer' - Information about the disk container.
--
-- 'clientToken', 'importSnapshot_clientToken' - Token to enable idempotency for VM import requests.
--
-- 'roleName', 'importSnapshot_roleName' - The name of the role to use when not using the default role,
-- \'vmimport\'.
--
-- 'encrypted', 'importSnapshot_encrypted' - Specifies whether the destination snapshot of the imported image should
-- be encrypted. The default KMS key for EBS is used unless you specify a
-- non-default KMS key using @KmsKeyId@. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'tagSpecifications', 'importSnapshot_tagSpecifications' - The tags to apply to the import snapshot task during creation.
--
-- 'kmsKeyId', 'importSnapshot_kmsKeyId' - An identifier for the symmetric KMS key to use when creating the
-- encrypted snapshot. This parameter is only required if you want to use a
-- non-default KMS key; if this parameter is not specified, the default KMS
-- key for EBS is used. If a @KmsKeyId@ is specified, the @Encrypted@ flag
-- must also be set.
--
-- The KMS key identifier may be provided in any of the following formats:
--
-- -   Key ID
--
-- -   Key alias. The alias ARN contains the @arn:aws:kms@ namespace,
--     followed by the Region of the key, the Amazon Web Services account
--     ID of the key owner, the @alias@ namespace, and then the key alias.
--     For example,
--     arn:aws:kms:/us-east-1/:/012345678910/:alias\//ExampleAlias/.
--
-- -   ARN using key ID. The ID ARN contains the @arn:aws:kms@ namespace,
--     followed by the Region of the key, the Amazon Web Services account
--     ID of the key owner, the @key@ namespace, and then the key ID. For
--     example,
--     arn:aws:kms:/us-east-1/:/012345678910/:key\//abcd1234-a123-456a-a12b-a123b4cd56ef/.
--
-- -   ARN using key alias. The alias ARN contains the @arn:aws:kms@
--     namespace, followed by the Region of the key, the Amazon Web
--     Services account ID of the key owner, the @alias@ namespace, and
--     then the key alias. For example,
--     arn:aws:kms:/us-east-1/:/012345678910/:alias\//ExampleAlias/.
--
-- Amazon Web Services parses @KmsKeyId@ asynchronously, meaning that the
-- action you call may appear to complete even though you provided an
-- invalid identifier. This action will eventually report failure.
--
-- The specified KMS key must exist in the Region that the snapshot is
-- being copied to.
--
-- Amazon EBS does not support asymmetric KMS keys.
--
-- 'description', 'importSnapshot_description' - The description string for the import snapshot task.
--
-- 'dryRun', 'importSnapshot_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'clientData', 'importSnapshot_clientData' - The client-specific data.
newImportSnapshot ::
  ImportSnapshot
newImportSnapshot =
  ImportSnapshot'
    { diskContainer = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      roleName = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      description = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      clientData = Prelude.Nothing
    }

-- | Information about the disk container.
importSnapshot_diskContainer :: Lens.Lens' ImportSnapshot (Prelude.Maybe SnapshotDiskContainer)
importSnapshot_diskContainer = Lens.lens (\ImportSnapshot' {diskContainer} -> diskContainer) (\s@ImportSnapshot' {} a -> s {diskContainer = a} :: ImportSnapshot)

-- | Token to enable idempotency for VM import requests.
importSnapshot_clientToken :: Lens.Lens' ImportSnapshot (Prelude.Maybe Prelude.Text)
importSnapshot_clientToken = Lens.lens (\ImportSnapshot' {clientToken} -> clientToken) (\s@ImportSnapshot' {} a -> s {clientToken = a} :: ImportSnapshot)

-- | The name of the role to use when not using the default role,
-- \'vmimport\'.
importSnapshot_roleName :: Lens.Lens' ImportSnapshot (Prelude.Maybe Prelude.Text)
importSnapshot_roleName = Lens.lens (\ImportSnapshot' {roleName} -> roleName) (\s@ImportSnapshot' {} a -> s {roleName = a} :: ImportSnapshot)

-- | Specifies whether the destination snapshot of the imported image should
-- be encrypted. The default KMS key for EBS is used unless you specify a
-- non-default KMS key using @KmsKeyId@. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
importSnapshot_encrypted :: Lens.Lens' ImportSnapshot (Prelude.Maybe Prelude.Bool)
importSnapshot_encrypted = Lens.lens (\ImportSnapshot' {encrypted} -> encrypted) (\s@ImportSnapshot' {} a -> s {encrypted = a} :: ImportSnapshot)

-- | The tags to apply to the import snapshot task during creation.
importSnapshot_tagSpecifications :: Lens.Lens' ImportSnapshot (Prelude.Maybe [TagSpecification])
importSnapshot_tagSpecifications = Lens.lens (\ImportSnapshot' {tagSpecifications} -> tagSpecifications) (\s@ImportSnapshot' {} a -> s {tagSpecifications = a} :: ImportSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | An identifier for the symmetric KMS key to use when creating the
-- encrypted snapshot. This parameter is only required if you want to use a
-- non-default KMS key; if this parameter is not specified, the default KMS
-- key for EBS is used. If a @KmsKeyId@ is specified, the @Encrypted@ flag
-- must also be set.
--
-- The KMS key identifier may be provided in any of the following formats:
--
-- -   Key ID
--
-- -   Key alias. The alias ARN contains the @arn:aws:kms@ namespace,
--     followed by the Region of the key, the Amazon Web Services account
--     ID of the key owner, the @alias@ namespace, and then the key alias.
--     For example,
--     arn:aws:kms:/us-east-1/:/012345678910/:alias\//ExampleAlias/.
--
-- -   ARN using key ID. The ID ARN contains the @arn:aws:kms@ namespace,
--     followed by the Region of the key, the Amazon Web Services account
--     ID of the key owner, the @key@ namespace, and then the key ID. For
--     example,
--     arn:aws:kms:/us-east-1/:/012345678910/:key\//abcd1234-a123-456a-a12b-a123b4cd56ef/.
--
-- -   ARN using key alias. The alias ARN contains the @arn:aws:kms@
--     namespace, followed by the Region of the key, the Amazon Web
--     Services account ID of the key owner, the @alias@ namespace, and
--     then the key alias. For example,
--     arn:aws:kms:/us-east-1/:/012345678910/:alias\//ExampleAlias/.
--
-- Amazon Web Services parses @KmsKeyId@ asynchronously, meaning that the
-- action you call may appear to complete even though you provided an
-- invalid identifier. This action will eventually report failure.
--
-- The specified KMS key must exist in the Region that the snapshot is
-- being copied to.
--
-- Amazon EBS does not support asymmetric KMS keys.
importSnapshot_kmsKeyId :: Lens.Lens' ImportSnapshot (Prelude.Maybe Prelude.Text)
importSnapshot_kmsKeyId = Lens.lens (\ImportSnapshot' {kmsKeyId} -> kmsKeyId) (\s@ImportSnapshot' {} a -> s {kmsKeyId = a} :: ImportSnapshot)

-- | The description string for the import snapshot task.
importSnapshot_description :: Lens.Lens' ImportSnapshot (Prelude.Maybe Prelude.Text)
importSnapshot_description = Lens.lens (\ImportSnapshot' {description} -> description) (\s@ImportSnapshot' {} a -> s {description = a} :: ImportSnapshot)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
importSnapshot_dryRun :: Lens.Lens' ImportSnapshot (Prelude.Maybe Prelude.Bool)
importSnapshot_dryRun = Lens.lens (\ImportSnapshot' {dryRun} -> dryRun) (\s@ImportSnapshot' {} a -> s {dryRun = a} :: ImportSnapshot)

-- | The client-specific data.
importSnapshot_clientData :: Lens.Lens' ImportSnapshot (Prelude.Maybe ClientData)
importSnapshot_clientData = Lens.lens (\ImportSnapshot' {clientData} -> clientData) (\s@ImportSnapshot' {} a -> s {clientData = a} :: ImportSnapshot)

instance Core.AWSRequest ImportSnapshot where
  type
    AWSResponse ImportSnapshot =
      ImportSnapshotResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ImportSnapshotResponse'
            Prelude.<$> (x Core..@? "snapshotTaskDetail")
            Prelude.<*> (x Core..@? "importTaskId")
            Prelude.<*> (x Core..@? "description")
            Prelude.<*> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportSnapshot

instance Prelude.NFData ImportSnapshot

instance Core.ToHeaders ImportSnapshot where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ImportSnapshot where
  toPath = Prelude.const "/"

instance Core.ToQuery ImportSnapshot where
  toQuery ImportSnapshot' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ImportSnapshot" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DiskContainer" Core.=: diskContainer,
        "ClientToken" Core.=: clientToken,
        "RoleName" Core.=: roleName,
        "Encrypted" Core.=: encrypted,
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "KmsKeyId" Core.=: kmsKeyId,
        "Description" Core.=: description,
        "DryRun" Core.=: dryRun,
        "ClientData" Core.=: clientData
      ]

-- | /See:/ 'newImportSnapshotResponse' smart constructor.
data ImportSnapshotResponse = ImportSnapshotResponse'
  { -- | Information about the import snapshot task.
    snapshotTaskDetail :: Prelude.Maybe SnapshotTaskDetail,
    -- | The ID of the import snapshot task.
    importTaskId :: Prelude.Maybe Prelude.Text,
    -- | A description of the import snapshot task.
    description :: Prelude.Maybe Prelude.Text,
    -- | Any tags assigned to the import snapshot task.
    tags :: Prelude.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'description', 'importSnapshotResponse_description' - A description of the import snapshot task.
--
-- 'tags', 'importSnapshotResponse_tags' - Any tags assigned to the import snapshot task.
--
-- 'httpStatus', 'importSnapshotResponse_httpStatus' - The response's http status code.
newImportSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ImportSnapshotResponse
newImportSnapshotResponse pHttpStatus_ =
  ImportSnapshotResponse'
    { snapshotTaskDetail =
        Prelude.Nothing,
      importTaskId = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the import snapshot task.
importSnapshotResponse_snapshotTaskDetail :: Lens.Lens' ImportSnapshotResponse (Prelude.Maybe SnapshotTaskDetail)
importSnapshotResponse_snapshotTaskDetail = Lens.lens (\ImportSnapshotResponse' {snapshotTaskDetail} -> snapshotTaskDetail) (\s@ImportSnapshotResponse' {} a -> s {snapshotTaskDetail = a} :: ImportSnapshotResponse)

-- | The ID of the import snapshot task.
importSnapshotResponse_importTaskId :: Lens.Lens' ImportSnapshotResponse (Prelude.Maybe Prelude.Text)
importSnapshotResponse_importTaskId = Lens.lens (\ImportSnapshotResponse' {importTaskId} -> importTaskId) (\s@ImportSnapshotResponse' {} a -> s {importTaskId = a} :: ImportSnapshotResponse)

-- | A description of the import snapshot task.
importSnapshotResponse_description :: Lens.Lens' ImportSnapshotResponse (Prelude.Maybe Prelude.Text)
importSnapshotResponse_description = Lens.lens (\ImportSnapshotResponse' {description} -> description) (\s@ImportSnapshotResponse' {} a -> s {description = a} :: ImportSnapshotResponse)

-- | Any tags assigned to the import snapshot task.
importSnapshotResponse_tags :: Lens.Lens' ImportSnapshotResponse (Prelude.Maybe [Tag])
importSnapshotResponse_tags = Lens.lens (\ImportSnapshotResponse' {tags} -> tags) (\s@ImportSnapshotResponse' {} a -> s {tags = a} :: ImportSnapshotResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
importSnapshotResponse_httpStatus :: Lens.Lens' ImportSnapshotResponse Prelude.Int
importSnapshotResponse_httpStatus = Lens.lens (\ImportSnapshotResponse' {httpStatus} -> httpStatus) (\s@ImportSnapshotResponse' {} a -> s {httpStatus = a} :: ImportSnapshotResponse)

instance Prelude.NFData ImportSnapshotResponse
