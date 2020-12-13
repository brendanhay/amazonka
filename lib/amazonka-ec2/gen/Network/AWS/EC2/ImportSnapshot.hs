{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ImportSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports a disk into an EBS snapshot.
module Network.AWS.EC2.ImportSnapshot
  ( -- * Creating a request
    ImportSnapshot (..),
    mkImportSnapshot,

    -- ** Request lenses
    isDiskContainer,
    isClientToken,
    isRoleName,
    isEncrypted,
    isTagSpecifications,
    isKMSKeyId,
    isDescription,
    isDryRun,
    isClientData,

    -- * Destructuring the response
    ImportSnapshotResponse (..),
    mkImportSnapshotResponse,

    -- ** Response lenses
    isrsSnapshotTaskDetail,
    isrsImportTaskId,
    isrsDescription,
    isrsTags,
    isrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkImportSnapshot' smart constructor.
data ImportSnapshot = ImportSnapshot'
  { -- | Information about the disk container.
    diskContainer :: Lude.Maybe SnapshotDiskContainer,
    -- | Token to enable idempotency for VM import requests.
    clientToken :: Lude.Maybe Lude.Text,
    -- | The name of the role to use when not using the default role, 'vmimport'.
    roleName :: Lude.Maybe Lude.Text,
    -- | Specifies whether the destination snapshot of the imported image should be encrypted. The default CMK for EBS is used unless you specify a non-default AWS Key Management Service (AWS KMS) CMK using @KmsKeyId@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
    encrypted :: Lude.Maybe Lude.Bool,
    -- | The tags to apply to the snapshot being imported.
    tagSpecifications :: Lude.Maybe [TagSpecification],
    -- | An identifier for the symmetric AWS Key Management Service (AWS KMS) customer master key (CMK) to use when creating the encrypted snapshot. This parameter is only required if you want to use a non-default CMK; if this parameter is not specified, the default CMK for EBS is used. If a @KmsKeyId@ is specified, the @Encrypted@ flag must also be set.
    --
    -- The CMK identifier may be provided in any of the following formats:
    --
    --     * Key ID
    --
    --
    --     * Key alias. The alias ARN contains the @arn:aws:kms@ namespace, followed by the Region of the CMK, the AWS account ID of the CMK owner, the @alias@ namespace, and then the CMK alias. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :alias//ExampleAlias/ .
    --
    --
    --     * ARN using key ID. The ID ARN contains the @arn:aws:kms@ namespace, followed by the Region of the CMK, the AWS account ID of the CMK owner, the @key@ namespace, and then the CMK ID. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :key//abcd1234-a123-456a-a12b-a123b4cd56ef/ .
    --
    --
    --     * ARN using key alias. The alias ARN contains the @arn:aws:kms@ namespace, followed by the Region of the CMK, the AWS account ID of the CMK owner, the @alias@ namespace, and then the CMK alias. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :alias//ExampleAlias/ .
    --
    --
    -- AWS parses @KmsKeyId@ asynchronously, meaning that the action you call may appear to complete even though you provided an invalid identifier. This action will eventually report failure.
    -- The specified CMK must exist in the Region that the snapshot is being copied to.
    -- Amazon EBS does not support asymmetric CMKs.
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | The description string for the import snapshot task.
    description :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The client-specific data.
    clientData :: Lude.Maybe ClientData
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportSnapshot' with the minimum fields required to make a request.
--
-- * 'diskContainer' - Information about the disk container.
-- * 'clientToken' - Token to enable idempotency for VM import requests.
-- * 'roleName' - The name of the role to use when not using the default role, 'vmimport'.
-- * 'encrypted' - Specifies whether the destination snapshot of the imported image should be encrypted. The default CMK for EBS is used unless you specify a non-default AWS Key Management Service (AWS KMS) CMK using @KmsKeyId@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
-- * 'tagSpecifications' - The tags to apply to the snapshot being imported.
-- * 'kmsKeyId' - An identifier for the symmetric AWS Key Management Service (AWS KMS) customer master key (CMK) to use when creating the encrypted snapshot. This parameter is only required if you want to use a non-default CMK; if this parameter is not specified, the default CMK for EBS is used. If a @KmsKeyId@ is specified, the @Encrypted@ flag must also be set.
--
-- The CMK identifier may be provided in any of the following formats:
--
--     * Key ID
--
--
--     * Key alias. The alias ARN contains the @arn:aws:kms@ namespace, followed by the Region of the CMK, the AWS account ID of the CMK owner, the @alias@ namespace, and then the CMK alias. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :alias//ExampleAlias/ .
--
--
--     * ARN using key ID. The ID ARN contains the @arn:aws:kms@ namespace, followed by the Region of the CMK, the AWS account ID of the CMK owner, the @key@ namespace, and then the CMK ID. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :key//abcd1234-a123-456a-a12b-a123b4cd56ef/ .
--
--
--     * ARN using key alias. The alias ARN contains the @arn:aws:kms@ namespace, followed by the Region of the CMK, the AWS account ID of the CMK owner, the @alias@ namespace, and then the CMK alias. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :alias//ExampleAlias/ .
--
--
-- AWS parses @KmsKeyId@ asynchronously, meaning that the action you call may appear to complete even though you provided an invalid identifier. This action will eventually report failure.
-- The specified CMK must exist in the Region that the snapshot is being copied to.
-- Amazon EBS does not support asymmetric CMKs.
-- * 'description' - The description string for the import snapshot task.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'clientData' - The client-specific data.
mkImportSnapshot ::
  ImportSnapshot
mkImportSnapshot =
  ImportSnapshot'
    { diskContainer = Lude.Nothing,
      clientToken = Lude.Nothing,
      roleName = Lude.Nothing,
      encrypted = Lude.Nothing,
      tagSpecifications = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      description = Lude.Nothing,
      dryRun = Lude.Nothing,
      clientData = Lude.Nothing
    }

-- | Information about the disk container.
--
-- /Note:/ Consider using 'diskContainer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isDiskContainer :: Lens.Lens' ImportSnapshot (Lude.Maybe SnapshotDiskContainer)
isDiskContainer = Lens.lens (diskContainer :: ImportSnapshot -> Lude.Maybe SnapshotDiskContainer) (\s a -> s {diskContainer = a} :: ImportSnapshot)
{-# DEPRECATED isDiskContainer "Use generic-lens or generic-optics with 'diskContainer' instead." #-}

-- | Token to enable idempotency for VM import requests.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isClientToken :: Lens.Lens' ImportSnapshot (Lude.Maybe Lude.Text)
isClientToken = Lens.lens (clientToken :: ImportSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: ImportSnapshot)
{-# DEPRECATED isClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The name of the role to use when not using the default role, 'vmimport'.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isRoleName :: Lens.Lens' ImportSnapshot (Lude.Maybe Lude.Text)
isRoleName = Lens.lens (roleName :: ImportSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {roleName = a} :: ImportSnapshot)
{-# DEPRECATED isRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | Specifies whether the destination snapshot of the imported image should be encrypted. The default CMK for EBS is used unless you specify a non-default AWS Key Management Service (AWS KMS) CMK using @KmsKeyId@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isEncrypted :: Lens.Lens' ImportSnapshot (Lude.Maybe Lude.Bool)
isEncrypted = Lens.lens (encrypted :: ImportSnapshot -> Lude.Maybe Lude.Bool) (\s a -> s {encrypted = a} :: ImportSnapshot)
{-# DEPRECATED isEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The tags to apply to the snapshot being imported.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isTagSpecifications :: Lens.Lens' ImportSnapshot (Lude.Maybe [TagSpecification])
isTagSpecifications = Lens.lens (tagSpecifications :: ImportSnapshot -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: ImportSnapshot)
{-# DEPRECATED isTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | An identifier for the symmetric AWS Key Management Service (AWS KMS) customer master key (CMK) to use when creating the encrypted snapshot. This parameter is only required if you want to use a non-default CMK; if this parameter is not specified, the default CMK for EBS is used. If a @KmsKeyId@ is specified, the @Encrypted@ flag must also be set.
--
-- The CMK identifier may be provided in any of the following formats:
--
--     * Key ID
--
--
--     * Key alias. The alias ARN contains the @arn:aws:kms@ namespace, followed by the Region of the CMK, the AWS account ID of the CMK owner, the @alias@ namespace, and then the CMK alias. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :alias//ExampleAlias/ .
--
--
--     * ARN using key ID. The ID ARN contains the @arn:aws:kms@ namespace, followed by the Region of the CMK, the AWS account ID of the CMK owner, the @key@ namespace, and then the CMK ID. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :key//abcd1234-a123-456a-a12b-a123b4cd56ef/ .
--
--
--     * ARN using key alias. The alias ARN contains the @arn:aws:kms@ namespace, followed by the Region of the CMK, the AWS account ID of the CMK owner, the @alias@ namespace, and then the CMK alias. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :alias//ExampleAlias/ .
--
--
-- AWS parses @KmsKeyId@ asynchronously, meaning that the action you call may appear to complete even though you provided an invalid identifier. This action will eventually report failure.
-- The specified CMK must exist in the Region that the snapshot is being copied to.
-- Amazon EBS does not support asymmetric CMKs.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isKMSKeyId :: Lens.Lens' ImportSnapshot (Lude.Maybe Lude.Text)
isKMSKeyId = Lens.lens (kmsKeyId :: ImportSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: ImportSnapshot)
{-# DEPRECATED isKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The description string for the import snapshot task.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isDescription :: Lens.Lens' ImportSnapshot (Lude.Maybe Lude.Text)
isDescription = Lens.lens (description :: ImportSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ImportSnapshot)
{-# DEPRECATED isDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isDryRun :: Lens.Lens' ImportSnapshot (Lude.Maybe Lude.Bool)
isDryRun = Lens.lens (dryRun :: ImportSnapshot -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ImportSnapshot)
{-# DEPRECATED isDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The client-specific data.
--
-- /Note:/ Consider using 'clientData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isClientData :: Lens.Lens' ImportSnapshot (Lude.Maybe ClientData)
isClientData = Lens.lens (clientData :: ImportSnapshot -> Lude.Maybe ClientData) (\s a -> s {clientData = a} :: ImportSnapshot)
{-# DEPRECATED isClientData "Use generic-lens or generic-optics with 'clientData' instead." #-}

instance Lude.AWSRequest ImportSnapshot where
  type Rs ImportSnapshot = ImportSnapshotResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ImportSnapshotResponse'
            Lude.<$> (x Lude..@? "snapshotTaskDetail")
            Lude.<*> (x Lude..@? "importTaskId")
            Lude.<*> (x Lude..@? "description")
            Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ImportSnapshot where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ImportSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery ImportSnapshot where
  toQuery ImportSnapshot' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ImportSnapshot" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DiskContainer" Lude.=: diskContainer,
        "ClientToken" Lude.=: clientToken,
        "RoleName" Lude.=: roleName,
        "Encrypted" Lude.=: encrypted,
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "KmsKeyId" Lude.=: kmsKeyId,
        "Description" Lude.=: description,
        "DryRun" Lude.=: dryRun,
        "ClientData" Lude.=: clientData
      ]

-- | /See:/ 'mkImportSnapshotResponse' smart constructor.
data ImportSnapshotResponse = ImportSnapshotResponse'
  { -- | Information about the import snapshot task.
    snapshotTaskDetail :: Lude.Maybe SnapshotTaskDetail,
    -- | The ID of the import snapshot task.
    importTaskId :: Lude.Maybe Lude.Text,
    -- | A description of the import snapshot task.
    description :: Lude.Maybe Lude.Text,
    -- | Any tags assigned to the snapshot being imported.
    tags :: Lude.Maybe [Tag],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'snapshotTaskDetail' - Information about the import snapshot task.
-- * 'importTaskId' - The ID of the import snapshot task.
-- * 'description' - A description of the import snapshot task.
-- * 'tags' - Any tags assigned to the snapshot being imported.
-- * 'responseStatus' - The response status code.
mkImportSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ImportSnapshotResponse
mkImportSnapshotResponse pResponseStatus_ =
  ImportSnapshotResponse'
    { snapshotTaskDetail = Lude.Nothing,
      importTaskId = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the import snapshot task.
--
-- /Note:/ Consider using 'snapshotTaskDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isrsSnapshotTaskDetail :: Lens.Lens' ImportSnapshotResponse (Lude.Maybe SnapshotTaskDetail)
isrsSnapshotTaskDetail = Lens.lens (snapshotTaskDetail :: ImportSnapshotResponse -> Lude.Maybe SnapshotTaskDetail) (\s a -> s {snapshotTaskDetail = a} :: ImportSnapshotResponse)
{-# DEPRECATED isrsSnapshotTaskDetail "Use generic-lens or generic-optics with 'snapshotTaskDetail' instead." #-}

-- | The ID of the import snapshot task.
--
-- /Note:/ Consider using 'importTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isrsImportTaskId :: Lens.Lens' ImportSnapshotResponse (Lude.Maybe Lude.Text)
isrsImportTaskId = Lens.lens (importTaskId :: ImportSnapshotResponse -> Lude.Maybe Lude.Text) (\s a -> s {importTaskId = a} :: ImportSnapshotResponse)
{-# DEPRECATED isrsImportTaskId "Use generic-lens or generic-optics with 'importTaskId' instead." #-}

-- | A description of the import snapshot task.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isrsDescription :: Lens.Lens' ImportSnapshotResponse (Lude.Maybe Lude.Text)
isrsDescription = Lens.lens (description :: ImportSnapshotResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ImportSnapshotResponse)
{-# DEPRECATED isrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Any tags assigned to the snapshot being imported.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isrsTags :: Lens.Lens' ImportSnapshotResponse (Lude.Maybe [Tag])
isrsTags = Lens.lens (tags :: ImportSnapshotResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ImportSnapshotResponse)
{-# DEPRECATED isrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isrsResponseStatus :: Lens.Lens' ImportSnapshotResponse Lude.Int
isrsResponseStatus = Lens.lens (responseStatus :: ImportSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ImportSnapshotResponse)
{-# DEPRECATED isrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
