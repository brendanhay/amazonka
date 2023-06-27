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
-- Module      : Amazonka.EC2.CopyImage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates the copy of an AMI. You can copy an AMI from one Region to
-- another, or from a Region to an Outpost. You can\'t copy an AMI from an
-- Outpost to a Region, from one Outpost to another, or within the same
-- Outpost. To copy an AMI to another partition, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateStoreImageTask.html CreateStoreImageTask>.
--
-- To copy an AMI from one Region to another, specify the source Region
-- using the __SourceRegion__ parameter, and specify the destination Region
-- using its endpoint. Copies of encrypted backing snapshots for the AMI
-- are encrypted. Copies of unencrypted backing snapshots remain
-- unencrypted, unless you set @Encrypted@ during the copy operation. You
-- cannot create an unencrypted copy of an encrypted backing snapshot.
--
-- To copy an AMI from a Region to an Outpost, specify the source Region
-- using the __SourceRegion__ parameter, and specify the ARN of the
-- destination Outpost using __DestinationOutpostArn__. Backing snapshots
-- copied to an Outpost are encrypted by default using the default
-- encryption key for the Region, or a different key that you specify in
-- the request using __KmsKeyId__. Outposts do not support unencrypted
-- snapshots. For more information,
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html#ami Amazon EBS local snapshots on Outposts>
-- in the /Amazon EC2 User Guide/.
--
-- For more information about the prerequisites and limits when copying an
-- AMI, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/CopyingAMIs.html Copy an AMI>
-- in the /Amazon EC2 User Guide/.
module Amazonka.EC2.CopyImage
  ( -- * Creating a Request
    CopyImage (..),
    newCopyImage,

    -- * Request Lenses
    copyImage_clientToken,
    copyImage_copyImageTags,
    copyImage_description,
    copyImage_destinationOutpostArn,
    copyImage_dryRun,
    copyImage_encrypted,
    copyImage_kmsKeyId,
    copyImage_name,
    copyImage_sourceImageId,
    copyImage_sourceRegion,

    -- * Destructuring the Response
    CopyImageResponse (..),
    newCopyImageResponse,

    -- * Response Lenses
    copyImageResponse_imageId,
    copyImageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for CopyImage.
--
-- /See:/ 'newCopyImage' smart constructor.
data CopyImage = CopyImage'
  { -- | Unique, case-sensitive identifier you provide to ensure idempotency of
    -- the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring idempotency>
    -- in the /Amazon EC2 API Reference/.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to include your user-defined AMI tags when copying the
    -- AMI.
    --
    -- The following tags will not be copied:
    --
    -- -   System tags (prefixed with @aws:@)
    --
    -- -   For public and shared AMIs, user-defined tags that are attached by
    --     other Amazon Web Services accounts
    --
    -- Default: Your user-defined AMI tags are not copied.
    copyImageTags :: Prelude.Maybe Prelude.Bool,
    -- | A description for the new AMI in the destination Region.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Outpost to which to copy the AMI.
    -- Only specify this parameter when copying an AMI from an Amazon Web
    -- Services Region to an Outpost. The AMI must be in the Region of the
    -- destination Outpost. You cannot copy an AMI from an Outpost to a Region,
    -- from one Outpost to another, or within the same Outpost.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html#copy-amis Copy AMIs from an Amazon Web Services Region to an Outpost>
    -- in the /Amazon EC2 User Guide/.
    destinationOutpostArn :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether the destination snapshots of the copied image should
    -- be encrypted. You can encrypt a copy of an unencrypted snapshot, but you
    -- cannot create an unencrypted copy of an encrypted snapshot. The default
    -- KMS key for Amazon EBS is used unless you specify a non-default Key
    -- Management Service (KMS) KMS key using @KmsKeyId@. For more information,
    -- see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS encryption>
    -- in the /Amazon EC2 User Guide/.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of the symmetric Key Management Service (KMS) KMS key to
    -- use when creating encrypted volumes. If this parameter is not specified,
    -- your Amazon Web Services managed KMS key for Amazon EBS is used. If you
    -- specify a KMS key, you must also set the encrypted state to @true@.
    --
    -- You can specify a KMS key using any of the following:
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
    -- if you specify an identifier that is not valid, the action can appear to
    -- complete, but eventually fails.
    --
    -- The specified KMS key must exist in the destination Region.
    --
    -- Amazon EBS does not support asymmetric KMS keys.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The name of the new AMI in the destination Region.
    name :: Prelude.Text,
    -- | The ID of the AMI to copy.
    sourceImageId :: Prelude.Text,
    -- | The name of the Region that contains the AMI to copy.
    sourceRegion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopyImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'copyImage_clientToken' - Unique, case-sensitive identifier you provide to ensure idempotency of
-- the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring idempotency>
-- in the /Amazon EC2 API Reference/.
--
-- 'copyImageTags', 'copyImage_copyImageTags' - Indicates whether to include your user-defined AMI tags when copying the
-- AMI.
--
-- The following tags will not be copied:
--
-- -   System tags (prefixed with @aws:@)
--
-- -   For public and shared AMIs, user-defined tags that are attached by
--     other Amazon Web Services accounts
--
-- Default: Your user-defined AMI tags are not copied.
--
-- 'description', 'copyImage_description' - A description for the new AMI in the destination Region.
--
-- 'destinationOutpostArn', 'copyImage_destinationOutpostArn' - The Amazon Resource Name (ARN) of the Outpost to which to copy the AMI.
-- Only specify this parameter when copying an AMI from an Amazon Web
-- Services Region to an Outpost. The AMI must be in the Region of the
-- destination Outpost. You cannot copy an AMI from an Outpost to a Region,
-- from one Outpost to another, or within the same Outpost.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html#copy-amis Copy AMIs from an Amazon Web Services Region to an Outpost>
-- in the /Amazon EC2 User Guide/.
--
-- 'dryRun', 'copyImage_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'encrypted', 'copyImage_encrypted' - Specifies whether the destination snapshots of the copied image should
-- be encrypted. You can encrypt a copy of an unencrypted snapshot, but you
-- cannot create an unencrypted copy of an encrypted snapshot. The default
-- KMS key for Amazon EBS is used unless you specify a non-default Key
-- Management Service (KMS) KMS key using @KmsKeyId@. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS encryption>
-- in the /Amazon EC2 User Guide/.
--
-- 'kmsKeyId', 'copyImage_kmsKeyId' - The identifier of the symmetric Key Management Service (KMS) KMS key to
-- use when creating encrypted volumes. If this parameter is not specified,
-- your Amazon Web Services managed KMS key for Amazon EBS is used. If you
-- specify a KMS key, you must also set the encrypted state to @true@.
--
-- You can specify a KMS key using any of the following:
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
-- if you specify an identifier that is not valid, the action can appear to
-- complete, but eventually fails.
--
-- The specified KMS key must exist in the destination Region.
--
-- Amazon EBS does not support asymmetric KMS keys.
--
-- 'name', 'copyImage_name' - The name of the new AMI in the destination Region.
--
-- 'sourceImageId', 'copyImage_sourceImageId' - The ID of the AMI to copy.
--
-- 'sourceRegion', 'copyImage_sourceRegion' - The name of the Region that contains the AMI to copy.
newCopyImage ::
  -- | 'name'
  Prelude.Text ->
  -- | 'sourceImageId'
  Prelude.Text ->
  -- | 'sourceRegion'
  Prelude.Text ->
  CopyImage
newCopyImage pName_ pSourceImageId_ pSourceRegion_ =
  CopyImage'
    { clientToken = Prelude.Nothing,
      copyImageTags = Prelude.Nothing,
      description = Prelude.Nothing,
      destinationOutpostArn = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      name = pName_,
      sourceImageId = pSourceImageId_,
      sourceRegion = pSourceRegion_
    }

-- | Unique, case-sensitive identifier you provide to ensure idempotency of
-- the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring idempotency>
-- in the /Amazon EC2 API Reference/.
copyImage_clientToken :: Lens.Lens' CopyImage (Prelude.Maybe Prelude.Text)
copyImage_clientToken = Lens.lens (\CopyImage' {clientToken} -> clientToken) (\s@CopyImage' {} a -> s {clientToken = a} :: CopyImage)

-- | Indicates whether to include your user-defined AMI tags when copying the
-- AMI.
--
-- The following tags will not be copied:
--
-- -   System tags (prefixed with @aws:@)
--
-- -   For public and shared AMIs, user-defined tags that are attached by
--     other Amazon Web Services accounts
--
-- Default: Your user-defined AMI tags are not copied.
copyImage_copyImageTags :: Lens.Lens' CopyImage (Prelude.Maybe Prelude.Bool)
copyImage_copyImageTags = Lens.lens (\CopyImage' {copyImageTags} -> copyImageTags) (\s@CopyImage' {} a -> s {copyImageTags = a} :: CopyImage)

-- | A description for the new AMI in the destination Region.
copyImage_description :: Lens.Lens' CopyImage (Prelude.Maybe Prelude.Text)
copyImage_description = Lens.lens (\CopyImage' {description} -> description) (\s@CopyImage' {} a -> s {description = a} :: CopyImage)

-- | The Amazon Resource Name (ARN) of the Outpost to which to copy the AMI.
-- Only specify this parameter when copying an AMI from an Amazon Web
-- Services Region to an Outpost. The AMI must be in the Region of the
-- destination Outpost. You cannot copy an AMI from an Outpost to a Region,
-- from one Outpost to another, or within the same Outpost.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html#copy-amis Copy AMIs from an Amazon Web Services Region to an Outpost>
-- in the /Amazon EC2 User Guide/.
copyImage_destinationOutpostArn :: Lens.Lens' CopyImage (Prelude.Maybe Prelude.Text)
copyImage_destinationOutpostArn = Lens.lens (\CopyImage' {destinationOutpostArn} -> destinationOutpostArn) (\s@CopyImage' {} a -> s {destinationOutpostArn = a} :: CopyImage)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
copyImage_dryRun :: Lens.Lens' CopyImage (Prelude.Maybe Prelude.Bool)
copyImage_dryRun = Lens.lens (\CopyImage' {dryRun} -> dryRun) (\s@CopyImage' {} a -> s {dryRun = a} :: CopyImage)

-- | Specifies whether the destination snapshots of the copied image should
-- be encrypted. You can encrypt a copy of an unencrypted snapshot, but you
-- cannot create an unencrypted copy of an encrypted snapshot. The default
-- KMS key for Amazon EBS is used unless you specify a non-default Key
-- Management Service (KMS) KMS key using @KmsKeyId@. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS encryption>
-- in the /Amazon EC2 User Guide/.
copyImage_encrypted :: Lens.Lens' CopyImage (Prelude.Maybe Prelude.Bool)
copyImage_encrypted = Lens.lens (\CopyImage' {encrypted} -> encrypted) (\s@CopyImage' {} a -> s {encrypted = a} :: CopyImage)

-- | The identifier of the symmetric Key Management Service (KMS) KMS key to
-- use when creating encrypted volumes. If this parameter is not specified,
-- your Amazon Web Services managed KMS key for Amazon EBS is used. If you
-- specify a KMS key, you must also set the encrypted state to @true@.
--
-- You can specify a KMS key using any of the following:
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
-- if you specify an identifier that is not valid, the action can appear to
-- complete, but eventually fails.
--
-- The specified KMS key must exist in the destination Region.
--
-- Amazon EBS does not support asymmetric KMS keys.
copyImage_kmsKeyId :: Lens.Lens' CopyImage (Prelude.Maybe Prelude.Text)
copyImage_kmsKeyId = Lens.lens (\CopyImage' {kmsKeyId} -> kmsKeyId) (\s@CopyImage' {} a -> s {kmsKeyId = a} :: CopyImage)

-- | The name of the new AMI in the destination Region.
copyImage_name :: Lens.Lens' CopyImage Prelude.Text
copyImage_name = Lens.lens (\CopyImage' {name} -> name) (\s@CopyImage' {} a -> s {name = a} :: CopyImage)

-- | The ID of the AMI to copy.
copyImage_sourceImageId :: Lens.Lens' CopyImage Prelude.Text
copyImage_sourceImageId = Lens.lens (\CopyImage' {sourceImageId} -> sourceImageId) (\s@CopyImage' {} a -> s {sourceImageId = a} :: CopyImage)

-- | The name of the Region that contains the AMI to copy.
copyImage_sourceRegion :: Lens.Lens' CopyImage Prelude.Text
copyImage_sourceRegion = Lens.lens (\CopyImage' {sourceRegion} -> sourceRegion) (\s@CopyImage' {} a -> s {sourceRegion = a} :: CopyImage)

instance Core.AWSRequest CopyImage where
  type AWSResponse CopyImage = CopyImageResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CopyImageResponse'
            Prelude.<$> (x Data..@? "imageId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CopyImage where
  hashWithSalt _salt CopyImage' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` copyImageTags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` destinationOutpostArn
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` sourceImageId
      `Prelude.hashWithSalt` sourceRegion

instance Prelude.NFData CopyImage where
  rnf CopyImage' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf copyImageTags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf destinationOutpostArn
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf sourceImageId
      `Prelude.seq` Prelude.rnf sourceRegion

instance Data.ToHeaders CopyImage where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CopyImage where
  toPath = Prelude.const "/"

instance Data.ToQuery CopyImage where
  toQuery CopyImage' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CopyImage" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Data.=: clientToken,
        "CopyImageTags" Data.=: copyImageTags,
        "Description" Data.=: description,
        "DestinationOutpostArn"
          Data.=: destinationOutpostArn,
        "DryRun" Data.=: dryRun,
        "Encrypted" Data.=: encrypted,
        "KmsKeyId" Data.=: kmsKeyId,
        "Name" Data.=: name,
        "SourceImageId" Data.=: sourceImageId,
        "SourceRegion" Data.=: sourceRegion
      ]

-- | Contains the output of CopyImage.
--
-- /See:/ 'newCopyImageResponse' smart constructor.
data CopyImageResponse = CopyImageResponse'
  { -- | The ID of the new AMI.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopyImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageId', 'copyImageResponse_imageId' - The ID of the new AMI.
--
-- 'httpStatus', 'copyImageResponse_httpStatus' - The response's http status code.
newCopyImageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CopyImageResponse
newCopyImageResponse pHttpStatus_ =
  CopyImageResponse'
    { imageId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the new AMI.
copyImageResponse_imageId :: Lens.Lens' CopyImageResponse (Prelude.Maybe Prelude.Text)
copyImageResponse_imageId = Lens.lens (\CopyImageResponse' {imageId} -> imageId) (\s@CopyImageResponse' {} a -> s {imageId = a} :: CopyImageResponse)

-- | The response's http status code.
copyImageResponse_httpStatus :: Lens.Lens' CopyImageResponse Prelude.Int
copyImageResponse_httpStatus = Lens.lens (\CopyImageResponse' {httpStatus} -> httpStatus) (\s@CopyImageResponse' {} a -> s {httpStatus = a} :: CopyImageResponse)

instance Prelude.NFData CopyImageResponse where
  rnf CopyImageResponse' {..} =
    Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf httpStatus
