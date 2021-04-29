{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.ModifyEbsDefaultKmsKeyId
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the default customer master key (CMK) for EBS encryption by
-- default for your account in this Region.
--
-- AWS creates a unique AWS managed CMK in each Region for use with
-- encryption by default. If you change the default CMK to a symmetric
-- customer managed CMK, it is used instead of the AWS managed CMK. To
-- reset the default CMK to the AWS managed CMK for EBS, use
-- ResetEbsDefaultKmsKeyId. Amazon EBS does not support asymmetric CMKs.
--
-- If you delete or disable the customer managed CMK that you specified for
-- use with encryption by default, your instances will fail to launch.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Network.AWS.EC2.ModifyEbsDefaultKmsKeyId
  ( -- * Creating a Request
    ModifyEbsDefaultKmsKeyId (..),
    newModifyEbsDefaultKmsKeyId,

    -- * Request Lenses
    modifyEbsDefaultKmsKeyId_dryRun,
    modifyEbsDefaultKmsKeyId_kmsKeyId,

    -- * Destructuring the Response
    ModifyEbsDefaultKmsKeyIdResponse (..),
    newModifyEbsDefaultKmsKeyIdResponse,

    -- * Response Lenses
    modifyEbsDefaultKmsKeyIdResponse_kmsKeyId,
    modifyEbsDefaultKmsKeyIdResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyEbsDefaultKmsKeyId' smart constructor.
data ModifyEbsDefaultKmsKeyId = ModifyEbsDefaultKmsKeyId'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of the AWS Key Management Service (AWS KMS) customer
    -- master key (CMK) to use for Amazon EBS encryption. If this parameter is
    -- not specified, your AWS managed CMK for EBS is used. If @KmsKeyId@ is
    -- specified, the encrypted state must be @true@.
    --
    -- You can specify the CMK using any of the following:
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
    -- AWS authenticates the CMK asynchronously. Therefore, if you specify an
    -- ID, alias, or ARN that is not valid, the action can appear to complete,
    -- but eventually fails.
    --
    -- Amazon EBS does not support asymmetric CMKs.
    kmsKeyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyEbsDefaultKmsKeyId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'modifyEbsDefaultKmsKeyId_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'kmsKeyId', 'modifyEbsDefaultKmsKeyId_kmsKeyId' - The identifier of the AWS Key Management Service (AWS KMS) customer
-- master key (CMK) to use for Amazon EBS encryption. If this parameter is
-- not specified, your AWS managed CMK for EBS is used. If @KmsKeyId@ is
-- specified, the encrypted state must be @true@.
--
-- You can specify the CMK using any of the following:
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
-- AWS authenticates the CMK asynchronously. Therefore, if you specify an
-- ID, alias, or ARN that is not valid, the action can appear to complete,
-- but eventually fails.
--
-- Amazon EBS does not support asymmetric CMKs.
newModifyEbsDefaultKmsKeyId ::
  -- | 'kmsKeyId'
  Prelude.Text ->
  ModifyEbsDefaultKmsKeyId
newModifyEbsDefaultKmsKeyId pKmsKeyId_ =
  ModifyEbsDefaultKmsKeyId'
    { dryRun = Prelude.Nothing,
      kmsKeyId = pKmsKeyId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyEbsDefaultKmsKeyId_dryRun :: Lens.Lens' ModifyEbsDefaultKmsKeyId (Prelude.Maybe Prelude.Bool)
modifyEbsDefaultKmsKeyId_dryRun = Lens.lens (\ModifyEbsDefaultKmsKeyId' {dryRun} -> dryRun) (\s@ModifyEbsDefaultKmsKeyId' {} a -> s {dryRun = a} :: ModifyEbsDefaultKmsKeyId)

-- | The identifier of the AWS Key Management Service (AWS KMS) customer
-- master key (CMK) to use for Amazon EBS encryption. If this parameter is
-- not specified, your AWS managed CMK for EBS is used. If @KmsKeyId@ is
-- specified, the encrypted state must be @true@.
--
-- You can specify the CMK using any of the following:
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
-- AWS authenticates the CMK asynchronously. Therefore, if you specify an
-- ID, alias, or ARN that is not valid, the action can appear to complete,
-- but eventually fails.
--
-- Amazon EBS does not support asymmetric CMKs.
modifyEbsDefaultKmsKeyId_kmsKeyId :: Lens.Lens' ModifyEbsDefaultKmsKeyId Prelude.Text
modifyEbsDefaultKmsKeyId_kmsKeyId = Lens.lens (\ModifyEbsDefaultKmsKeyId' {kmsKeyId} -> kmsKeyId) (\s@ModifyEbsDefaultKmsKeyId' {} a -> s {kmsKeyId = a} :: ModifyEbsDefaultKmsKeyId)

instance Prelude.AWSRequest ModifyEbsDefaultKmsKeyId where
  type
    Rs ModifyEbsDefaultKmsKeyId =
      ModifyEbsDefaultKmsKeyIdResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyEbsDefaultKmsKeyIdResponse'
            Prelude.<$> (x Prelude..@? "kmsKeyId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyEbsDefaultKmsKeyId

instance Prelude.NFData ModifyEbsDefaultKmsKeyId

instance Prelude.ToHeaders ModifyEbsDefaultKmsKeyId where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ModifyEbsDefaultKmsKeyId where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ModifyEbsDefaultKmsKeyId where
  toQuery ModifyEbsDefaultKmsKeyId' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ModifyEbsDefaultKmsKeyId" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "KmsKeyId" Prelude.=: kmsKeyId
      ]

-- | /See:/ 'newModifyEbsDefaultKmsKeyIdResponse' smart constructor.
data ModifyEbsDefaultKmsKeyIdResponse = ModifyEbsDefaultKmsKeyIdResponse'
  { -- | The Amazon Resource Name (ARN) of the default CMK for encryption by
    -- default.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyEbsDefaultKmsKeyIdResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'modifyEbsDefaultKmsKeyIdResponse_kmsKeyId' - The Amazon Resource Name (ARN) of the default CMK for encryption by
-- default.
--
-- 'httpStatus', 'modifyEbsDefaultKmsKeyIdResponse_httpStatus' - The response's http status code.
newModifyEbsDefaultKmsKeyIdResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyEbsDefaultKmsKeyIdResponse
newModifyEbsDefaultKmsKeyIdResponse pHttpStatus_ =
  ModifyEbsDefaultKmsKeyIdResponse'
    { kmsKeyId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the default CMK for encryption by
-- default.
modifyEbsDefaultKmsKeyIdResponse_kmsKeyId :: Lens.Lens' ModifyEbsDefaultKmsKeyIdResponse (Prelude.Maybe Prelude.Text)
modifyEbsDefaultKmsKeyIdResponse_kmsKeyId = Lens.lens (\ModifyEbsDefaultKmsKeyIdResponse' {kmsKeyId} -> kmsKeyId) (\s@ModifyEbsDefaultKmsKeyIdResponse' {} a -> s {kmsKeyId = a} :: ModifyEbsDefaultKmsKeyIdResponse)

-- | The response's http status code.
modifyEbsDefaultKmsKeyIdResponse_httpStatus :: Lens.Lens' ModifyEbsDefaultKmsKeyIdResponse Prelude.Int
modifyEbsDefaultKmsKeyIdResponse_httpStatus = Lens.lens (\ModifyEbsDefaultKmsKeyIdResponse' {httpStatus} -> httpStatus) (\s@ModifyEbsDefaultKmsKeyIdResponse' {} a -> s {httpStatus = a} :: ModifyEbsDefaultKmsKeyIdResponse)

instance
  Prelude.NFData
    ModifyEbsDefaultKmsKeyIdResponse
