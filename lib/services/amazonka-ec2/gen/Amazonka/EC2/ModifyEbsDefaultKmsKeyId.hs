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
-- Module      : Amazonka.EC2.ModifyEbsDefaultKmsKeyId
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the default KMS key for EBS encryption by default for your
-- account in this Region.
--
-- Amazon Web Services creates a unique Amazon Web Services managed KMS key
-- in each Region for use with encryption by default. If you change the
-- default KMS key to a symmetric customer managed KMS key, it is used
-- instead of the Amazon Web Services managed KMS key. To reset the default
-- KMS key to the Amazon Web Services managed KMS key for EBS, use
-- ResetEbsDefaultKmsKeyId. Amazon EBS does not support asymmetric KMS
-- keys.
--
-- If you delete or disable the customer managed KMS key that you specified
-- for use with encryption by default, your instances will fail to launch.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.EC2.ModifyEbsDefaultKmsKeyId
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyEbsDefaultKmsKeyId' smart constructor.
data ModifyEbsDefaultKmsKeyId = ModifyEbsDefaultKmsKeyId'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
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
    --
    -- Amazon EBS does not support asymmetric KMS keys.
    kmsKeyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'kmsKeyId', 'modifyEbsDefaultKmsKeyId_kmsKeyId' - The identifier of the Key Management Service (KMS) KMS key to use for
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
-- Amazon EBS does not support asymmetric KMS keys.
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
--
-- Amazon EBS does not support asymmetric KMS keys.
modifyEbsDefaultKmsKeyId_kmsKeyId :: Lens.Lens' ModifyEbsDefaultKmsKeyId Prelude.Text
modifyEbsDefaultKmsKeyId_kmsKeyId = Lens.lens (\ModifyEbsDefaultKmsKeyId' {kmsKeyId} -> kmsKeyId) (\s@ModifyEbsDefaultKmsKeyId' {} a -> s {kmsKeyId = a} :: ModifyEbsDefaultKmsKeyId)

instance Core.AWSRequest ModifyEbsDefaultKmsKeyId where
  type
    AWSResponse ModifyEbsDefaultKmsKeyId =
      ModifyEbsDefaultKmsKeyIdResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyEbsDefaultKmsKeyIdResponse'
            Prelude.<$> (x Data..@? "kmsKeyId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyEbsDefaultKmsKeyId where
  hashWithSalt _salt ModifyEbsDefaultKmsKeyId' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` kmsKeyId

instance Prelude.NFData ModifyEbsDefaultKmsKeyId where
  rnf ModifyEbsDefaultKmsKeyId' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf kmsKeyId

instance Data.ToHeaders ModifyEbsDefaultKmsKeyId where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyEbsDefaultKmsKeyId where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyEbsDefaultKmsKeyId where
  toQuery ModifyEbsDefaultKmsKeyId' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyEbsDefaultKmsKeyId" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "KmsKeyId" Data.=: kmsKeyId
      ]

-- | /See:/ 'newModifyEbsDefaultKmsKeyIdResponse' smart constructor.
data ModifyEbsDefaultKmsKeyIdResponse = ModifyEbsDefaultKmsKeyIdResponse'
  { -- | The Amazon Resource Name (ARN) of the default KMS key for encryption by
    -- default.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyEbsDefaultKmsKeyIdResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'modifyEbsDefaultKmsKeyIdResponse_kmsKeyId' - The Amazon Resource Name (ARN) of the default KMS key for encryption by
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

-- | The Amazon Resource Name (ARN) of the default KMS key for encryption by
-- default.
modifyEbsDefaultKmsKeyIdResponse_kmsKeyId :: Lens.Lens' ModifyEbsDefaultKmsKeyIdResponse (Prelude.Maybe Prelude.Text)
modifyEbsDefaultKmsKeyIdResponse_kmsKeyId = Lens.lens (\ModifyEbsDefaultKmsKeyIdResponse' {kmsKeyId} -> kmsKeyId) (\s@ModifyEbsDefaultKmsKeyIdResponse' {} a -> s {kmsKeyId = a} :: ModifyEbsDefaultKmsKeyIdResponse)

-- | The response's http status code.
modifyEbsDefaultKmsKeyIdResponse_httpStatus :: Lens.Lens' ModifyEbsDefaultKmsKeyIdResponse Prelude.Int
modifyEbsDefaultKmsKeyIdResponse_httpStatus = Lens.lens (\ModifyEbsDefaultKmsKeyIdResponse' {httpStatus} -> httpStatus) (\s@ModifyEbsDefaultKmsKeyIdResponse' {} a -> s {httpStatus = a} :: ModifyEbsDefaultKmsKeyIdResponse)

instance
  Prelude.NFData
    ModifyEbsDefaultKmsKeyIdResponse
  where
  rnf ModifyEbsDefaultKmsKeyIdResponse' {..} =
    Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf httpStatus
