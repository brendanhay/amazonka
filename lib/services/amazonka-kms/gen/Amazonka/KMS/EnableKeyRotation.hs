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
-- Module      : Amazonka.KMS.EnableKeyRotation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables
-- <https://docs.aws.amazon.com/kms/latest/developerguide/rotate-keys.html automatic rotation of the key material>
-- of the specified symmetric encryption KMS key.
--
-- When you enable automatic rotation of
-- a<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#customer-cmk customer managed KMS key>,
-- KMS rotates the key material of the KMS key one year (approximately 365
-- days) from the enable date and every year thereafter. You can monitor
-- rotation of the key material for your KMS keys in CloudTrail and Amazon
-- CloudWatch. To disable rotation of the key material in a customer
-- managed KMS key, use the DisableKeyRotation operation.
--
-- Automatic key rotation is supported only on
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#symmetric-cmks symmetric encryption KMS keys>.
-- You cannot enable automatic rotation of
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html asymmetric KMS keys>,
-- <https://docs.aws.amazon.com/kms/latest/developerguide/hmac.html HMAC KMS keys>,
-- KMS keys with
-- <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html imported key material>,
-- or KMS keys in a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>.
-- To enable or disable automatic rotation of a set of related
-- <https://docs.aws.amazon.com/kms/latest/developerguide/multi-region-keys-manage.html#multi-region-rotate multi-Region keys>,
-- set the property on the primary key.
--
-- You cannot enable or disable automatic rotation
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#aws-managed-cmk Amazon Web Services managed KMS keys>.
-- KMS always rotates the key material of Amazon Web Services managed keys
-- every year. Rotation of
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#aws-owned-cmk Amazon Web Services owned KMS keys>
-- varies.
--
-- In May 2022, KMS changed the rotation schedule for Amazon Web Services
-- managed keys from every three years (approximately 1,095 days) to every
-- year (approximately 365 days).
--
-- New Amazon Web Services managed keys are automatically rotated one year
-- after they are created, and approximately every year thereafter.
--
-- Existing Amazon Web Services managed keys are automatically rotated one
-- year after their most recent rotation, and every year thereafter.
--
-- The KMS key that you use for this operation must be in a compatible key
-- state. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key states of KMS keys>
-- in the /Key Management Service Developer Guide/.
--
-- __Cross-account use__: No. You cannot perform this operation on a KMS
-- key in a different Amazon Web Services account.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:EnableKeyRotation>
-- (key policy)
--
-- __Related operations:__
--
-- -   DisableKeyRotation
--
-- -   GetKeyRotationStatus
module Amazonka.KMS.EnableKeyRotation
  ( -- * Creating a Request
    EnableKeyRotation (..),
    newEnableKeyRotation,

    -- * Request Lenses
    enableKeyRotation_keyId,

    -- * Destructuring the Response
    EnableKeyRotationResponse (..),
    newEnableKeyRotationResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEnableKeyRotation' smart constructor.
data EnableKeyRotation = EnableKeyRotation'
  { -- | Identifies a symmetric encryption KMS key. You cannot enable automatic
    -- rotation of
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html asymmetric KMS keys>,
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/hmac.html HMAC KMS keys>,
    -- KMS keys with
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html imported key material>,
    -- or KMS keys in a
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>.
    -- To enable or disable automatic rotation of a set of related
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/multi-region-keys-manage.html#multi-region-rotate multi-Region keys>,
    -- set the property on the primary key.
    --
    -- Specify the key ID or key ARN of the KMS key.
    --
    -- For example:
    --
    -- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Key ARN:
    --     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- To get the key ID and key ARN for a KMS key, use ListKeys or
    -- DescribeKey.
    keyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableKeyRotation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyId', 'enableKeyRotation_keyId' - Identifies a symmetric encryption KMS key. You cannot enable automatic
-- rotation of
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html asymmetric KMS keys>,
-- <https://docs.aws.amazon.com/kms/latest/developerguide/hmac.html HMAC KMS keys>,
-- KMS keys with
-- <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html imported key material>,
-- or KMS keys in a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>.
-- To enable or disable automatic rotation of a set of related
-- <https://docs.aws.amazon.com/kms/latest/developerguide/multi-region-keys-manage.html#multi-region-rotate multi-Region keys>,
-- set the property on the primary key.
--
-- Specify the key ID or key ARN of the KMS key.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- To get the key ID and key ARN for a KMS key, use ListKeys or
-- DescribeKey.
newEnableKeyRotation ::
  -- | 'keyId'
  Prelude.Text ->
  EnableKeyRotation
newEnableKeyRotation pKeyId_ =
  EnableKeyRotation' {keyId = pKeyId_}

-- | Identifies a symmetric encryption KMS key. You cannot enable automatic
-- rotation of
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html asymmetric KMS keys>,
-- <https://docs.aws.amazon.com/kms/latest/developerguide/hmac.html HMAC KMS keys>,
-- KMS keys with
-- <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html imported key material>,
-- or KMS keys in a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>.
-- To enable or disable automatic rotation of a set of related
-- <https://docs.aws.amazon.com/kms/latest/developerguide/multi-region-keys-manage.html#multi-region-rotate multi-Region keys>,
-- set the property on the primary key.
--
-- Specify the key ID or key ARN of the KMS key.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- To get the key ID and key ARN for a KMS key, use ListKeys or
-- DescribeKey.
enableKeyRotation_keyId :: Lens.Lens' EnableKeyRotation Prelude.Text
enableKeyRotation_keyId = Lens.lens (\EnableKeyRotation' {keyId} -> keyId) (\s@EnableKeyRotation' {} a -> s {keyId = a} :: EnableKeyRotation)

instance Core.AWSRequest EnableKeyRotation where
  type
    AWSResponse EnableKeyRotation =
      EnableKeyRotationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull EnableKeyRotationResponse'

instance Prelude.Hashable EnableKeyRotation where
  hashWithSalt _salt EnableKeyRotation' {..} =
    _salt `Prelude.hashWithSalt` keyId

instance Prelude.NFData EnableKeyRotation where
  rnf EnableKeyRotation' {..} = Prelude.rnf keyId

instance Data.ToHeaders EnableKeyRotation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TrentService.EnableKeyRotation" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON EnableKeyRotation where
  toJSON EnableKeyRotation' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("KeyId" Data..= keyId)]
      )

instance Data.ToPath EnableKeyRotation where
  toPath = Prelude.const "/"

instance Data.ToQuery EnableKeyRotation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEnableKeyRotationResponse' smart constructor.
data EnableKeyRotationResponse = EnableKeyRotationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableKeyRotationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newEnableKeyRotationResponse ::
  EnableKeyRotationResponse
newEnableKeyRotationResponse =
  EnableKeyRotationResponse'

instance Prelude.NFData EnableKeyRotationResponse where
  rnf _ = ()
