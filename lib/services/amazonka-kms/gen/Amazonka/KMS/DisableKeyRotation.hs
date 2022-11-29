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
-- Module      : Amazonka.KMS.DisableKeyRotation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables
-- <https://docs.aws.amazon.com/kms/latest/developerguide/rotate-keys.html automatic rotation of the key material>
-- of the specified symmetric encryption KMS key.
--
-- Automatic key rotation is supported only on symmetric encryption KMS
-- keys. You cannot enable or disable automatic rotation of
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html asymmetric KMS keys>,
-- <https://docs.aws.amazon.com/kms/latest/developerguide/hmac.html HMAC KMS keys>,
-- KMS keys with
-- <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html imported key material>,
-- or KMS keys in a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>.
-- The key rotation status of these KMS keys is always @false@. To enable
-- or disable automatic rotation of a set of related
-- <https://docs.aws.amazon.com/kms/latest/developerguide/multi-region-keys-manage.html#multi-region-rotate multi-Region keys>,
-- set the property on the primary key.
--
-- You can enable (EnableKeyRotation) and disable automatic rotation of the
-- key material in
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#customer-cmk customer managed KMS keys>.
-- Key material rotation of
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#aws-managed-cmk Amazon Web Services managed KMS keys>
-- is not configurable. KMS always rotates the key material for every year.
-- Rotation of
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#aws-owned-cmk Amazon Web Services owned KMS keys>
-- varies.
--
-- In May 2022, KMS changed the rotation schedule for Amazon Web Services
-- managed keys from every three years to every year. For details, see
-- EnableKeyRotation.
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
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:DisableKeyRotation>
-- (key policy)
--
-- __Related operations:__
--
-- -   EnableKeyRotation
--
-- -   GetKeyRotationStatus
module Amazonka.KMS.DisableKeyRotation
  ( -- * Creating a Request
    DisableKeyRotation (..),
    newDisableKeyRotation,

    -- * Request Lenses
    disableKeyRotation_keyId,

    -- * Destructuring the Response
    DisableKeyRotationResponse (..),
    newDisableKeyRotationResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisableKeyRotation' smart constructor.
data DisableKeyRotation = DisableKeyRotation'
  { -- | Identifies a symmetric encryption KMS key. You cannot enable or disable
    -- automatic rotation of
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html#asymmetric-cmks asymmetric KMS keys>,
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/hmac.html HMAC KMS keys>,
    -- KMS keys with
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html imported key material>,
    -- or KMS keys in a
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>.
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
-- Create a value of 'DisableKeyRotation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyId', 'disableKeyRotation_keyId' - Identifies a symmetric encryption KMS key. You cannot enable or disable
-- automatic rotation of
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html#asymmetric-cmks asymmetric KMS keys>,
-- <https://docs.aws.amazon.com/kms/latest/developerguide/hmac.html HMAC KMS keys>,
-- KMS keys with
-- <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html imported key material>,
-- or KMS keys in a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>.
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
newDisableKeyRotation ::
  -- | 'keyId'
  Prelude.Text ->
  DisableKeyRotation
newDisableKeyRotation pKeyId_ =
  DisableKeyRotation' {keyId = pKeyId_}

-- | Identifies a symmetric encryption KMS key. You cannot enable or disable
-- automatic rotation of
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html#asymmetric-cmks asymmetric KMS keys>,
-- <https://docs.aws.amazon.com/kms/latest/developerguide/hmac.html HMAC KMS keys>,
-- KMS keys with
-- <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html imported key material>,
-- or KMS keys in a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>.
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
disableKeyRotation_keyId :: Lens.Lens' DisableKeyRotation Prelude.Text
disableKeyRotation_keyId = Lens.lens (\DisableKeyRotation' {keyId} -> keyId) (\s@DisableKeyRotation' {} a -> s {keyId = a} :: DisableKeyRotation)

instance Core.AWSRequest DisableKeyRotation where
  type
    AWSResponse DisableKeyRotation =
      DisableKeyRotationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DisableKeyRotationResponse'

instance Prelude.Hashable DisableKeyRotation where
  hashWithSalt _salt DisableKeyRotation' {..} =
    _salt `Prelude.hashWithSalt` keyId

instance Prelude.NFData DisableKeyRotation where
  rnf DisableKeyRotation' {..} = Prelude.rnf keyId

instance Core.ToHeaders DisableKeyRotation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "TrentService.DisableKeyRotation" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DisableKeyRotation where
  toJSON DisableKeyRotation' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("KeyId" Core..= keyId)]
      )

instance Core.ToPath DisableKeyRotation where
  toPath = Prelude.const "/"

instance Core.ToQuery DisableKeyRotation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisableKeyRotationResponse' smart constructor.
data DisableKeyRotationResponse = DisableKeyRotationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableKeyRotationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisableKeyRotationResponse ::
  DisableKeyRotationResponse
newDisableKeyRotationResponse =
  DisableKeyRotationResponse'

instance Prelude.NFData DisableKeyRotationResponse where
  rnf _ = ()
