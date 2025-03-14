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
-- Module      : Amazonka.KMS.GetKeyRotationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a Boolean value that indicates whether
-- <https://docs.aws.amazon.com/kms/latest/developerguide/rotate-keys.html automatic rotation of the key material>
-- is enabled for the specified KMS key.
--
-- When you enable automatic rotation for
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#customer-cmk customer managed KMS keys>,
-- KMS rotates the key material of the KMS key one year (approximately 365
-- days) from the enable date and every year thereafter. You can monitor
-- rotation of the key material for your KMS keys in CloudTrail and Amazon
-- CloudWatch.
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
-- set the property on the primary key..
--
-- You can enable (EnableKeyRotation) and disable automatic rotation
-- (DisableKeyRotation) of the key material in customer managed KMS keys.
-- Key material rotation of
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#aws-managed-cmk Amazon Web Services managed KMS keys>
-- is not configurable. KMS always rotates the key material in Amazon Web
-- Services managed KMS keys every year. The key rotation status for Amazon
-- Web Services managed KMS keys is always @true@.
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
-- -   Disabled: The key rotation status does not change when you disable a
--     KMS key. However, while the KMS key is disabled, KMS does not rotate
--     the key material. When you re-enable the KMS key, rotation resumes.
--     If the key material in the re-enabled KMS key hasn\'t been rotated
--     in one year, KMS rotates it immediately, and every year thereafter.
--     If it\'s been less than a year since the key material in the
--     re-enabled KMS key was rotated, the KMS key resumes its prior
--     rotation schedule.
--
-- -   Pending deletion: While a KMS key is pending deletion, its key
--     rotation status is @false@ and KMS does not rotate the key material.
--     If you cancel the deletion, the original key rotation status returns
--     to @true@.
--
-- __Cross-account use__: Yes. To perform this operation on a KMS key in a
-- different Amazon Web Services account, specify the key ARN in the value
-- of the @KeyId@ parameter.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:GetKeyRotationStatus>
-- (key policy)
--
-- __Related operations:__
--
-- -   DisableKeyRotation
--
-- -   EnableKeyRotation
module Amazonka.KMS.GetKeyRotationStatus
  ( -- * Creating a Request
    GetKeyRotationStatus (..),
    newGetKeyRotationStatus,

    -- * Request Lenses
    getKeyRotationStatus_keyId,

    -- * Destructuring the Response
    GetKeyRotationStatusResponse (..),
    newGetKeyRotationStatusResponse,

    -- * Response Lenses
    getKeyRotationStatusResponse_keyRotationEnabled,
    getKeyRotationStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetKeyRotationStatus' smart constructor.
data GetKeyRotationStatus = GetKeyRotationStatus'
  { -- | Gets the rotation status for the specified KMS key.
    --
    -- Specify the key ID or key ARN of the KMS key. To specify a KMS key in a
    -- different Amazon Web Services account, you must use the key ARN.
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
-- Create a value of 'GetKeyRotationStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyId', 'getKeyRotationStatus_keyId' - Gets the rotation status for the specified KMS key.
--
-- Specify the key ID or key ARN of the KMS key. To specify a KMS key in a
-- different Amazon Web Services account, you must use the key ARN.
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
newGetKeyRotationStatus ::
  -- | 'keyId'
  Prelude.Text ->
  GetKeyRotationStatus
newGetKeyRotationStatus pKeyId_ =
  GetKeyRotationStatus' {keyId = pKeyId_}

-- | Gets the rotation status for the specified KMS key.
--
-- Specify the key ID or key ARN of the KMS key. To specify a KMS key in a
-- different Amazon Web Services account, you must use the key ARN.
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
getKeyRotationStatus_keyId :: Lens.Lens' GetKeyRotationStatus Prelude.Text
getKeyRotationStatus_keyId = Lens.lens (\GetKeyRotationStatus' {keyId} -> keyId) (\s@GetKeyRotationStatus' {} a -> s {keyId = a} :: GetKeyRotationStatus)

instance Core.AWSRequest GetKeyRotationStatus where
  type
    AWSResponse GetKeyRotationStatus =
      GetKeyRotationStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetKeyRotationStatusResponse'
            Prelude.<$> (x Data..?> "KeyRotationEnabled")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetKeyRotationStatus where
  hashWithSalt _salt GetKeyRotationStatus' {..} =
    _salt `Prelude.hashWithSalt` keyId

instance Prelude.NFData GetKeyRotationStatus where
  rnf GetKeyRotationStatus' {..} = Prelude.rnf keyId

instance Data.ToHeaders GetKeyRotationStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TrentService.GetKeyRotationStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetKeyRotationStatus where
  toJSON GetKeyRotationStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("KeyId" Data..= keyId)]
      )

instance Data.ToPath GetKeyRotationStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery GetKeyRotationStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetKeyRotationStatusResponse' smart constructor.
data GetKeyRotationStatusResponse = GetKeyRotationStatusResponse'
  { -- | A Boolean value that specifies whether key rotation is enabled.
    keyRotationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetKeyRotationStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyRotationEnabled', 'getKeyRotationStatusResponse_keyRotationEnabled' - A Boolean value that specifies whether key rotation is enabled.
--
-- 'httpStatus', 'getKeyRotationStatusResponse_httpStatus' - The response's http status code.
newGetKeyRotationStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetKeyRotationStatusResponse
newGetKeyRotationStatusResponse pHttpStatus_ =
  GetKeyRotationStatusResponse'
    { keyRotationEnabled =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A Boolean value that specifies whether key rotation is enabled.
getKeyRotationStatusResponse_keyRotationEnabled :: Lens.Lens' GetKeyRotationStatusResponse (Prelude.Maybe Prelude.Bool)
getKeyRotationStatusResponse_keyRotationEnabled = Lens.lens (\GetKeyRotationStatusResponse' {keyRotationEnabled} -> keyRotationEnabled) (\s@GetKeyRotationStatusResponse' {} a -> s {keyRotationEnabled = a} :: GetKeyRotationStatusResponse)

-- | The response's http status code.
getKeyRotationStatusResponse_httpStatus :: Lens.Lens' GetKeyRotationStatusResponse Prelude.Int
getKeyRotationStatusResponse_httpStatus = Lens.lens (\GetKeyRotationStatusResponse' {httpStatus} -> httpStatus) (\s@GetKeyRotationStatusResponse' {} a -> s {httpStatus = a} :: GetKeyRotationStatusResponse)

instance Prelude.NFData GetKeyRotationStatusResponse where
  rnf GetKeyRotationStatusResponse' {..} =
    Prelude.rnf keyRotationEnabled `Prelude.seq`
      Prelude.rnf httpStatus
