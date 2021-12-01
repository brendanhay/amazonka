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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a Boolean value that indicates whether
-- <https://docs.aws.amazon.com/kms/latest/developerguide/rotate-keys.html automatic rotation of the key material>
-- is enabled for the specified KMS key.
--
-- You cannot enable automatic rotation of
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symm-asymm-concepts.html#asymmetric-cmks asymmetric KMS keys>,
-- KMS keys with
-- <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html imported key material>,
-- or KMS keys in a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>.
-- To enable or disable automatic rotation of a set of related
-- <https://docs.aws.amazon.com/kms/latest/developerguide/multi-region-keys-overview.html#mrk-replica-key multi-Region keys>,
-- set the property on the primary key. The key rotation status for these
-- KMS keys is always @false@.
--
-- The KMS key that you use for this operation must be in a compatible key
-- state. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key state: Effect on your KMS key>
-- in the /Key Management Service Developer Guide/.
--
-- -   Disabled: The key rotation status does not change when you disable a
--     KMS key. However, while the KMS key is disabled, KMS does not rotate
--     the key material.
--
-- -   Pending deletion: While a KMS key is pending deletion, its key
--     rotation status is @false@ and KMS does not rotate the key material.
--     If you cancel the deletion, the original key rotation status is
--     restored.
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
import Amazonka.KMS.Types
import qualified Amazonka.Lens as Lens
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetKeyRotationStatusResponse'
            Prelude.<$> (x Core..?> "KeyRotationEnabled")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetKeyRotationStatus where
  hashWithSalt salt' GetKeyRotationStatus' {..} =
    salt' `Prelude.hashWithSalt` keyId

instance Prelude.NFData GetKeyRotationStatus where
  rnf GetKeyRotationStatus' {..} = Prelude.rnf keyId

instance Core.ToHeaders GetKeyRotationStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "TrentService.GetKeyRotationStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetKeyRotationStatus where
  toJSON GetKeyRotationStatus' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("KeyId" Core..= keyId)]
      )

instance Core.ToPath GetKeyRotationStatus where
  toPath = Prelude.const "/"

instance Core.ToQuery GetKeyRotationStatus where
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
    Prelude.rnf keyRotationEnabled
      `Prelude.seq` Prelude.rnf httpStatus
