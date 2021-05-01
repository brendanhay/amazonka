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
-- Module      : Network.AWS.KMS.GetKeyRotationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a Boolean value that indicates whether
-- <https://docs.aws.amazon.com/kms/latest/developerguide/rotate-keys.html automatic rotation of the key material>
-- is enabled for the specified customer master key (CMK).
--
-- You cannot enable automatic rotation of asymmetric CMKs, CMKs with
-- imported key material, or CMKs in a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>.
-- The key rotation status for these CMKs is always @false@.
--
-- The CMK that you use for this operation must be in a compatible key
-- state. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key>
-- in the /AWS Key Management Service Developer Guide/.
--
-- -   Disabled: The key rotation status does not change when you disable a
--     CMK. However, while the CMK is disabled, AWS KMS does not rotate the
--     backing key.
--
-- -   Pending deletion: While a CMK is pending deletion, its key rotation
--     status is @false@ and AWS KMS does not rotate the backing key. If
--     you cancel the deletion, the original key rotation status is
--     restored.
--
-- __Cross-account use__: Yes. To perform this operation on a CMK in a
-- different AWS account, specify the key ARN in the value of the @KeyId@
-- parameter.
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
module Network.AWS.KMS.GetKeyRotationStatus
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

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetKeyRotationStatus' smart constructor.
data GetKeyRotationStatus = GetKeyRotationStatus'
  { -- | A unique identifier for the customer master key (CMK).
    --
    -- Specify the key ID or the Amazon Resource Name (ARN) of the CMK. To
    -- specify a CMK in a different AWS account, you must use the key ARN.
    --
    -- For example:
    --
    -- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Key ARN:
    --     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- To get the key ID and key ARN for a CMK, use ListKeys or DescribeKey.
    keyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetKeyRotationStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyId', 'getKeyRotationStatus_keyId' - A unique identifier for the customer master key (CMK).
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK. To
-- specify a CMK in a different AWS account, you must use the key ARN.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- To get the key ID and key ARN for a CMK, use ListKeys or DescribeKey.
newGetKeyRotationStatus ::
  -- | 'keyId'
  Prelude.Text ->
  GetKeyRotationStatus
newGetKeyRotationStatus pKeyId_ =
  GetKeyRotationStatus' {keyId = pKeyId_}

-- | A unique identifier for the customer master key (CMK).
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK. To
-- specify a CMK in a different AWS account, you must use the key ARN.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- To get the key ID and key ARN for a CMK, use ListKeys or DescribeKey.
getKeyRotationStatus_keyId :: Lens.Lens' GetKeyRotationStatus Prelude.Text
getKeyRotationStatus_keyId = Lens.lens (\GetKeyRotationStatus' {keyId} -> keyId) (\s@GetKeyRotationStatus' {} a -> s {keyId = a} :: GetKeyRotationStatus)

instance Prelude.AWSRequest GetKeyRotationStatus where
  type
    Rs GetKeyRotationStatus =
      GetKeyRotationStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetKeyRotationStatusResponse'
            Prelude.<$> (x Prelude..?> "KeyRotationEnabled")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetKeyRotationStatus

instance Prelude.NFData GetKeyRotationStatus

instance Prelude.ToHeaders GetKeyRotationStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "TrentService.GetKeyRotationStatus" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetKeyRotationStatus where
  toJSON GetKeyRotationStatus' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("KeyId" Prelude..= keyId)]
      )

instance Prelude.ToPath GetKeyRotationStatus where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetKeyRotationStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetKeyRotationStatusResponse' smart constructor.
data GetKeyRotationStatusResponse = GetKeyRotationStatusResponse'
  { -- | A Boolean value that specifies whether key rotation is enabled.
    keyRotationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData GetKeyRotationStatusResponse
