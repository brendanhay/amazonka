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
-- Module      : Amazonka.KMS.CancelKeyDeletion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the deletion of a KMS key. When this operation succeeds, the key
-- state of the KMS key is @Disabled@. To enable the KMS key, use
-- EnableKey.
--
-- For more information about scheduling and canceling deletion of a KMS
-- key, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/deleting-keys.html Deleting KMS keys>
-- in the /Key Management Service Developer Guide/.
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
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:CancelKeyDeletion>
-- (key policy)
--
-- __Related operations__: ScheduleKeyDeletion
module Amazonka.KMS.CancelKeyDeletion
  ( -- * Creating a Request
    CancelKeyDeletion (..),
    newCancelKeyDeletion,

    -- * Request Lenses
    cancelKeyDeletion_keyId,

    -- * Destructuring the Response
    CancelKeyDeletionResponse (..),
    newCancelKeyDeletionResponse,

    -- * Response Lenses
    cancelKeyDeletionResponse_keyId,
    cancelKeyDeletionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelKeyDeletion' smart constructor.
data CancelKeyDeletion = CancelKeyDeletion'
  { -- | Identifies the KMS key whose deletion is being canceled.
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
-- Create a value of 'CancelKeyDeletion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyId', 'cancelKeyDeletion_keyId' - Identifies the KMS key whose deletion is being canceled.
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
newCancelKeyDeletion ::
  -- | 'keyId'
  Prelude.Text ->
  CancelKeyDeletion
newCancelKeyDeletion pKeyId_ =
  CancelKeyDeletion' {keyId = pKeyId_}

-- | Identifies the KMS key whose deletion is being canceled.
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
cancelKeyDeletion_keyId :: Lens.Lens' CancelKeyDeletion Prelude.Text
cancelKeyDeletion_keyId = Lens.lens (\CancelKeyDeletion' {keyId} -> keyId) (\s@CancelKeyDeletion' {} a -> s {keyId = a} :: CancelKeyDeletion)

instance Core.AWSRequest CancelKeyDeletion where
  type
    AWSResponse CancelKeyDeletion =
      CancelKeyDeletionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelKeyDeletionResponse'
            Prelude.<$> (x Core..?> "KeyId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelKeyDeletion where
  hashWithSalt _salt CancelKeyDeletion' {..} =
    _salt `Prelude.hashWithSalt` keyId

instance Prelude.NFData CancelKeyDeletion where
  rnf CancelKeyDeletion' {..} = Prelude.rnf keyId

instance Core.ToHeaders CancelKeyDeletion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "TrentService.CancelKeyDeletion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CancelKeyDeletion where
  toJSON CancelKeyDeletion' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("KeyId" Core..= keyId)]
      )

instance Core.ToPath CancelKeyDeletion where
  toPath = Prelude.const "/"

instance Core.ToQuery CancelKeyDeletion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelKeyDeletionResponse' smart constructor.
data CancelKeyDeletionResponse = CancelKeyDeletionResponse'
  { -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
    -- of the KMS key whose deletion is canceled.
    keyId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelKeyDeletionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyId', 'cancelKeyDeletionResponse_keyId' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the KMS key whose deletion is canceled.
--
-- 'httpStatus', 'cancelKeyDeletionResponse_httpStatus' - The response's http status code.
newCancelKeyDeletionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelKeyDeletionResponse
newCancelKeyDeletionResponse pHttpStatus_ =
  CancelKeyDeletionResponse'
    { keyId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the KMS key whose deletion is canceled.
cancelKeyDeletionResponse_keyId :: Lens.Lens' CancelKeyDeletionResponse (Prelude.Maybe Prelude.Text)
cancelKeyDeletionResponse_keyId = Lens.lens (\CancelKeyDeletionResponse' {keyId} -> keyId) (\s@CancelKeyDeletionResponse' {} a -> s {keyId = a} :: CancelKeyDeletionResponse)

-- | The response's http status code.
cancelKeyDeletionResponse_httpStatus :: Lens.Lens' CancelKeyDeletionResponse Prelude.Int
cancelKeyDeletionResponse_httpStatus = Lens.lens (\CancelKeyDeletionResponse' {httpStatus} -> httpStatus) (\s@CancelKeyDeletionResponse' {} a -> s {httpStatus = a} :: CancelKeyDeletionResponse)

instance Prelude.NFData CancelKeyDeletionResponse where
  rnf CancelKeyDeletionResponse' {..} =
    Prelude.rnf keyId
      `Prelude.seq` Prelude.rnf httpStatus
