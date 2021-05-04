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
-- Module      : Network.AWS.KMS.CancelKeyDeletion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the deletion of a customer master key (CMK). When this operation
-- succeeds, the key state of the CMK is @Disabled@. To enable the CMK, use
-- EnableKey.
--
-- For more information about scheduling and canceling deletion of a CMK,
-- see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/deleting-keys.html Deleting Customer Master Keys>
-- in the /AWS Key Management Service Developer Guide/.
--
-- The CMK that you use for this operation must be in a compatible key
-- state. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key>
-- in the /AWS Key Management Service Developer Guide/.
--
-- __Cross-account use__: No. You cannot perform this operation on a CMK in
-- a different AWS account.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:CancelKeyDeletion>
-- (key policy)
--
-- __Related operations__: ScheduleKeyDeletion
module Network.AWS.KMS.CancelKeyDeletion
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

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCancelKeyDeletion' smart constructor.
data CancelKeyDeletion = CancelKeyDeletion'
  { -- | The unique identifier for the customer master key (CMK) for which to
    -- cancel deletion.
    --
    -- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
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
-- Create a value of 'CancelKeyDeletion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyId', 'cancelKeyDeletion_keyId' - The unique identifier for the customer master key (CMK) for which to
-- cancel deletion.
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- To get the key ID and key ARN for a CMK, use ListKeys or DescribeKey.
newCancelKeyDeletion ::
  -- | 'keyId'
  Prelude.Text ->
  CancelKeyDeletion
newCancelKeyDeletion pKeyId_ =
  CancelKeyDeletion' {keyId = pKeyId_}

-- | The unique identifier for the customer master key (CMK) for which to
-- cancel deletion.
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- To get the key ID and key ARN for a CMK, use ListKeys or DescribeKey.
cancelKeyDeletion_keyId :: Lens.Lens' CancelKeyDeletion Prelude.Text
cancelKeyDeletion_keyId = Lens.lens (\CancelKeyDeletion' {keyId} -> keyId) (\s@CancelKeyDeletion' {} a -> s {keyId = a} :: CancelKeyDeletion)

instance Prelude.AWSRequest CancelKeyDeletion where
  type Rs CancelKeyDeletion = CancelKeyDeletionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelKeyDeletionResponse'
            Prelude.<$> (x Prelude..?> "KeyId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelKeyDeletion

instance Prelude.NFData CancelKeyDeletion

instance Prelude.ToHeaders CancelKeyDeletion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "TrentService.CancelKeyDeletion" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CancelKeyDeletion where
  toJSON CancelKeyDeletion' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("KeyId" Prelude..= keyId)]
      )

instance Prelude.ToPath CancelKeyDeletion where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CancelKeyDeletion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelKeyDeletionResponse' smart constructor.
data CancelKeyDeletionResponse = CancelKeyDeletionResponse'
  { -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
    -- of the CMK whose deletion is canceled.
    keyId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- of the CMK whose deletion is canceled.
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
-- of the CMK whose deletion is canceled.
cancelKeyDeletionResponse_keyId :: Lens.Lens' CancelKeyDeletionResponse (Prelude.Maybe Prelude.Text)
cancelKeyDeletionResponse_keyId = Lens.lens (\CancelKeyDeletionResponse' {keyId} -> keyId) (\s@CancelKeyDeletionResponse' {} a -> s {keyId = a} :: CancelKeyDeletionResponse)

-- | The response's http status code.
cancelKeyDeletionResponse_httpStatus :: Lens.Lens' CancelKeyDeletionResponse Prelude.Int
cancelKeyDeletionResponse_httpStatus = Lens.lens (\CancelKeyDeletionResponse' {httpStatus} -> httpStatus) (\s@CancelKeyDeletionResponse' {} a -> s {httpStatus = a} :: CancelKeyDeletionResponse)

instance Prelude.NFData CancelKeyDeletionResponse
