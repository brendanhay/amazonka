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
-- Module      : Network.AWS.KMS.DisableKeyRotation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables
-- <https://docs.aws.amazon.com/kms/latest/developerguide/rotate-keys.html automatic rotation of the key material>
-- for the specified symmetric customer master key (CMK).
--
-- You cannot enable automatic rotation of asymmetric CMKs, CMKs with
-- imported key material, or CMKs in a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>.
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
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:DisableKeyRotation>
-- (key policy)
--
-- __Related operations:__
--
-- -   EnableKeyRotation
--
-- -   GetKeyRotationStatus
module Network.AWS.KMS.DisableKeyRotation
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

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisableKeyRotation' smart constructor.
data DisableKeyRotation = DisableKeyRotation'
  { -- | Identifies a symmetric customer master key (CMK). You cannot enable or
    -- disable automatic rotation of
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html#asymmetric-cmks asymmetric CMKs>,
    -- CMKs with
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html imported key material>,
    -- or CMKs in a
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>.
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
-- Create a value of 'DisableKeyRotation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyId', 'disableKeyRotation_keyId' - Identifies a symmetric customer master key (CMK). You cannot enable or
-- disable automatic rotation of
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html#asymmetric-cmks asymmetric CMKs>,
-- CMKs with
-- <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html imported key material>,
-- or CMKs in a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>.
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
newDisableKeyRotation ::
  -- | 'keyId'
  Prelude.Text ->
  DisableKeyRotation
newDisableKeyRotation pKeyId_ =
  DisableKeyRotation' {keyId = pKeyId_}

-- | Identifies a symmetric customer master key (CMK). You cannot enable or
-- disable automatic rotation of
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html#asymmetric-cmks asymmetric CMKs>,
-- CMKs with
-- <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html imported key material>,
-- or CMKs in a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>.
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
disableKeyRotation_keyId :: Lens.Lens' DisableKeyRotation Prelude.Text
disableKeyRotation_keyId = Lens.lens (\DisableKeyRotation' {keyId} -> keyId) (\s@DisableKeyRotation' {} a -> s {keyId = a} :: DisableKeyRotation)

instance Prelude.AWSRequest DisableKeyRotation where
  type
    Rs DisableKeyRotation =
      DisableKeyRotationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DisableKeyRotationResponse'

instance Prelude.Hashable DisableKeyRotation

instance Prelude.NFData DisableKeyRotation

instance Prelude.ToHeaders DisableKeyRotation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "TrentService.DisableKeyRotation" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DisableKeyRotation where
  toJSON DisableKeyRotation' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("KeyId" Prelude..= keyId)]
      )

instance Prelude.ToPath DisableKeyRotation where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DisableKeyRotation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisableKeyRotationResponse' smart constructor.
data DisableKeyRotationResponse = DisableKeyRotationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisableKeyRotationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisableKeyRotationResponse ::
  DisableKeyRotationResponse
newDisableKeyRotationResponse =
  DisableKeyRotationResponse'

instance Prelude.NFData DisableKeyRotationResponse
