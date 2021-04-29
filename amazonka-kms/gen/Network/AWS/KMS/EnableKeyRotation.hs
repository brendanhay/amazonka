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
-- Module      : Network.AWS.KMS.EnableKeyRotation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables
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
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:EnableKeyRotation>
-- (key policy)
--
-- __Related operations:__
--
-- -   DisableKeyRotation
--
-- -   GetKeyRotationStatus
module Network.AWS.KMS.EnableKeyRotation
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

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newEnableKeyRotation' smart constructor.
data EnableKeyRotation = EnableKeyRotation'
  { -- | Identifies a symmetric customer master key (CMK). You cannot enable
    -- automatic rotation of asymmetric CMKs, CMKs with imported key material,
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
-- Create a value of 'EnableKeyRotation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyId', 'enableKeyRotation_keyId' - Identifies a symmetric customer master key (CMK). You cannot enable
-- automatic rotation of asymmetric CMKs, CMKs with imported key material,
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
newEnableKeyRotation ::
  -- | 'keyId'
  Prelude.Text ->
  EnableKeyRotation
newEnableKeyRotation pKeyId_ =
  EnableKeyRotation' {keyId = pKeyId_}

-- | Identifies a symmetric customer master key (CMK). You cannot enable
-- automatic rotation of asymmetric CMKs, CMKs with imported key material,
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
enableKeyRotation_keyId :: Lens.Lens' EnableKeyRotation Prelude.Text
enableKeyRotation_keyId = Lens.lens (\EnableKeyRotation' {keyId} -> keyId) (\s@EnableKeyRotation' {} a -> s {keyId = a} :: EnableKeyRotation)

instance Prelude.AWSRequest EnableKeyRotation where
  type Rs EnableKeyRotation = EnableKeyRotationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull EnableKeyRotationResponse'

instance Prelude.Hashable EnableKeyRotation

instance Prelude.NFData EnableKeyRotation

instance Prelude.ToHeaders EnableKeyRotation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "TrentService.EnableKeyRotation" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON EnableKeyRotation where
  toJSON EnableKeyRotation' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("KeyId" Prelude..= keyId)]
      )

instance Prelude.ToPath EnableKeyRotation where
  toPath = Prelude.const "/"

instance Prelude.ToQuery EnableKeyRotation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEnableKeyRotationResponse' smart constructor.
data EnableKeyRotationResponse = EnableKeyRotationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EnableKeyRotationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newEnableKeyRotationResponse ::
  EnableKeyRotationResponse
newEnableKeyRotationResponse =
  EnableKeyRotationResponse'

instance Prelude.NFData EnableKeyRotationResponse
