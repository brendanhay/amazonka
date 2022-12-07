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
-- Module      : Amazonka.KMS.EnableKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the key state of a KMS key to enabled. This allows you to use the
-- KMS key for
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations>.
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
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:EnableKey>
-- (key policy)
--
-- __Related operations__: DisableKey
module Amazonka.KMS.EnableKey
  ( -- * Creating a Request
    EnableKey (..),
    newEnableKey,

    -- * Request Lenses
    enableKey_keyId,

    -- * Destructuring the Response
    EnableKeyResponse (..),
    newEnableKeyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEnableKey' smart constructor.
data EnableKey = EnableKey'
  { -- | Identifies the KMS key to enable.
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
-- Create a value of 'EnableKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyId', 'enableKey_keyId' - Identifies the KMS key to enable.
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
newEnableKey ::
  -- | 'keyId'
  Prelude.Text ->
  EnableKey
newEnableKey pKeyId_ = EnableKey' {keyId = pKeyId_}

-- | Identifies the KMS key to enable.
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
enableKey_keyId :: Lens.Lens' EnableKey Prelude.Text
enableKey_keyId = Lens.lens (\EnableKey' {keyId} -> keyId) (\s@EnableKey' {} a -> s {keyId = a} :: EnableKey)

instance Core.AWSRequest EnableKey where
  type AWSResponse EnableKey = EnableKeyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull EnableKeyResponse'

instance Prelude.Hashable EnableKey where
  hashWithSalt _salt EnableKey' {..} =
    _salt `Prelude.hashWithSalt` keyId

instance Prelude.NFData EnableKey where
  rnf EnableKey' {..} = Prelude.rnf keyId

instance Data.ToHeaders EnableKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("TrentService.EnableKey" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON EnableKey where
  toJSON EnableKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("KeyId" Data..= keyId)]
      )

instance Data.ToPath EnableKey where
  toPath = Prelude.const "/"

instance Data.ToQuery EnableKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEnableKeyResponse' smart constructor.
data EnableKeyResponse = EnableKeyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newEnableKeyResponse ::
  EnableKeyResponse
newEnableKeyResponse = EnableKeyResponse'

instance Prelude.NFData EnableKeyResponse where
  rnf _ = ()
