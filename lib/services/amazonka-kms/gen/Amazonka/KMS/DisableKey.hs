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
-- Module      : Amazonka.KMS.DisableKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the state of a KMS key to disabled. This change temporarily
-- prevents use of the KMS key for
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations>.
--
-- For more information about how key state affects the use of a KMS key,
-- see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key states of KMS keys>
-- in the //Key Management Service Developer Guide// .
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
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:DisableKey>
-- (key policy)
--
-- __Related operations__: EnableKey
module Amazonka.KMS.DisableKey
  ( -- * Creating a Request
    DisableKey (..),
    newDisableKey,

    -- * Request Lenses
    disableKey_keyId,

    -- * Destructuring the Response
    DisableKeyResponse (..),
    newDisableKeyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisableKey' smart constructor.
data DisableKey = DisableKey'
  { -- | Identifies the KMS key to disable.
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
-- Create a value of 'DisableKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyId', 'disableKey_keyId' - Identifies the KMS key to disable.
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
newDisableKey ::
  -- | 'keyId'
  Prelude.Text ->
  DisableKey
newDisableKey pKeyId_ = DisableKey' {keyId = pKeyId_}

-- | Identifies the KMS key to disable.
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
disableKey_keyId :: Lens.Lens' DisableKey Prelude.Text
disableKey_keyId = Lens.lens (\DisableKey' {keyId} -> keyId) (\s@DisableKey' {} a -> s {keyId = a} :: DisableKey)

instance Core.AWSRequest DisableKey where
  type AWSResponse DisableKey = DisableKeyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull DisableKeyResponse'

instance Prelude.Hashable DisableKey where
  hashWithSalt _salt DisableKey' {..} =
    _salt `Prelude.hashWithSalt` keyId

instance Prelude.NFData DisableKey where
  rnf DisableKey' {..} = Prelude.rnf keyId

instance Core.ToHeaders DisableKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("TrentService.DisableKey" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DisableKey where
  toJSON DisableKey' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("KeyId" Core..= keyId)]
      )

instance Core.ToPath DisableKey where
  toPath = Prelude.const "/"

instance Core.ToQuery DisableKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisableKeyResponse' smart constructor.
data DisableKeyResponse = DisableKeyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisableKeyResponse ::
  DisableKeyResponse
newDisableKeyResponse = DisableKeyResponse'

instance Prelude.NFData DisableKeyResponse where
  rnf _ = ()
