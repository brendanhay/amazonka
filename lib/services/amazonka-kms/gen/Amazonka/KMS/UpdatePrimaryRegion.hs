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
-- Module      : Amazonka.KMS.UpdatePrimaryRegion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the primary key of a multi-Region key.
--
-- This operation changes the replica key in the specified Region to a
-- primary key and changes the former primary key to a replica key. For
-- example, suppose you have a primary key in @us-east-1@ and a replica key
-- in @eu-west-2@. If you run @UpdatePrimaryRegion@ with a @PrimaryRegion@
-- value of @eu-west-2@, the primary key is now the key in @eu-west-2@, and
-- the key in @us-east-1@ becomes a replica key. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/multi-region-keys-manage.html#multi-region-update Updating the primary Region>
-- in the /Key Management Service Developer Guide/.
--
-- This operation supports /multi-Region keys/, an KMS feature that lets
-- you create multiple interoperable KMS keys in different Amazon Web
-- Services Regions. Because these KMS keys have the same key ID, key
-- material, and other metadata, you can use them interchangeably to
-- encrypt data in one Amazon Web Services Region and decrypt it in a
-- different Amazon Web Services Region without re-encrypting the data or
-- making a cross-Region call. For more information about multi-Region
-- keys, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/multi-region-keys-overview.html Multi-Region keys in KMS>
-- in the /Key Management Service Developer Guide/.
--
-- The /primary key/ of a multi-Region key is the source for properties
-- that are always shared by primary and replica keys, including the key
-- material,
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-id key ID>,
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-spec key spec>,
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-usage key usage>,
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-origin key material origin>,
-- and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/rotate-keys.html automatic key rotation>.
-- It\'s the only key that can be replicated. You cannot
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_ScheduleKeyDeletion.html delete the primary key>
-- until all replica keys are deleted.
--
-- The key ID and primary Region that you specify uniquely identify the
-- replica key that will become the primary key. The primary Region must
-- already have a replica key. This operation does not create a KMS key in
-- the specified Region. To find the replica keys, use the DescribeKey
-- operation on the primary key or any replica key. To create a replica
-- key, use the ReplicateKey operation.
--
-- You can run this operation while using the affected multi-Region keys in
-- cryptographic operations. This operation should not delay, interrupt, or
-- cause failures in cryptographic operations.
--
-- Even after this operation completes, the process of updating the primary
-- Region might still be in progress for a few more seconds. Operations
-- such as @DescribeKey@ might display both the old and new primary keys as
-- replicas. The old and new primary keys have a transient key state of
-- @Updating@. The original key state is restored when the update is
-- complete. While the key state is @Updating@, you can use the keys in
-- cryptographic operations, but you cannot replicate the new primary key
-- or perform certain management operations, such as enabling or disabling
-- these keys. For details about the @Updating@ key state, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key states of KMS keys>
-- in the /Key Management Service Developer Guide/.
--
-- This operation does not return any output. To verify that primary key is
-- changed, use the DescribeKey operation.
--
-- __Cross-account use__: No. You cannot use this operation in a different
-- Amazon Web Services account.
--
-- __Required permissions__:
--
-- -   @kms:UpdatePrimaryRegion@ on the current primary key (in the primary
--     key\'s Region). Include this permission primary key\'s key policy.
--
-- -   @kms:UpdatePrimaryRegion@ on the current replica key (in the replica
--     key\'s Region). Include this permission in the replica key\'s key
--     policy.
--
-- __Related operations__
--
-- -   CreateKey
--
-- -   ReplicateKey
module Amazonka.KMS.UpdatePrimaryRegion
  ( -- * Creating a Request
    UpdatePrimaryRegion (..),
    newUpdatePrimaryRegion,

    -- * Request Lenses
    updatePrimaryRegion_keyId,
    updatePrimaryRegion_primaryRegion,

    -- * Destructuring the Response
    UpdatePrimaryRegionResponse (..),
    newUpdatePrimaryRegionResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdatePrimaryRegion' smart constructor.
data UpdatePrimaryRegion = UpdatePrimaryRegion'
  { -- | Identifies the current primary key. When the operation completes, this
    -- KMS key will be a replica key.
    --
    -- Specify the key ID or key ARN of a multi-Region primary key.
    --
    -- For example:
    --
    -- -   Key ID: @mrk-1234abcd12ab34cd56ef1234567890ab@
    --
    -- -   Key ARN:
    --     @arn:aws:kms:us-east-2:111122223333:key\/mrk-1234abcd12ab34cd56ef1234567890ab@
    --
    -- To get the key ID and key ARN for a KMS key, use ListKeys or
    -- DescribeKey.
    keyId :: Prelude.Text,
    -- | The Amazon Web Services Region of the new primary key. Enter the Region
    -- ID, such as @us-east-1@ or @ap-southeast-2@. There must be an existing
    -- replica key in this Region.
    --
    -- When the operation completes, the multi-Region key in this Region will
    -- be the primary key.
    primaryRegion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePrimaryRegion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyId', 'updatePrimaryRegion_keyId' - Identifies the current primary key. When the operation completes, this
-- KMS key will be a replica key.
--
-- Specify the key ID or key ARN of a multi-Region primary key.
--
-- For example:
--
-- -   Key ID: @mrk-1234abcd12ab34cd56ef1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/mrk-1234abcd12ab34cd56ef1234567890ab@
--
-- To get the key ID and key ARN for a KMS key, use ListKeys or
-- DescribeKey.
--
-- 'primaryRegion', 'updatePrimaryRegion_primaryRegion' - The Amazon Web Services Region of the new primary key. Enter the Region
-- ID, such as @us-east-1@ or @ap-southeast-2@. There must be an existing
-- replica key in this Region.
--
-- When the operation completes, the multi-Region key in this Region will
-- be the primary key.
newUpdatePrimaryRegion ::
  -- | 'keyId'
  Prelude.Text ->
  -- | 'primaryRegion'
  Prelude.Text ->
  UpdatePrimaryRegion
newUpdatePrimaryRegion pKeyId_ pPrimaryRegion_ =
  UpdatePrimaryRegion'
    { keyId = pKeyId_,
      primaryRegion = pPrimaryRegion_
    }

-- | Identifies the current primary key. When the operation completes, this
-- KMS key will be a replica key.
--
-- Specify the key ID or key ARN of a multi-Region primary key.
--
-- For example:
--
-- -   Key ID: @mrk-1234abcd12ab34cd56ef1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/mrk-1234abcd12ab34cd56ef1234567890ab@
--
-- To get the key ID and key ARN for a KMS key, use ListKeys or
-- DescribeKey.
updatePrimaryRegion_keyId :: Lens.Lens' UpdatePrimaryRegion Prelude.Text
updatePrimaryRegion_keyId = Lens.lens (\UpdatePrimaryRegion' {keyId} -> keyId) (\s@UpdatePrimaryRegion' {} a -> s {keyId = a} :: UpdatePrimaryRegion)

-- | The Amazon Web Services Region of the new primary key. Enter the Region
-- ID, such as @us-east-1@ or @ap-southeast-2@. There must be an existing
-- replica key in this Region.
--
-- When the operation completes, the multi-Region key in this Region will
-- be the primary key.
updatePrimaryRegion_primaryRegion :: Lens.Lens' UpdatePrimaryRegion Prelude.Text
updatePrimaryRegion_primaryRegion = Lens.lens (\UpdatePrimaryRegion' {primaryRegion} -> primaryRegion) (\s@UpdatePrimaryRegion' {} a -> s {primaryRegion = a} :: UpdatePrimaryRegion)

instance Core.AWSRequest UpdatePrimaryRegion where
  type
    AWSResponse UpdatePrimaryRegion =
      UpdatePrimaryRegionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull UpdatePrimaryRegionResponse'

instance Prelude.Hashable UpdatePrimaryRegion where
  hashWithSalt _salt UpdatePrimaryRegion' {..} =
    _salt
      `Prelude.hashWithSalt` keyId
      `Prelude.hashWithSalt` primaryRegion

instance Prelude.NFData UpdatePrimaryRegion where
  rnf UpdatePrimaryRegion' {..} =
    Prelude.rnf keyId
      `Prelude.seq` Prelude.rnf primaryRegion

instance Data.ToHeaders UpdatePrimaryRegion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TrentService.UpdatePrimaryRegion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdatePrimaryRegion where
  toJSON UpdatePrimaryRegion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("KeyId" Data..= keyId),
            Prelude.Just
              ("PrimaryRegion" Data..= primaryRegion)
          ]
      )

instance Data.ToPath UpdatePrimaryRegion where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdatePrimaryRegion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePrimaryRegionResponse' smart constructor.
data UpdatePrimaryRegionResponse = UpdatePrimaryRegionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePrimaryRegionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdatePrimaryRegionResponse ::
  UpdatePrimaryRegionResponse
newUpdatePrimaryRegionResponse =
  UpdatePrimaryRegionResponse'

instance Prelude.NFData UpdatePrimaryRegionResponse where
  rnf _ = ()
