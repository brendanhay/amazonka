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
-- Module      : Amazonka.KMS.UpdateAlias
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an existing KMS alias with a different KMS key. Each alias is
-- associated with only one KMS key at a time, although a KMS key can have
-- multiple aliases. The alias and the KMS key must be in the same Amazon
-- Web Services account and Region.
--
-- Adding, deleting, or updating an alias can allow or deny permission to
-- the KMS key. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/abac.html ABAC for KMS>
-- in the /Key Management Service Developer Guide/.
--
-- The current and new KMS key must be the same type (both symmetric or
-- both asymmetric or both HMAC), and they must have the same key usage.
-- This restriction prevents errors in code that uses aliases. If you must
-- assign an alias to a different type of KMS key, use DeleteAlias to
-- delete the old alias and CreateAlias to create a new alias.
--
-- You cannot use @UpdateAlias@ to change an alias name. To change an alias
-- name, use DeleteAlias to delete the old alias and CreateAlias to create
-- a new alias.
--
-- Because an alias is not a property of a KMS key, you can create, update,
-- and delete the aliases of a KMS key without affecting the KMS key. Also,
-- aliases do not appear in the response from the DescribeKey operation. To
-- get the aliases of all KMS keys in the account, use the ListAliases
-- operation.
--
-- The KMS key that you use for this operation must be in a compatible key
-- state. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key states of KMS keys>
-- in the /Key Management Service Developer Guide/.
--
-- __Cross-account use__: No. You cannot perform this operation on a KMS
-- key in a different Amazon Web Services account.
--
-- __Required permissions__
--
-- -   <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:UpdateAlias>
--     on the alias (IAM policy).
--
-- -   <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:UpdateAlias>
--     on the current KMS key (key policy).
--
-- -   <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:UpdateAlias>
--     on the new KMS key (key policy).
--
-- For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-alias.html#alias-access Controlling access to aliases>
-- in the /Key Management Service Developer Guide/.
--
-- __Related operations:__
--
-- -   CreateAlias
--
-- -   DeleteAlias
--
-- -   ListAliases
module Amazonka.KMS.UpdateAlias
  ( -- * Creating a Request
    UpdateAlias (..),
    newUpdateAlias,

    -- * Request Lenses
    updateAlias_aliasName,
    updateAlias_targetKeyId,

    -- * Destructuring the Response
    UpdateAliasResponse (..),
    newUpdateAliasResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAlias' smart constructor.
data UpdateAlias = UpdateAlias'
  { -- | Identifies the alias that is changing its KMS key. This value must begin
    -- with @alias\/@ followed by the alias name, such as
    -- @alias\/ExampleAlias@. You cannot use @UpdateAlias@ to change the alias
    -- name.
    aliasName :: Prelude.Text,
    -- | Identifies the
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#customer-cmk customer managed key>
    -- to associate with the alias. You don\'t have permission to associate an
    -- alias with an
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#aws-managed-cmk Amazon Web Services managed key>.
    --
    -- The KMS key must be in the same Amazon Web Services account and Region
    -- as the alias. Also, the new target KMS key must be the same type as the
    -- current target KMS key (both symmetric or both asymmetric or both HMAC)
    -- and they must have the same key usage.
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
    --
    -- To verify that the alias is mapped to the correct KMS key, use
    -- ListAliases.
    targetKeyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aliasName', 'updateAlias_aliasName' - Identifies the alias that is changing its KMS key. This value must begin
-- with @alias\/@ followed by the alias name, such as
-- @alias\/ExampleAlias@. You cannot use @UpdateAlias@ to change the alias
-- name.
--
-- 'targetKeyId', 'updateAlias_targetKeyId' - Identifies the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#customer-cmk customer managed key>
-- to associate with the alias. You don\'t have permission to associate an
-- alias with an
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#aws-managed-cmk Amazon Web Services managed key>.
--
-- The KMS key must be in the same Amazon Web Services account and Region
-- as the alias. Also, the new target KMS key must be the same type as the
-- current target KMS key (both symmetric or both asymmetric or both HMAC)
-- and they must have the same key usage.
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
--
-- To verify that the alias is mapped to the correct KMS key, use
-- ListAliases.
newUpdateAlias ::
  -- | 'aliasName'
  Prelude.Text ->
  -- | 'targetKeyId'
  Prelude.Text ->
  UpdateAlias
newUpdateAlias pAliasName_ pTargetKeyId_ =
  UpdateAlias'
    { aliasName = pAliasName_,
      targetKeyId = pTargetKeyId_
    }

-- | Identifies the alias that is changing its KMS key. This value must begin
-- with @alias\/@ followed by the alias name, such as
-- @alias\/ExampleAlias@. You cannot use @UpdateAlias@ to change the alias
-- name.
updateAlias_aliasName :: Lens.Lens' UpdateAlias Prelude.Text
updateAlias_aliasName = Lens.lens (\UpdateAlias' {aliasName} -> aliasName) (\s@UpdateAlias' {} a -> s {aliasName = a} :: UpdateAlias)

-- | Identifies the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#customer-cmk customer managed key>
-- to associate with the alias. You don\'t have permission to associate an
-- alias with an
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#aws-managed-cmk Amazon Web Services managed key>.
--
-- The KMS key must be in the same Amazon Web Services account and Region
-- as the alias. Also, the new target KMS key must be the same type as the
-- current target KMS key (both symmetric or both asymmetric or both HMAC)
-- and they must have the same key usage.
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
--
-- To verify that the alias is mapped to the correct KMS key, use
-- ListAliases.
updateAlias_targetKeyId :: Lens.Lens' UpdateAlias Prelude.Text
updateAlias_targetKeyId = Lens.lens (\UpdateAlias' {targetKeyId} -> targetKeyId) (\s@UpdateAlias' {} a -> s {targetKeyId = a} :: UpdateAlias)

instance Core.AWSRequest UpdateAlias where
  type AWSResponse UpdateAlias = UpdateAliasResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull UpdateAliasResponse'

instance Prelude.Hashable UpdateAlias where
  hashWithSalt _salt UpdateAlias' {..} =
    _salt
      `Prelude.hashWithSalt` aliasName
      `Prelude.hashWithSalt` targetKeyId

instance Prelude.NFData UpdateAlias where
  rnf UpdateAlias' {..} =
    Prelude.rnf aliasName
      `Prelude.seq` Prelude.rnf targetKeyId

instance Data.ToHeaders UpdateAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("TrentService.UpdateAlias" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateAlias where
  toJSON UpdateAlias' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AliasName" Data..= aliasName),
            Prelude.Just ("TargetKeyId" Data..= targetKeyId)
          ]
      )

instance Data.ToPath UpdateAlias where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAliasResponse' smart constructor.
data UpdateAliasResponse = UpdateAliasResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateAliasResponse ::
  UpdateAliasResponse
newUpdateAliasResponse = UpdateAliasResponse'

instance Prelude.NFData UpdateAliasResponse where
  rnf _ = ()
