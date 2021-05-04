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
-- Module      : Network.AWS.KMS.UpdateAlias
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an existing AWS KMS alias with a different customer master
-- key (CMK). Each alias is associated with only one CMK at a time,
-- although a CMK can have multiple aliases. The alias and the CMK must be
-- in the same AWS account and region.
--
-- The current and new CMK must be the same type (both symmetric or both
-- asymmetric), and they must have the same key usage (@ENCRYPT_DECRYPT@ or
-- @SIGN_VERIFY@). This restriction prevents errors in code that uses
-- aliases. If you must assign an alias to a different type of CMK, use
-- DeleteAlias to delete the old alias and CreateAlias to create a new
-- alias.
--
-- You cannot use @UpdateAlias@ to change an alias name. To change an alias
-- name, use DeleteAlias to delete the old alias and CreateAlias to create
-- a new alias.
--
-- Because an alias is not a property of a CMK, you can create, update, and
-- delete the aliases of a CMK without affecting the CMK. Also, aliases do
-- not appear in the response from the DescribeKey operation. To get the
-- aliases of all CMKs in the account, use the ListAliases operation.
--
-- The CMK that you use for this operation must be in a compatible key
-- state. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key>
-- in the /AWS Key Management Service Developer Guide/.
--
-- __Cross-account use__: No. You cannot perform this operation on a CMK in
-- a different AWS account.
--
-- __Required permissions__
--
-- -   <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:UpdateAlias>
--     on the alias (IAM policy).
--
-- -   <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:UpdateAlias>
--     on the current CMK (key policy).
--
-- -   <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:UpdateAlias>
--     on the new CMK (key policy).
--
-- For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-alias.html#alias-access Controlling access to aliases>
-- in the /AWS Key Management Service Developer Guide/.
--
-- __Related operations:__
--
-- -   CreateAlias
--
-- -   DeleteAlias
--
-- -   ListAliases
module Network.AWS.KMS.UpdateAlias
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

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateAlias' smart constructor.
data UpdateAlias = UpdateAlias'
  { -- | Identifies the alias that is changing its CMK. This value must begin
    -- with @alias\/@ followed by the alias name, such as
    -- @alias\/ExampleAlias@. You cannot use UpdateAlias to change the alias
    -- name.
    aliasName :: Prelude.Text,
    -- | Identifies the
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#customer-cmk customer managed CMK>
    -- to associate with the alias. You don\'t have permission to associate an
    -- alias with an
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#aws-managed-cmk AWS managed CMK>.
    --
    -- The CMK must be in the same AWS account and Region as the alias. Also,
    -- the new target CMK must be the same type as the current target CMK (both
    -- symmetric or both asymmetric) and they must have the same key usage.
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
    --
    -- To verify that the alias is mapped to the correct CMK, use ListAliases.
    targetKeyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aliasName', 'updateAlias_aliasName' - Identifies the alias that is changing its CMK. This value must begin
-- with @alias\/@ followed by the alias name, such as
-- @alias\/ExampleAlias@. You cannot use UpdateAlias to change the alias
-- name.
--
-- 'targetKeyId', 'updateAlias_targetKeyId' - Identifies the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#customer-cmk customer managed CMK>
-- to associate with the alias. You don\'t have permission to associate an
-- alias with an
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#aws-managed-cmk AWS managed CMK>.
--
-- The CMK must be in the same AWS account and Region as the alias. Also,
-- the new target CMK must be the same type as the current target CMK (both
-- symmetric or both asymmetric) and they must have the same key usage.
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
--
-- To verify that the alias is mapped to the correct CMK, use ListAliases.
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

-- | Identifies the alias that is changing its CMK. This value must begin
-- with @alias\/@ followed by the alias name, such as
-- @alias\/ExampleAlias@. You cannot use UpdateAlias to change the alias
-- name.
updateAlias_aliasName :: Lens.Lens' UpdateAlias Prelude.Text
updateAlias_aliasName = Lens.lens (\UpdateAlias' {aliasName} -> aliasName) (\s@UpdateAlias' {} a -> s {aliasName = a} :: UpdateAlias)

-- | Identifies the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#customer-cmk customer managed CMK>
-- to associate with the alias. You don\'t have permission to associate an
-- alias with an
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#aws-managed-cmk AWS managed CMK>.
--
-- The CMK must be in the same AWS account and Region as the alias. Also,
-- the new target CMK must be the same type as the current target CMK (both
-- symmetric or both asymmetric) and they must have the same key usage.
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
--
-- To verify that the alias is mapped to the correct CMK, use ListAliases.
updateAlias_targetKeyId :: Lens.Lens' UpdateAlias Prelude.Text
updateAlias_targetKeyId = Lens.lens (\UpdateAlias' {targetKeyId} -> targetKeyId) (\s@UpdateAlias' {} a -> s {targetKeyId = a} :: UpdateAlias)

instance Prelude.AWSRequest UpdateAlias where
  type Rs UpdateAlias = UpdateAliasResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull UpdateAliasResponse'

instance Prelude.Hashable UpdateAlias

instance Prelude.NFData UpdateAlias

instance Prelude.ToHeaders UpdateAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("TrentService.UpdateAlias" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateAlias where
  toJSON UpdateAlias' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AliasName" Prelude..= aliasName),
            Prelude.Just ("TargetKeyId" Prelude..= targetKeyId)
          ]
      )

instance Prelude.ToPath UpdateAlias where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAliasResponse' smart constructor.
data UpdateAliasResponse = UpdateAliasResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateAliasResponse ::
  UpdateAliasResponse
newUpdateAliasResponse = UpdateAliasResponse'

instance Prelude.NFData UpdateAliasResponse
