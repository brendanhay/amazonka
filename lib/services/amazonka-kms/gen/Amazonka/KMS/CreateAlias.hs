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
-- Module      : Amazonka.KMS.CreateAlias
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a friendly name for a KMS key.
--
-- Adding, deleting, or updating an alias can allow or deny permission to
-- the KMS key. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/abac.html ABAC for KMS>
-- in the /Key Management Service Developer Guide/.
--
-- You can use an alias to identify a KMS key in the KMS console, in the
-- DescribeKey operation and in
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations>,
-- such as Encrypt and GenerateDataKey. You can also change the KMS key
-- that\'s associated with the alias (UpdateAlias) or delete the alias
-- (DeleteAlias) at any time. These operations don\'t affect the underlying
-- KMS key.
--
-- You can associate the alias with any customer managed key in the same
-- Amazon Web Services Region. Each alias is associated with only one KMS
-- key at a time, but a KMS key can have multiple aliases. A valid KMS key
-- is required. You can\'t create an alias without a KMS key.
--
-- The alias must be unique in the account and Region, but you can have
-- aliases with the same name in different Regions. For detailed
-- information about aliases, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-alias.html Using aliases>
-- in the /Key Management Service Developer Guide/.
--
-- This operation does not return a response. To get the alias that you
-- created, use the ListAliases operation.
--
-- The KMS key that you use for this operation must be in a compatible key
-- state. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key states of KMS keys>
-- in the /Key Management Service Developer Guide/.
--
-- __Cross-account use__: No. You cannot perform this operation on an alias
-- in a different Amazon Web Services account.
--
-- __Required permissions__
--
-- -   <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:CreateAlias>
--     on the alias (IAM policy).
--
-- -   <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:CreateAlias>
--     on the KMS key (key policy).
--
-- For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-alias.html#alias-access Controlling access to aliases>
-- in the /Key Management Service Developer Guide/.
--
-- __Related operations:__
--
-- -   DeleteAlias
--
-- -   ListAliases
--
-- -   UpdateAlias
module Amazonka.KMS.CreateAlias
  ( -- * Creating a Request
    CreateAlias (..),
    newCreateAlias,

    -- * Request Lenses
    createAlias_aliasName,
    createAlias_targetKeyId,

    -- * Destructuring the Response
    CreateAliasResponse (..),
    newCreateAliasResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAlias' smart constructor.
data CreateAlias = CreateAlias'
  { -- | Specifies the alias name. This value must begin with @alias\/@ followed
    -- by a name, such as @alias\/ExampleAlias@.
    --
    -- The @AliasName@ value must be string of 1-256 characters. It can contain
    -- only alphanumeric characters, forward slashes (\/), underscores (_), and
    -- dashes (-). The alias name cannot begin with @alias\/aws\/@. The
    -- @alias\/aws\/@ prefix is reserved for
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#aws-managed-cmk Amazon Web Services managed keys>.
    aliasName :: Prelude.Text,
    -- | Associates the alias with the specified
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#customer-cmk customer managed key>.
    -- The KMS key must be in the same Amazon Web Services Region.
    --
    -- A valid key ID is required. If you supply a null or empty string value,
    -- this operation returns an error.
    --
    -- For help finding the key ID and ARN, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/viewing-keys.html#find-cmk-id-arn Finding the Key ID and ARN>
    -- in the //Key Management Service Developer Guide// .
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
    targetKeyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aliasName', 'createAlias_aliasName' - Specifies the alias name. This value must begin with @alias\/@ followed
-- by a name, such as @alias\/ExampleAlias@.
--
-- The @AliasName@ value must be string of 1-256 characters. It can contain
-- only alphanumeric characters, forward slashes (\/), underscores (_), and
-- dashes (-). The alias name cannot begin with @alias\/aws\/@. The
-- @alias\/aws\/@ prefix is reserved for
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#aws-managed-cmk Amazon Web Services managed keys>.
--
-- 'targetKeyId', 'createAlias_targetKeyId' - Associates the alias with the specified
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#customer-cmk customer managed key>.
-- The KMS key must be in the same Amazon Web Services Region.
--
-- A valid key ID is required. If you supply a null or empty string value,
-- this operation returns an error.
--
-- For help finding the key ID and ARN, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/viewing-keys.html#find-cmk-id-arn Finding the Key ID and ARN>
-- in the //Key Management Service Developer Guide// .
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
newCreateAlias ::
  -- | 'aliasName'
  Prelude.Text ->
  -- | 'targetKeyId'
  Prelude.Text ->
  CreateAlias
newCreateAlias pAliasName_ pTargetKeyId_ =
  CreateAlias'
    { aliasName = pAliasName_,
      targetKeyId = pTargetKeyId_
    }

-- | Specifies the alias name. This value must begin with @alias\/@ followed
-- by a name, such as @alias\/ExampleAlias@.
--
-- The @AliasName@ value must be string of 1-256 characters. It can contain
-- only alphanumeric characters, forward slashes (\/), underscores (_), and
-- dashes (-). The alias name cannot begin with @alias\/aws\/@. The
-- @alias\/aws\/@ prefix is reserved for
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#aws-managed-cmk Amazon Web Services managed keys>.
createAlias_aliasName :: Lens.Lens' CreateAlias Prelude.Text
createAlias_aliasName = Lens.lens (\CreateAlias' {aliasName} -> aliasName) (\s@CreateAlias' {} a -> s {aliasName = a} :: CreateAlias)

-- | Associates the alias with the specified
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#customer-cmk customer managed key>.
-- The KMS key must be in the same Amazon Web Services Region.
--
-- A valid key ID is required. If you supply a null or empty string value,
-- this operation returns an error.
--
-- For help finding the key ID and ARN, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/viewing-keys.html#find-cmk-id-arn Finding the Key ID and ARN>
-- in the //Key Management Service Developer Guide// .
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
createAlias_targetKeyId :: Lens.Lens' CreateAlias Prelude.Text
createAlias_targetKeyId = Lens.lens (\CreateAlias' {targetKeyId} -> targetKeyId) (\s@CreateAlias' {} a -> s {targetKeyId = a} :: CreateAlias)

instance Core.AWSRequest CreateAlias where
  type AWSResponse CreateAlias = CreateAliasResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull CreateAliasResponse'

instance Prelude.Hashable CreateAlias where
  hashWithSalt _salt CreateAlias' {..} =
    _salt `Prelude.hashWithSalt` aliasName
      `Prelude.hashWithSalt` targetKeyId

instance Prelude.NFData CreateAlias where
  rnf CreateAlias' {..} =
    Prelude.rnf aliasName
      `Prelude.seq` Prelude.rnf targetKeyId

instance Data.ToHeaders CreateAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("TrentService.CreateAlias" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAlias where
  toJSON CreateAlias' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AliasName" Data..= aliasName),
            Prelude.Just ("TargetKeyId" Data..= targetKeyId)
          ]
      )

instance Data.ToPath CreateAlias where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAliasResponse' smart constructor.
data CreateAliasResponse = CreateAliasResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreateAliasResponse ::
  CreateAliasResponse
newCreateAliasResponse = CreateAliasResponse'

instance Prelude.NFData CreateAliasResponse where
  rnf _ = ()
