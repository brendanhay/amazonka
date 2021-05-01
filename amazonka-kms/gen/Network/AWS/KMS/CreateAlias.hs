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
-- Module      : Network.AWS.KMS.CreateAlias
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a friendly name for a customer master key (CMK). You can use an
-- alias to identify a CMK in the AWS KMS console, in the DescribeKey
-- operation and in
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations>,
-- such as Encrypt and GenerateDataKey.
--
-- You can also change the CMK that\'s associated with the alias
-- (UpdateAlias) or delete the alias (DeleteAlias) at any time. These
-- operations don\'t affect the underlying CMK.
--
-- You can associate the alias with any customer managed CMK in the same
-- AWS Region. Each alias is associated with only on CMK at a time, but a
-- CMK can have multiple aliases. A valid CMK is required. You can\'t
-- create an alias without a CMK.
--
-- The alias must be unique in the account and Region, but you can have
-- aliases with the same name in different Regions. For detailed
-- information about aliases, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-alias.html Using aliases>
-- in the /AWS Key Management Service Developer Guide/.
--
-- This operation does not return a response. To get the alias that you
-- created, use the ListAliases operation.
--
-- The CMK that you use for this operation must be in a compatible key
-- state. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key>
-- in the /AWS Key Management Service Developer Guide/.
--
-- __Cross-account use__: No. You cannot perform this operation on an alias
-- in a different AWS account.
--
-- __Required permissions__
--
-- -   <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:CreateAlias>
--     on the alias (IAM policy).
--
-- -   <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:CreateAlias>
--     on the CMK (key policy).
--
-- For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-alias.html#alias-access Controlling access to aliases>
-- in the /AWS Key Management Service Developer Guide/.
--
-- __Related operations:__
--
-- -   DeleteAlias
--
-- -   ListAliases
--
-- -   UpdateAlias
module Network.AWS.KMS.CreateAlias
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

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateAlias' smart constructor.
data CreateAlias = CreateAlias'
  { -- | Specifies the alias name. This value must begin with @alias\/@ followed
    -- by a name, such as @alias\/ExampleAlias@.
    --
    -- The @AliasName@ value must be string of 1-256 characters. It can contain
    -- only alphanumeric characters, forward slashes (\/), underscores (_), and
    -- dashes (-). The alias name cannot begin with @alias\/aws\/@. The
    -- @alias\/aws\/@ prefix is reserved for
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#aws-managed-cmk AWS managed CMKs>.
    aliasName :: Prelude.Text,
    -- | Associates the alias with the specified
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#customer-cmk customer managed CMK>.
    -- The CMK must be in the same AWS Region.
    --
    -- A valid CMK ID is required. If you supply a null or empty string value,
    -- this operation returns an error.
    --
    -- For help finding the key ID and ARN, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/viewing-keys.html#find-cmk-id-arn Finding the Key ID and ARN>
    -- in the /AWS Key Management Service Developer Guide/.
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
    targetKeyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#aws-managed-cmk AWS managed CMKs>.
--
-- 'targetKeyId', 'createAlias_targetKeyId' - Associates the alias with the specified
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#customer-cmk customer managed CMK>.
-- The CMK must be in the same AWS Region.
--
-- A valid CMK ID is required. If you supply a null or empty string value,
-- this operation returns an error.
--
-- For help finding the key ID and ARN, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/viewing-keys.html#find-cmk-id-arn Finding the Key ID and ARN>
-- in the /AWS Key Management Service Developer Guide/.
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
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#aws-managed-cmk AWS managed CMKs>.
createAlias_aliasName :: Lens.Lens' CreateAlias Prelude.Text
createAlias_aliasName = Lens.lens (\CreateAlias' {aliasName} -> aliasName) (\s@CreateAlias' {} a -> s {aliasName = a} :: CreateAlias)

-- | Associates the alias with the specified
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#customer-cmk customer managed CMK>.
-- The CMK must be in the same AWS Region.
--
-- A valid CMK ID is required. If you supply a null or empty string value,
-- this operation returns an error.
--
-- For help finding the key ID and ARN, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/viewing-keys.html#find-cmk-id-arn Finding the Key ID and ARN>
-- in the /AWS Key Management Service Developer Guide/.
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
createAlias_targetKeyId :: Lens.Lens' CreateAlias Prelude.Text
createAlias_targetKeyId = Lens.lens (\CreateAlias' {targetKeyId} -> targetKeyId) (\s@CreateAlias' {} a -> s {targetKeyId = a} :: CreateAlias)

instance Prelude.AWSRequest CreateAlias where
  type Rs CreateAlias = CreateAliasResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull CreateAliasResponse'

instance Prelude.Hashable CreateAlias

instance Prelude.NFData CreateAlias

instance Prelude.ToHeaders CreateAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("TrentService.CreateAlias" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateAlias where
  toJSON CreateAlias' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AliasName" Prelude..= aliasName),
            Prelude.Just ("TargetKeyId" Prelude..= targetKeyId)
          ]
      )

instance Prelude.ToPath CreateAlias where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAliasResponse' smart constructor.
data CreateAliasResponse = CreateAliasResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreateAliasResponse ::
  CreateAliasResponse
newCreateAliasResponse = CreateAliasResponse'

instance Prelude.NFData CreateAliasResponse
