{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.UpdateAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an existing AWS KMS alias with a different customer master key (CMK). Each alias is associated with only one CMK at a time, although a CMK can have multiple aliases. The alias and the CMK must be in the same AWS account and region. You cannot perform this operation on an alias in a different AWS account.
--
-- The current and new CMK must be the same type (both symmetric or both asymmetric), and they must have the same key usage (@ENCRYPT_DECRYPT@ or @SIGN_VERIFY@ ). This restriction prevents errors in code that uses aliases. If you must assign an alias to a different type of CMK, use 'DeleteAlias' to delete the old alias and 'CreateAlias' to create a new alias.
-- You cannot use @UpdateAlias@ to change an alias name. To change an alias name, use 'DeleteAlias' to delete the old alias and 'CreateAlias' to create a new alias.
-- Because an alias is not a property of a CMK, you can create, update, and delete the aliases of a CMK without affecting the CMK. Also, aliases do not appear in the response from the 'DescribeKey' operation. To get the aliases of all CMKs in the account, use the 'ListAliases' operation.
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.UpdateAlias
  ( -- * Creating a request
    UpdateAlias (..),
    mkUpdateAlias,

    -- ** Request lenses
    uaTargetKeyId,
    uaAliasName,

    -- * Destructuring the response
    UpdateAliasResponse (..),
    mkUpdateAliasResponse,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateAlias' smart constructor.
data UpdateAlias = UpdateAlias'
  { -- | Identifies the CMK to associate with the alias. When the update operation completes, the alias will point to this CMK.
    --
    -- The CMK must be in the same AWS account and Region as the alias. Also, the new target CMK must be the same type as the current target CMK (both symmetric or both asymmetric) and they must have the same key usage.
    -- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
    -- For example:
    --
    --     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    --
    --     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    --
    -- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
    -- To verify that the alias is mapped to the correct CMK, use 'ListAliases' .
    targetKeyId :: Lude.Text,
    -- | Identifies the alias that is changing its CMK. This value must begin with @alias/@ followed by the alias name, such as @alias/ExampleAlias@ . You cannot use UpdateAlias to change the alias name.
    aliasName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAlias' with the minimum fields required to make a request.
--
-- * 'targetKeyId' - Identifies the CMK to associate with the alias. When the update operation completes, the alias will point to this CMK.
--
-- The CMK must be in the same AWS account and Region as the alias. Also, the new target CMK must be the same type as the current target CMK (both symmetric or both asymmetric) and they must have the same key usage.
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
-- For example:
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
-- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
-- To verify that the alias is mapped to the correct CMK, use 'ListAliases' .
-- * 'aliasName' - Identifies the alias that is changing its CMK. This value must begin with @alias/@ followed by the alias name, such as @alias/ExampleAlias@ . You cannot use UpdateAlias to change the alias name.
mkUpdateAlias ::
  -- | 'targetKeyId'
  Lude.Text ->
  -- | 'aliasName'
  Lude.Text ->
  UpdateAlias
mkUpdateAlias pTargetKeyId_ pAliasName_ =
  UpdateAlias'
    { targetKeyId = pTargetKeyId_,
      aliasName = pAliasName_
    }

-- | Identifies the CMK to associate with the alias. When the update operation completes, the alias will point to this CMK.
--
-- The CMK must be in the same AWS account and Region as the alias. Also, the new target CMK must be the same type as the current target CMK (both symmetric or both asymmetric) and they must have the same key usage.
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
-- For example:
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
-- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
-- To verify that the alias is mapped to the correct CMK, use 'ListAliases' .
--
-- /Note:/ Consider using 'targetKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaTargetKeyId :: Lens.Lens' UpdateAlias Lude.Text
uaTargetKeyId = Lens.lens (targetKeyId :: UpdateAlias -> Lude.Text) (\s a -> s {targetKeyId = a} :: UpdateAlias)
{-# DEPRECATED uaTargetKeyId "Use generic-lens or generic-optics with 'targetKeyId' instead." #-}

-- | Identifies the alias that is changing its CMK. This value must begin with @alias/@ followed by the alias name, such as @alias/ExampleAlias@ . You cannot use UpdateAlias to change the alias name.
--
-- /Note:/ Consider using 'aliasName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaAliasName :: Lens.Lens' UpdateAlias Lude.Text
uaAliasName = Lens.lens (aliasName :: UpdateAlias -> Lude.Text) (\s a -> s {aliasName = a} :: UpdateAlias)
{-# DEPRECATED uaAliasName "Use generic-lens or generic-optics with 'aliasName' instead." #-}

instance Lude.AWSRequest UpdateAlias where
  type Rs UpdateAlias = UpdateAliasResponse
  request = Req.postJSON kmsService
  response = Res.receiveNull UpdateAliasResponse'

instance Lude.ToHeaders UpdateAlias where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.UpdateAlias" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateAlias where
  toJSON UpdateAlias' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TargetKeyId" Lude..= targetKeyId),
            Lude.Just ("AliasName" Lude..= aliasName)
          ]
      )

instance Lude.ToPath UpdateAlias where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateAlias where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateAliasResponse' smart constructor.
data UpdateAliasResponse = UpdateAliasResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAliasResponse' with the minimum fields required to make a request.
mkUpdateAliasResponse ::
  UpdateAliasResponse
mkUpdateAliasResponse = UpdateAliasResponse'
