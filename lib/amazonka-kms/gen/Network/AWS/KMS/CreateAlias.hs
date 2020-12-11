{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.CreateAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a display name for a customer managed customer master key (CMK). You can use an alias to identify a CMK in <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations> , such as 'Encrypt' and 'GenerateDataKey' . You can change the CMK associated with the alias at any time.
--
-- Aliases are easier to remember than key IDs. They can also help to simplify your applications. For example, if you use an alias in your code, you can change the CMK your code uses by associating a given alias with a different CMK.
-- To run the same code in multiple AWS regions, use an alias in your code, such as @alias/ApplicationKey@ . Then, in each AWS Region, create an @alias/ApplicationKey@ alias that is associated with a CMK in that Region. When you run your code, it uses the @alias/ApplicationKey@ CMK for that AWS Region without any Region-specific code.
-- This operation does not return a response. To get the alias that you created, use the 'ListAliases' operation.
-- To use aliases successfully, be aware of the following information.
--
--     * Each alias points to only one CMK at a time, although a single CMK can have multiple aliases. The alias and its associated CMK must be in the same AWS account and Region.
--
--
--     * You can associate an alias with any customer managed CMK in the same AWS account and Region. However, you do not have permission to associate an alias with an <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#aws-managed-cmk AWS managed CMK> or an <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#aws-owned-cmk AWS owned CMK> .
--
--
--     * To change the CMK associated with an alias, use the 'UpdateAlias' operation. The current CMK and the new CMK must be the same type (both symmetric or both asymmetric) and they must have the same key usage (@ENCRYPT_DECRYPT@ or @SIGN_VERIFY@ ). This restriction prevents cryptographic errors in code that uses aliases.
--
--
--     * The alias name must begin with @alias/@ followed by a name, such as @alias/ExampleAlias@ . It can contain only alphanumeric characters, forward slashes (/), underscores (_), and dashes (-). The alias name cannot begin with @alias/aws/@ . The @alias/aws/@ prefix is reserved for <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#aws-managed-cmk AWS managed CMKs> .
--
--
--     * The alias name must be unique within an AWS Region. However, you can use the same alias name in multiple Regions of the same AWS account. Each instance of the alias is associated with a CMK in its Region.
--
--
--     * After you create an alias, you cannot change its alias name. However, you can use the 'DeleteAlias' operation to delete the alias and then create a new alias with the desired name.
--
--
--     * You can use an alias name or alias ARN to identify a CMK in AWS KMS <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations> and in the 'DescribeKey' operation. However, you cannot use alias names or alias ARNs in API operations that manage CMKs, such as 'DisableKey' or 'GetKeyPolicy' . For information about the valid CMK identifiers for each AWS KMS API operation, see the descriptions of the @KeyId@ parameter in the API operation documentation.
--
--
-- Because an alias is not a property of a CMK, you can delete and change the aliases of a CMK without affecting the CMK. Also, aliases do not appear in the response from the 'DescribeKey' operation. To get the aliases and alias ARNs of CMKs in each AWS account and Region, use the 'ListAliases' operation.
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.CreateAlias
  ( -- * Creating a request
    CreateAlias (..),
    mkCreateAlias,

    -- ** Request lenses
    caAliasName,
    caTargetKeyId,

    -- * Destructuring the response
    CreateAliasResponse (..),
    mkCreateAliasResponse,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateAlias' smart constructor.
data CreateAlias = CreateAlias'
  { aliasName :: Lude.Text,
    targetKeyId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAlias' with the minimum fields required to make a request.
--
-- * 'aliasName' - Specifies the alias name. This value must begin with @alias/@ followed by a name, such as @alias/ExampleAlias@ . The alias name cannot begin with @alias/aws/@ . The @alias/aws/@ prefix is reserved for AWS managed CMKs.
-- * 'targetKeyId' - Identifies the CMK to which the alias refers. Specify the key ID or the Amazon Resource Name (ARN) of the CMK. You cannot specify another alias. For help finding the key ID and ARN, see <https://docs.aws.amazon.com/kms/latest/developerguide/viewing-keys.html#find-cmk-id-arn Finding the Key ID and ARN> in the /AWS Key Management Service Developer Guide/ .
mkCreateAlias ::
  -- | 'aliasName'
  Lude.Text ->
  -- | 'targetKeyId'
  Lude.Text ->
  CreateAlias
mkCreateAlias pAliasName_ pTargetKeyId_ =
  CreateAlias'
    { aliasName = pAliasName_,
      targetKeyId = pTargetKeyId_
    }

-- | Specifies the alias name. This value must begin with @alias/@ followed by a name, such as @alias/ExampleAlias@ . The alias name cannot begin with @alias/aws/@ . The @alias/aws/@ prefix is reserved for AWS managed CMKs.
--
-- /Note:/ Consider using 'aliasName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAliasName :: Lens.Lens' CreateAlias Lude.Text
caAliasName = Lens.lens (aliasName :: CreateAlias -> Lude.Text) (\s a -> s {aliasName = a} :: CreateAlias)
{-# DEPRECATED caAliasName "Use generic-lens or generic-optics with 'aliasName' instead." #-}

-- | Identifies the CMK to which the alias refers. Specify the key ID or the Amazon Resource Name (ARN) of the CMK. You cannot specify another alias. For help finding the key ID and ARN, see <https://docs.aws.amazon.com/kms/latest/developerguide/viewing-keys.html#find-cmk-id-arn Finding the Key ID and ARN> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'targetKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTargetKeyId :: Lens.Lens' CreateAlias Lude.Text
caTargetKeyId = Lens.lens (targetKeyId :: CreateAlias -> Lude.Text) (\s a -> s {targetKeyId = a} :: CreateAlias)
{-# DEPRECATED caTargetKeyId "Use generic-lens or generic-optics with 'targetKeyId' instead." #-}

instance Lude.AWSRequest CreateAlias where
  type Rs CreateAlias = CreateAliasResponse
  request = Req.postJSON kmsService
  response = Res.receiveNull CreateAliasResponse'

instance Lude.ToHeaders CreateAlias where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.CreateAlias" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateAlias where
  toJSON CreateAlias' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AliasName" Lude..= aliasName),
            Lude.Just ("TargetKeyId" Lude..= targetKeyId)
          ]
      )

instance Lude.ToPath CreateAlias where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateAlias where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateAliasResponse' smart constructor.
data CreateAliasResponse = CreateAliasResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAliasResponse' with the minimum fields required to make a request.
mkCreateAliasResponse ::
  CreateAliasResponse
mkCreateAliasResponse = CreateAliasResponse'
