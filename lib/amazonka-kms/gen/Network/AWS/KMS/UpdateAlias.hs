{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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
--
-- The current and new CMK must be the same type (both symmetric or both asymmetric), and they must have the same key usage (@ENCRYPT_DECRYPT@ or @SIGN_VERIFY@ ). This restriction prevents errors in code that uses aliases. If you must assign an alias to a different type of CMK, use 'DeleteAlias' to delete the old alias and 'CreateAlias' to create a new alias.
--
-- You cannot use @UpdateAlias@ to change an alias name. To change an alias name, use 'DeleteAlias' to delete the old alias and 'CreateAlias' to create a new alias.
--
-- Because an alias is not a property of a CMK, you can create, update, and delete the aliases of a CMK without affecting the CMK. Also, aliases do not appear in the response from the 'DescribeKey' operation. To get the aliases of all CMKs in the account, use the 'ListAliases' operation.
--
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.UpdateAlias
  ( -- * Creating a Request
    updateAlias,
    UpdateAlias,

    -- * Request Lenses
    uaAliasName,
    uaTargetKeyId,

    -- * Destructuring the Response
    updateAliasResponse,
    UpdateAliasResponse,
  )
where

import Network.AWS.KMS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateAlias' smart constructor.
data UpdateAlias = UpdateAlias'
  { _uaAliasName :: !Text,
    _uaTargetKeyId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateAlias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaAliasName' - Identifies the alias that is changing its CMK. This value must begin with @alias/@ followed by the alias name, such as @alias/ExampleAlias@ . You cannot use UpdateAlias to change the alias name.
--
-- * 'uaTargetKeyId' - Identifies the CMK to associate with the alias. When the update operation completes, the alias will point to this CMK.  The CMK must be in the same AWS account and Region as the alias. Also, the new target CMK must be the same type as the current target CMK (both symmetric or both asymmetric) and they must have the same key usage.  Specify the key ID or the Amazon Resource Name (ARN) of the CMK. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' . To verify that the alias is mapped to the correct CMK, use 'ListAliases' .
updateAlias ::
  -- | 'uaAliasName'
  Text ->
  -- | 'uaTargetKeyId'
  Text ->
  UpdateAlias
updateAlias pAliasName_ pTargetKeyId_ =
  UpdateAlias'
    { _uaAliasName = pAliasName_,
      _uaTargetKeyId = pTargetKeyId_
    }

-- | Identifies the alias that is changing its CMK. This value must begin with @alias/@ followed by the alias name, such as @alias/ExampleAlias@ . You cannot use UpdateAlias to change the alias name.
uaAliasName :: Lens' UpdateAlias Text
uaAliasName = lens _uaAliasName (\s a -> s {_uaAliasName = a})

-- | Identifies the CMK to associate with the alias. When the update operation completes, the alias will point to this CMK.  The CMK must be in the same AWS account and Region as the alias. Also, the new target CMK must be the same type as the current target CMK (both symmetric or both asymmetric) and they must have the same key usage.  Specify the key ID or the Amazon Resource Name (ARN) of the CMK. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' . To verify that the alias is mapped to the correct CMK, use 'ListAliases' .
uaTargetKeyId :: Lens' UpdateAlias Text
uaTargetKeyId = lens _uaTargetKeyId (\s a -> s {_uaTargetKeyId = a})

instance AWSRequest UpdateAlias where
  type Rs UpdateAlias = UpdateAliasResponse
  request = postJSON kms
  response = receiveNull UpdateAliasResponse'

instance Hashable UpdateAlias

instance NFData UpdateAlias

instance ToHeaders UpdateAlias where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("TrentService.UpdateAlias" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateAlias where
  toJSON UpdateAlias' {..} =
    object
      ( catMaybes
          [ Just ("AliasName" .= _uaAliasName),
            Just ("TargetKeyId" .= _uaTargetKeyId)
          ]
      )

instance ToPath UpdateAlias where
  toPath = const "/"

instance ToQuery UpdateAlias where
  toQuery = const mempty

-- | /See:/ 'updateAliasResponse' smart constructor.
data UpdateAliasResponse = UpdateAliasResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateAliasResponse' with the minimum fields required to make a request.
updateAliasResponse ::
  UpdateAliasResponse
updateAliasResponse = UpdateAliasResponse'

instance NFData UpdateAliasResponse
