{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.UpdateAlias
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an existing alias with a different customer master key (CMK). Each CMK can have multiple aliases, but the aliases must be unique within the account and region. You cannot perform this operation on an alias in a different AWS account.
--
--
-- This operation works only on existing aliases. To change the alias of a CMK to a new value, use 'CreateAlias' to create a new alias and 'DeleteAlias' to delete the old alias.
--
-- Because an alias is not a property of a CMK, you can create, update, and delete the aliases of a CMK without affecting the CMK. Also, aliases do not appear in the response from the 'DescribeKey' operation. To get the aliases of all CMKs in the account, use the 'ListAliases' operation.
--
-- An alias name can contain only alphanumeric characters, forward slashes (/), underscores (_), and dashes (-). An alias must start with the word @alias@ followed by a forward slash (@alias/@ ). The alias name can contain only alphanumeric characters, forward slashes (/), underscores (_), and dashes (-). Alias names cannot begin with @aws@ ; that alias name prefix is reserved by Amazon Web Services (AWS).
--
module Network.AWS.KMS.UpdateAlias
    (
    -- * Creating a Request
      updateAlias
    , UpdateAlias
    -- * Request Lenses
    , uaAliasName
    , uaTargetKeyId

    -- * Destructuring the Response
    , updateAliasResponse
    , UpdateAliasResponse
    ) where

import Network.AWS.KMS.Types
import Network.AWS.KMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateAlias' smart constructor.
data UpdateAlias = UpdateAlias'
  { _uaAliasName   :: !Text
  , _uaTargetKeyId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAlias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaAliasName' - String that contains the name of the alias to be modified. The name must start with the word "alias" followed by a forward slash (alias/). Aliases that begin with "alias/aws" are reserved.
--
-- * 'uaTargetKeyId' - Unique identifier of the customer master key to be mapped to the alias. Specify the key ID or the Amazon Resource Name (ARN) of the CMK. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' . To verify that the alias is mapped to the correct CMK, use 'ListAliases' .
updateAlias
    :: Text -- ^ 'uaAliasName'
    -> Text -- ^ 'uaTargetKeyId'
    -> UpdateAlias
updateAlias pAliasName_ pTargetKeyId_ =
  UpdateAlias' {_uaAliasName = pAliasName_, _uaTargetKeyId = pTargetKeyId_}


-- | String that contains the name of the alias to be modified. The name must start with the word "alias" followed by a forward slash (alias/). Aliases that begin with "alias/aws" are reserved.
uaAliasName :: Lens' UpdateAlias Text
uaAliasName = lens _uaAliasName (\ s a -> s{_uaAliasName = a})

-- | Unique identifier of the customer master key to be mapped to the alias. Specify the key ID or the Amazon Resource Name (ARN) of the CMK. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' . To verify that the alias is mapped to the correct CMK, use 'ListAliases' .
uaTargetKeyId :: Lens' UpdateAlias Text
uaTargetKeyId = lens _uaTargetKeyId (\ s a -> s{_uaTargetKeyId = a})

instance AWSRequest UpdateAlias where
        type Rs UpdateAlias = UpdateAliasResponse
        request = postJSON kms
        response = receiveNull UpdateAliasResponse'

instance Hashable UpdateAlias where

instance NFData UpdateAlias where

instance ToHeaders UpdateAlias where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.UpdateAlias" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateAlias where
        toJSON UpdateAlias'{..}
          = object
              (catMaybes
                 [Just ("AliasName" .= _uaAliasName),
                  Just ("TargetKeyId" .= _uaTargetKeyId)])

instance ToPath UpdateAlias where
        toPath = const "/"

instance ToQuery UpdateAlias where
        toQuery = const mempty

-- | /See:/ 'updateAliasResponse' smart constructor.
data UpdateAliasResponse =
  UpdateAliasResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAliasResponse' with the minimum fields required to make a request.
--
updateAliasResponse
    :: UpdateAliasResponse
updateAliasResponse = UpdateAliasResponse'


instance NFData UpdateAliasResponse where
