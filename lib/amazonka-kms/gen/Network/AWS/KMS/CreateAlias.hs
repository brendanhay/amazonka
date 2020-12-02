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
-- Module      : Network.AWS.KMS.CreateAlias
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a display name for a customer master key (CMK). You can use an alias to identify a CMK in selected operations, such as 'Encrypt' and 'GenerateDataKey' .
--
--
-- Each CMK can have multiple aliases, but each alias points to only one CMK. The alias name must be unique in the AWS account and region. To simplify code that runs in multiple regions, use the same alias name, but point it to a different CMK in each region.
--
-- Because an alias is not a property of a CMK, you can delete and change the aliases of a CMK without affecting the CMK. Also, aliases do not appear in the response from the 'DescribeKey' operation. To get the aliases of all CMKs, use the 'ListAliases' operation.
--
-- An alias must start with the word @alias@ followed by a forward slash (@alias/@ ). The alias name can contain only alphanumeric characters, forward slashes (/), underscores (_), and dashes (-). Alias names cannot begin with @aws@ ; that alias name prefix is reserved by Amazon Web Services (AWS).
--
-- The alias and the CMK it is mapped to must be in the same AWS account and the same region. You cannot perform this operation on an alias in a different AWS account.
--
-- To map an existing alias to a different CMK, call 'UpdateAlias' .
--
module Network.AWS.KMS.CreateAlias
    (
    -- * Creating a Request
      createAlias
    , CreateAlias
    -- * Request Lenses
    , caAliasName
    , caTargetKeyId

    -- * Destructuring the Response
    , createAliasResponse
    , CreateAliasResponse
    ) where

import Network.AWS.KMS.Types
import Network.AWS.KMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createAlias' smart constructor.
data CreateAlias = CreateAlias'
  { _caAliasName   :: !Text
  , _caTargetKeyId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAlias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caAliasName' - String that contains the display name. The name must start with the word "alias" followed by a forward slash (alias/). Aliases that begin with "alias/AWS" are reserved.
--
-- * 'caTargetKeyId' - Identifies the CMK for which you are creating the alias. This value cannot be an alias. Specify the key ID or the Amazon Resource Name (ARN) of the CMK. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
createAlias
    :: Text -- ^ 'caAliasName'
    -> Text -- ^ 'caTargetKeyId'
    -> CreateAlias
createAlias pAliasName_ pTargetKeyId_ =
  CreateAlias' {_caAliasName = pAliasName_, _caTargetKeyId = pTargetKeyId_}


-- | String that contains the display name. The name must start with the word "alias" followed by a forward slash (alias/). Aliases that begin with "alias/AWS" are reserved.
caAliasName :: Lens' CreateAlias Text
caAliasName = lens _caAliasName (\ s a -> s{_caAliasName = a})

-- | Identifies the CMK for which you are creating the alias. This value cannot be an alias. Specify the key ID or the Amazon Resource Name (ARN) of the CMK. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
caTargetKeyId :: Lens' CreateAlias Text
caTargetKeyId = lens _caTargetKeyId (\ s a -> s{_caTargetKeyId = a})

instance AWSRequest CreateAlias where
        type Rs CreateAlias = CreateAliasResponse
        request = postJSON kms
        response = receiveNull CreateAliasResponse'

instance Hashable CreateAlias where

instance NFData CreateAlias where

instance ToHeaders CreateAlias where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.CreateAlias" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateAlias where
        toJSON CreateAlias'{..}
          = object
              (catMaybes
                 [Just ("AliasName" .= _caAliasName),
                  Just ("TargetKeyId" .= _caTargetKeyId)])

instance ToPath CreateAlias where
        toPath = const "/"

instance ToQuery CreateAlias where
        toQuery = const mempty

-- | /See:/ 'createAliasResponse' smart constructor.
data CreateAliasResponse =
  CreateAliasResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAliasResponse' with the minimum fields required to make a request.
--
createAliasResponse
    :: CreateAliasResponse
createAliasResponse = CreateAliasResponse'


instance NFData CreateAliasResponse where
