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
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an alias to map it to a different key.
--
--
-- An alias is not a property of a key. Therefore, an alias can be mapped to and unmapped from an existing key without changing the properties of the key.
--
-- An alias name can contain only alphanumeric characters, forward slashes (/), underscores (_), and dashes (-). An alias must start with the word "alias" followed by a forward slash (alias/). An alias that begins with "aws" after the forward slash (alias/aws...) is reserved by Amazon Web Services (AWS).
--
-- The alias and the key it is mapped to must be in the same AWS account and the same region.
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
  { _uaAliasName   :: {-# NOUNPACK #-}!Text
  , _uaTargetKeyId :: {-# NOUNPACK #-}!Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAlias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaAliasName' - String that contains the name of the alias to be modified. The name must start with the word "alias" followed by a forward slash (alias/). Aliases that begin with "alias/aws" are reserved.
--
-- * 'uaTargetKeyId' - Unique identifier of the customer master key to be mapped to the alias. This value can be a globally unique identifier or the fully specified ARN of a key.     * Key ARN Example - arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012     * Globally Unique Key ID Example - 12345678-1234-1234-1234-123456789012 You can call 'ListAliases' to verify that the alias is mapped to the correct @TargetKeyId@ .
updateAlias
    :: Text -- ^ 'uaAliasName'
    -> Text -- ^ 'uaTargetKeyId'
    -> UpdateAlias
updateAlias pAliasName_ pTargetKeyId_ =
  UpdateAlias' {_uaAliasName = pAliasName_, _uaTargetKeyId = pTargetKeyId_}


-- | String that contains the name of the alias to be modified. The name must start with the word "alias" followed by a forward slash (alias/). Aliases that begin with "alias/aws" are reserved.
uaAliasName :: Lens' UpdateAlias Text
uaAliasName = lens _uaAliasName (\ s a -> s{_uaAliasName = a});

-- | Unique identifier of the customer master key to be mapped to the alias. This value can be a globally unique identifier or the fully specified ARN of a key.     * Key ARN Example - arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012     * Globally Unique Key ID Example - 12345678-1234-1234-1234-123456789012 You can call 'ListAliases' to verify that the alias is mapped to the correct @TargetKeyId@ .
uaTargetKeyId :: Lens' UpdateAlias Text
uaTargetKeyId = lens _uaTargetKeyId (\ s a -> s{_uaTargetKeyId = a});

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
