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
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a display name for a customer master key. An alias can be used to identify a key and should be unique. The console enforces a one-to-one mapping between the alias and a key. An alias name can contain only alphanumeric characters, forward slashes (/), underscores (_), and dashes (-). An alias must start with the word "alias" followed by a forward slash (alias/). An alias that begins with "aws" after the forward slash (alias/aws...) is reserved by Amazon Web Services (AWS).
--
--
-- The alias and the key it is mapped to must be in the same AWS account and the same region.
--
-- To map an alias to a different key, call 'UpdateAlias' .
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
  { _caAliasName   :: {-# NOUNPACK #-}!Text
  , _caTargetKeyId :: {-# NOUNPACK #-}!Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAlias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caAliasName' - String that contains the display name. The name must start with the word "alias" followed by a forward slash (alias/). Aliases that begin with "alias/AWS" are reserved.
--
-- * 'caTargetKeyId' - An identifier of the key for which you are creating the alias. This value cannot be another alias but can be a globally unique identifier or a fully specified ARN to a key.     * Key ARN Example - arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012     * Globally Unique Key ID Example - 12345678-1234-1234-1234-123456789012
createAlias
    :: Text -- ^ 'caAliasName'
    -> Text -- ^ 'caTargetKeyId'
    -> CreateAlias
createAlias pAliasName_ pTargetKeyId_ =
  CreateAlias' {_caAliasName = pAliasName_, _caTargetKeyId = pTargetKeyId_}


-- | String that contains the display name. The name must start with the word "alias" followed by a forward slash (alias/). Aliases that begin with "alias/AWS" are reserved.
caAliasName :: Lens' CreateAlias Text
caAliasName = lens _caAliasName (\ s a -> s{_caAliasName = a});

-- | An identifier of the key for which you are creating the alias. This value cannot be another alias but can be a globally unique identifier or a fully specified ARN to a key.     * Key ARN Example - arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012     * Globally Unique Key ID Example - 12345678-1234-1234-1234-123456789012
caTargetKeyId :: Lens' CreateAlias Text
caTargetKeyId = lens _caTargetKeyId (\ s a -> s{_caTargetKeyId = a});

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
