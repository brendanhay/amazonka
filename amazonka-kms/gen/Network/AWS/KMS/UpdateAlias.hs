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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an alias to associate it with a different key.
--
-- An alias name can contain only alphanumeric characters, forward slashes
-- (\/), underscores (_), and dashes (-). An alias must start with the word
-- \"alias\" followed by a forward slash (alias\/). An alias that begins
-- with \"aws\" after the forward slash (alias\/aws...) is reserved by
-- Amazon Web Services (AWS).
--
-- An alias is not a property of a key. Therefore, an alias can be
-- associated with and disassociated from an existing key without changing
-- the properties of the key.
--
-- Note that you cannot create or update an alias that represents a key in
-- another account.
--
-- /See:/ <http://docs.aws.amazon.com/kms/latest/APIReference/API_UpdateAlias.html AWS API Reference> for UpdateAlias.
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

import           Network.AWS.KMS.Types
import           Network.AWS.KMS.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateAlias' smart constructor.
data UpdateAlias = UpdateAlias'
    { _uaAliasName   :: !Text
    , _uaTargetKeyId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateAlias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaAliasName'
--
-- * 'uaTargetKeyId'
updateAlias
    :: Text -- ^ 'uaAliasName'
    -> Text -- ^ 'uaTargetKeyId'
    -> UpdateAlias
updateAlias pAliasName_ pTargetKeyId_ =
    UpdateAlias'
    { _uaAliasName = pAliasName_
    , _uaTargetKeyId = pTargetKeyId_
    }

-- | String that contains the name of the alias to be modifed. The name must
-- start with the word \"alias\" followed by a forward slash (alias\/).
-- Aliases that begin with \"alias\/AWS\" are reserved.
uaAliasName :: Lens' UpdateAlias Text
uaAliasName = lens _uaAliasName (\ s a -> s{_uaAliasName = a});

-- | Unique identifier of the customer master key to be associated with the
-- alias. This value can be a globally unique identifier or the fully
-- specified ARN of a key.
--
-- -   Key ARN Example -
--     arn:aws:kms:us-east-1:123456789012:key\/12345678-1234-1234-1234-123456789012
-- -   Globally Unique Key ID Example -
--     12345678-1234-1234-1234-123456789012
uaTargetKeyId :: Lens' UpdateAlias Text
uaTargetKeyId = lens _uaTargetKeyId (\ s a -> s{_uaTargetKeyId = a});

instance AWSRequest UpdateAlias where
        type Sv UpdateAlias = KMS
        type Rs UpdateAlias = UpdateAliasResponse
        request = postJSON
        response = receiveNull UpdateAliasResponse'

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
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateAliasResponse' with the minimum fields required to make a request.
--
updateAliasResponse
    :: UpdateAliasResponse
updateAliasResponse = UpdateAliasResponse'
