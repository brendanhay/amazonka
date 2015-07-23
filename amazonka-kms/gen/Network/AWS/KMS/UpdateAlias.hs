{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.UpdateAlias
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_UpdateAlias.html>
module Network.AWS.KMS.UpdateAlias
    (
    -- * Request
      UpdateAlias
    -- ** Request constructor
    , updateAlias
    -- ** Request lenses
    , uarqAliasName
    , uarqTargetKeyId

    -- * Response
    , UpdateAliasResponse
    -- ** Response constructor
    , updateAliasResponse
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateAlias' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uarqAliasName'
--
-- * 'uarqTargetKeyId'
data UpdateAlias = UpdateAlias'
    { _uarqAliasName   :: !Text
    , _uarqTargetKeyId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateAlias' smart constructor.
updateAlias :: Text -> Text -> UpdateAlias
updateAlias pAliasName_ pTargetKeyId_ =
    UpdateAlias'
    { _uarqAliasName = pAliasName_
    , _uarqTargetKeyId = pTargetKeyId_
    }

-- | String that contains the name of the alias to be modifed. The name must
-- start with the word \"alias\" followed by a forward slash (alias\/).
-- Aliases that begin with \"alias\/AWS\" are reserved.
uarqAliasName :: Lens' UpdateAlias Text
uarqAliasName = lens _uarqAliasName (\ s a -> s{_uarqAliasName = a});

-- | Unique identifier of the customer master key to be associated with the
-- alias. This value can be a globally unique identifier or the fully
-- specified ARN of a key.
--
-- -   Key ARN Example -
--     arn:aws:kms:us-east-1:123456789012:key\/12345678-1234-1234-1234-123456789012
-- -   Globally Unique Key ID Example -
--     12345678-1234-1234-1234-123456789012
uarqTargetKeyId :: Lens' UpdateAlias Text
uarqTargetKeyId = lens _uarqTargetKeyId (\ s a -> s{_uarqTargetKeyId = a});

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
              ["AliasName" .= _uarqAliasName,
               "TargetKeyId" .= _uarqTargetKeyId]

instance ToPath UpdateAlias where
        toPath = const "/"

instance ToQuery UpdateAlias where
        toQuery = const mempty

-- | /See:/ 'updateAliasResponse' smart constructor.
data UpdateAliasResponse =
    UpdateAliasResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateAliasResponse' smart constructor.
updateAliasResponse :: UpdateAliasResponse
updateAliasResponse = UpdateAliasResponse'
