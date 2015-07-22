{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.CreateAlias
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a display name for a customer master key. An alias can be used
-- to identify a key and should be unique. The console enforces a
-- one-to-one mapping between the alias and a key. An alias name can
-- contain only alphanumeric characters, forward slashes (\/), underscores
-- (_), and dashes (-). An alias must start with the word \"alias\"
-- followed by a forward slash (alias\/). An alias that begins with \"aws\"
-- after the forward slash (alias\/aws...) is reserved by Amazon Web
-- Services (AWS).
--
-- To associate an alias with a different key, call UpdateAlias.
--
-- Note that you cannot create or update an alias that represents a key in
-- another account.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_CreateAlias.html>
module Network.AWS.KMS.CreateAlias
    (
    -- * Request
      CreateAlias
    -- ** Request constructor
    , createAlias
    -- ** Request lenses
    , carqAliasName
    , carqTargetKeyId

    -- * Response
    , CreateAliasResponse
    -- ** Response constructor
    , createAliasResponse
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createAlias' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'carqAliasName'
--
-- * 'carqTargetKeyId'
data CreateAlias = CreateAlias'
    { _carqAliasName   :: !Text
    , _carqTargetKeyId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateAlias' smart constructor.
createAlias :: Text -> Text -> CreateAlias
createAlias pAliasName_ pTargetKeyId_ =
    CreateAlias'
    { _carqAliasName = pAliasName_
    , _carqTargetKeyId = pTargetKeyId_
    }

-- | String that contains the display name. The name must start with the word
-- \"alias\" followed by a forward slash (alias\/). Aliases that begin with
-- \"alias\/AWS\" are reserved.
carqAliasName :: Lens' CreateAlias Text
carqAliasName = lens _carqAliasName (\ s a -> s{_carqAliasName = a});

-- | An identifier of the key for which you are creating the alias. This
-- value cannot be another alias but can be a globally unique identifier or
-- a fully specified ARN to a key.
--
-- -   Key ARN Example -
--     arn:aws:kms:us-east-1:123456789012:key\/12345678-1234-1234-1234-123456789012
-- -   Globally Unique Key ID Example -
--     12345678-1234-1234-1234-123456789012
carqTargetKeyId :: Lens' CreateAlias Text
carqTargetKeyId = lens _carqTargetKeyId (\ s a -> s{_carqTargetKeyId = a});

instance AWSRequest CreateAlias where
        type Sv CreateAlias = KMS
        type Rs CreateAlias = CreateAliasResponse
        request = postJSON
        response = receiveNull CreateAliasResponse'

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
              ["AliasName" .= _carqAliasName,
               "TargetKeyId" .= _carqTargetKeyId]

instance ToPath CreateAlias where
        toPath = const "/"

instance ToQuery CreateAlias where
        toQuery = const mempty

-- | /See:/ 'createAliasResponse' smart constructor.
data CreateAliasResponse =
    CreateAliasResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateAliasResponse' smart constructor.
createAliasResponse :: CreateAliasResponse
createAliasResponse = CreateAliasResponse'
