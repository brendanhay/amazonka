{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.KMS.CreateKey
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a customer master key. Customer master keys can be used to encrypt
-- small amounts of data (less than 4K) directly, but they are most commonly
-- used to encrypt or envelope data keys that are then used to encrypt
-- customer data. For more information about data keys, see GenerateDataKey>
-- and GenerateDataKeyWithoutPlaintext>.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html>
module Network.AWS.KMS.CreateKey
    (
    -- * Request
      CreateKey
    -- ** Request constructor
    , createKey
    -- ** Request lenses
    , ckDescription
    , ckKeyUsage
    , ckPolicy

    -- * Response
    , CreateKeyResponse
    -- ** Response constructor
    , createKeyResponse
    -- ** Response lenses
    , ckrKeyMetadata
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.KMS.Types
import qualified GHC.Exts

data CreateKey = CreateKey
    { _ckDescription :: Maybe Text
    , _ckKeyUsage    :: Maybe KeyUsageType
    , _ckPolicy      :: Maybe Text
    } deriving (Eq, Show)

-- | 'CreateKey' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ckDescription' @::@ 'Maybe' 'Text'
--
-- * 'ckKeyUsage' @::@ 'Maybe' 'KeyUsageType'
--
-- * 'ckPolicy' @::@ 'Maybe' 'Text'
--
createKey :: CreateKey
createKey = CreateKey
    { _ckPolicy      = Nothing
    , _ckDescription = Nothing
    , _ckKeyUsage    = Nothing
    }

-- | Description of the key. We recommend that you choose a description that
-- helps your customer decide whether the key is appropriate for a task.
ckDescription :: Lens' CreateKey (Maybe Text)
ckDescription = lens _ckDescription (\s a -> s { _ckDescription = a })

-- | Specifies the intended use of the key. Currently this defaults to
-- ENCRYPT/DECRYPT, and only symmetric encryption and decryption are
-- supported.
ckKeyUsage :: Lens' CreateKey (Maybe KeyUsageType)
ckKeyUsage = lens _ckKeyUsage (\s a -> s { _ckKeyUsage = a })

-- | Policy to be attached to the key. This is required and delegates back to
-- the account. The key is the root of trust.
ckPolicy :: Lens' CreateKey (Maybe Text)
ckPolicy = lens _ckPolicy (\s a -> s { _ckPolicy = a })

newtype CreateKeyResponse = CreateKeyResponse
    { _ckrKeyMetadata :: Maybe KeyMetadata
    } deriving (Eq, Show)

-- | 'CreateKeyResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ckrKeyMetadata' @::@ 'Maybe' 'KeyMetadata'
--
createKeyResponse :: CreateKeyResponse
createKeyResponse = CreateKeyResponse
    { _ckrKeyMetadata = Nothing
    }

-- | Metadata associated with the key.
ckrKeyMetadata :: Lens' CreateKeyResponse (Maybe KeyMetadata)
ckrKeyMetadata = lens _ckrKeyMetadata (\s a -> s { _ckrKeyMetadata = a })

instance ToPath CreateKey where
    toPath = const "/"

instance ToQuery CreateKey where
    toQuery = const mempty

instance ToHeaders CreateKey

instance ToJSON CreateKey where
    toJSON CreateKey{..} = object
        [ "Policy"      .= _ckPolicy
        , "Description" .= _ckDescription
        , "KeyUsage"    .= _ckKeyUsage
        ]

instance AWSRequest CreateKey where
    type Sv CreateKey = KMS
    type Rs CreateKey = CreateKeyResponse

    request  = post "CreateKey"
    response = jsonResponse

instance FromJSON CreateKeyResponse where
    parseJSON = withObject "CreateKeyResponse" $ \o -> CreateKeyResponse
        <$> o .:? "KeyMetadata"
