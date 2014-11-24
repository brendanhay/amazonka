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

-- Module      : Network.AWS.EC2.CreateKeyPair
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a 2048-bit RSA key pair with the specified name. Amazon EC2 stores
-- the public key and displays the private key for you to save to a file. The
-- private key is returned as an unencrypted PEM encoded PKCS#8 private key.
-- If a key with the specified name already exists, Amazon EC2 returns an
-- error. You can have up to five thousand key pairs per region. The key pair
-- returned to you is available only in the region in which you create it. To
-- create a key pair that is available in all regions, use 'ImportKeyPair'.
-- For more information about key pairs, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Key
-- Pairs> in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateKeyPair.html>
module Network.AWS.EC2.CreateKeyPair
    (
    -- * Request
      CreateKeyPair
    -- ** Request constructor
    , createKeyPair
    -- ** Request lenses
    , ckpDryRun
    , ckpKeyName

    -- * Response
    , CreateKeyPairResponse
    -- ** Response constructor
    , createKeyPairResponse
    -- ** Response lenses
    , ckprKeyFingerprint
    , ckprKeyMaterial
    , ckprKeyName
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data CreateKeyPair = CreateKeyPair
    { _ckpDryRun  :: Maybe Bool
    , _ckpKeyName :: Text
    } deriving (Eq, Ord, Show)

-- | 'CreateKeyPair' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ckpDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'ckpKeyName' @::@ 'Text'
--
createKeyPair :: Text -- ^ 'ckpKeyName'
              -> CreateKeyPair
createKeyPair p1 = CreateKeyPair
    { _ckpKeyName = p1
    , _ckpDryRun  = Nothing
    }

ckpDryRun :: Lens' CreateKeyPair (Maybe Bool)
ckpDryRun = lens _ckpDryRun (\s a -> s { _ckpDryRun = a })

-- | A unique name for the key pair. Constraints: Up to 255 ASCII characters.
ckpKeyName :: Lens' CreateKeyPair Text
ckpKeyName = lens _ckpKeyName (\s a -> s { _ckpKeyName = a })

data CreateKeyPairResponse = CreateKeyPairResponse
    { _ckprKeyFingerprint :: Text
    , _ckprKeyMaterial    :: Text
    , _ckprKeyName        :: Text
    } deriving (Eq, Ord, Show)

-- | 'CreateKeyPairResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ckprKeyFingerprint' @::@ 'Text'
--
-- * 'ckprKeyMaterial' @::@ 'Text'
--
-- * 'ckprKeyName' @::@ 'Text'
--
createKeyPairResponse :: Text -- ^ 'ckprKeyName'
                      -> Text -- ^ 'ckprKeyFingerprint'
                      -> Text -- ^ 'ckprKeyMaterial'
                      -> CreateKeyPairResponse
createKeyPairResponse p1 p2 p3 = CreateKeyPairResponse
    { _ckprKeyName        = p1
    , _ckprKeyFingerprint = p2
    , _ckprKeyMaterial    = p3
    }

-- | The SHA-1 digest of the DER encoded private key.
ckprKeyFingerprint :: Lens' CreateKeyPairResponse Text
ckprKeyFingerprint =
    lens _ckprKeyFingerprint (\s a -> s { _ckprKeyFingerprint = a })

-- | An unencrypted PEM encoded RSA private key.
ckprKeyMaterial :: Lens' CreateKeyPairResponse Text
ckprKeyMaterial = lens _ckprKeyMaterial (\s a -> s { _ckprKeyMaterial = a })

-- | The name of the key pair.
ckprKeyName :: Lens' CreateKeyPairResponse Text
ckprKeyName = lens _ckprKeyName (\s a -> s { _ckprKeyName = a })

instance ToPath CreateKeyPair where
    toPath = const "/"

instance ToQuery CreateKeyPair where
    toQuery CreateKeyPair{..} = mconcat
        [ "dryRun"  =? _ckpDryRun
        , "KeyName" =? _ckpKeyName
        ]

instance ToHeaders CreateKeyPair

instance AWSRequest CreateKeyPair where
    type Sv CreateKeyPair = EC2
    type Rs CreateKeyPair = CreateKeyPairResponse

    request  = post "CreateKeyPair"
    response = xmlResponse

instance FromXML CreateKeyPairResponse where
    parseXML x = CreateKeyPairResponse
        <$> x .@  "keyFingerprint"
        <*> x .@  "keyMaterial"
        <*> x .@  "keyName"
