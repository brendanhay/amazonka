{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
-- create a key pair that is available in all regions, use ImportKeyPair. For
-- more information about key pairs, see Key Pairs in the Amazon Elastic
-- Compute Cloud User Guide.
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
    , KeyPair
    -- ** Response constructor
    , keyPair
    -- ** Response lenses
    , kpKeyFingerprint
    , kpKeyMaterial
    , kpKeyName
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data CreateKeyPair = CreateKeyPair
    { _ckpDryRun  :: Maybe Bool
    , _ckpKeyName :: Text
    } (Eq, Ord, Show, Generic)

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
instance ToQuery CreateKeyPair

instance ToPath CreateKeyPair where
    toPath = const "/"

data KeyPair = KeyPair
    { _kpKeyFingerprint :: Maybe Text
    , _kpKeyMaterial    :: Maybe Text
    , _kpKeyName        :: Maybe Text
    } (Eq, Ord, Show, Generic)

-- | 'KeyPair' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'kpKeyFingerprint' @::@ 'Maybe' 'Text'
--
-- * 'kpKeyMaterial' @::@ 'Maybe' 'Text'
--
-- * 'kpKeyName' @::@ 'Maybe' 'Text'
--
keyPair :: KeyPair
keyPair = KeyPair
    { _kpKeyName        = Nothing
    , _kpKeyFingerprint = Nothing
    , _kpKeyMaterial    = Nothing
    }

-- | The SHA-1 digest of the DER encoded private key.
kpKeyFingerprint :: Lens' KeyPair (Maybe Text)
kpKeyFingerprint = lens _kpKeyFingerprint (\s a -> s { _kpKeyFingerprint = a })

-- | An unencrypted PEM encoded RSA private key.
kpKeyMaterial :: Lens' KeyPair (Maybe Text)
kpKeyMaterial = lens _kpKeyMaterial (\s a -> s { _kpKeyMaterial = a })

-- | The name of the key pair.
kpKeyName :: Lens' KeyPair (Maybe Text)
kpKeyName = lens _kpKeyName (\s a -> s { _kpKeyName = a })

instance FromXML KeyPair where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "KeyPair"

instance AWSRequest CreateKeyPair where
    type Sv CreateKeyPair = EC2
    type Rs CreateKeyPair = KeyPair

    request  = post "CreateKeyPair"
    response = xmlResponse $ \h x -> KeyPair
        <$> x %| "keyFingerprint"
        <*> x %| "keyMaterial"
        <*> x %| "keyName"
