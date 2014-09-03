{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.CreateKeyPair
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
-- error. You can have up to five thousand key pairs per region. For more
-- information about key pairs, see Key Pairs in the Amazon Elastic Compute
-- Cloud User Guide. Example This example request creates a key pair named
-- my-key-pair. https://ec2.amazonaws.com/?Action=CreateKeyPair
-- &amp;KeyName=my-key-pair &amp;AUTHPARAMS &lt;CreateKeyPairResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt; my-key-pair
-- 1f:51:ae:28:bf:89:e9:d8:1f:25:5d:37:2d:7d:b8:ca:9f:f5:f1:6f ---- BEGIN RSA
-- PRIVATE KEY ----
-- MIICiTCCAfICCQD6m7oRw0uXOjANBgkqhkiG9w0BAQUFADCBiDELMAkGA1UEBhMC
-- VVMxCzAJBgNVBAgTAldBMRAwDgYDVQQHEwdTZWF0dGxlMQ8wDQYDVQQKEwZBbWF6
-- b24xFDASBgNVBAsTC0lBTSBDb25zb2xlMRIwEAYDVQQDEwlUZXN0Q2lsYWMxHzAd
-- BgkqhkiG9w0BCQEWEG5vb25lQGFtYXpvbi5jb20wHhcNMTEwNDI1MjA0NTIxWhcN
-- MTIwNDI0MjA0NTIxWjCBiDELMAkGA1UEBhMCVVMxCzAJBgNVBAgTAldBMRAwDgYD
-- VQQHEwdTZWF0dGxlMQ8wDQYDVQQKEwZBbWF6b24xFDASBgNVBAsTC0lBTSBDb25z
-- b2xlMRIwEAYDVQQDEwlUZXN0Q2lsYWMxHzAdBgkqhkiG9w0BCQEWEG5vb25lQGFt
-- YXpvbi5jb20wgZ8wDQYJKoZIhvcNAQEBBQADgY0AMIGJAoGBAMaK0dn+a4GmWIWJ
-- 21uUSfwfEvySWtC2XADZ4nB+BLYgVIk60CpiwsZ3G93vUEIO3IyNoH/f0wYK8m9T
-- rDHudUZg3qX4waLG5M43q7Wgc/MbQITxOUSQv7c7ugFFDzQGBzZswY6786m86gpE
-- Ibb3OhjZnzcvQAaRHhdlQWIMm2nrAgMBAAEwDQYJKoZIhvcNAQEFBQADgYEAtCu4
-- nUhVVxYUntneD9+h8Mg9q6q+auNKyExzyLwaxlAoo7TJHidbtS4J5iNmZgXL0Fkb
-- FFBjvSfpJIlJ00zbhNYS5f6GuoEDmFJl0ZxBHjJnyp378OD8uTs7fLvjx79LjSTb
-- NYiytVbZPQUQ5Yaxu2jXnimvw3rrszlaEXAMPLE -----END RSA PRIVATE KEY-----
-- Saving the File Create a file named my-key-pair.pem and paste the entire
-- key from the response into this file. Keep this file in a safe place; it is
-- required to decrypt login information when you connect to an instance that
-- you launched using this key pair. If you're using an SSH client on a Linux
-- computer to connect to your instance, use the following command to set the
-- permissions of your private key file so that only you can read it. chmod
-- 400 my-key-pair.pem.
module Network.AWS.EC2.V2014_06_15.CreateKeyPair
    (
    -- * Request
      CreateKeyPair
    -- ** Default constructor
    , createKeyPair
    -- ** Accessors and lenses
    , _ckprKeyName
    , ckprKeyName

    -- * Response
    , CreateKeyPairResponse
    -- ** Accessors and lenses
    , _ckpsKeyName
    , ckpsKeyName
    , _ckpsKeyFingerprint
    , ckpsKeyFingerprint
    , _ckpsKeyMaterial
    , ckpsKeyMaterial
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateKeyPair' request.
createKeyPair :: Text -- ^ 'ckprKeyName'
              -> CreateKeyPair
createKeyPair p1 = CreateKeyPair
    { _ckprKeyName = p1
    }

data CreateKeyPair = CreateKeyPair

makeSiglessLenses ''CreateKeyPair

instance ToQuery CreateKeyPair where
    toQuery = genericQuery def

data CreateKeyPairResponse = CreateKeyPairResponse
    { _ckpsKeyName :: Maybe Text
      -- ^ The name of the key pair.
    , _ckpsKeyFingerprint :: Maybe Text
      -- ^ The SHA-1 digest of the DER encoded private key.
    , _ckpsKeyMaterial :: Maybe Text
      -- ^ An unencrypted PEM encoded RSA private key.
    } deriving (Show, Generic)

makeSiglessLenses ''CreateKeyPairResponse

instance FromXML CreateKeyPairResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateKeyPair where
    type Sv CreateKeyPair = EC2
    type Rs CreateKeyPair = CreateKeyPairResponse

    request = post "CreateKeyPair"
    response _ = xmlResponse

-- | A unique name for the key pair.
ckprKeyName :: Lens' CreateKeyPair (Text)

-- | The name of the key pair.
ckpsKeyName :: Lens' CreateKeyPairResponse (Maybe Text)

-- | The SHA-1 digest of the DER encoded private key.
ckpsKeyFingerprint :: Lens' CreateKeyPairResponse (Maybe Text)

-- | An unencrypted PEM encoded RSA private key.
ckpsKeyMaterial :: Lens' CreateKeyPairResponse (Maybe Text)
