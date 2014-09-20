{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
module Network.AWS.EC2.CreateKeyPair
    (
    -- * Request
      CreateKeyPair
    -- ** Request constructor
    , createKeyPair
    -- ** Request lenses
    , ckpKeyName

    -- * Response
    , CreateKeyPairResponse
    -- ** Response constructor
    , createKeyPairResponse
    -- ** Response lenses
    , ckprKeyName
    , ckprKeyFingerprint
    , ckprKeyMaterial
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

newtype CreateKeyPair = CreateKeyPair
    { _ckpKeyName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateKeyPair' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @KeyName ::@ @Text@
--
createKeyPair :: Text -- ^ 'ckpKeyName'
              -> CreateKeyPair
createKeyPair p1 = CreateKeyPair
    { _ckpKeyName = p1
    }

-- | A unique name for the key pair.
ckpKeyName :: Lens' CreateKeyPair Text
ckpKeyName = lens _ckpKeyName (\s a -> s { _ckpKeyName = a })

instance ToQuery CreateKeyPair where
    toQuery = genericQuery def

data CreateKeyPairResponse = CreateKeyPairResponse
    { _ckprKeyName :: Text
    , _ckprKeyFingerprint :: Text
    , _ckprKeyMaterial :: Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateKeyPairResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @KeyName ::@ @Text@
--
-- * @KeyFingerprint ::@ @Text@
--
-- * @KeyMaterial ::@ @Text@
--
createKeyPairResponse :: Text -- ^ 'ckprKeyName'
                      -> Text -- ^ 'ckprKeyFingerprint'
                      -> Text -- ^ 'ckprKeyMaterial'
                      -> CreateKeyPairResponse
createKeyPairResponse p1 p2 p3 = CreateKeyPairResponse
    { _ckprKeyName = p1
    , _ckprKeyFingerprint = p2
    , _ckprKeyMaterial = p3
    }

-- | The name of the key pair.
ckprKeyName :: Lens' CreateKeyPairResponse Text
ckprKeyName = lens _ckprKeyName (\s a -> s { _ckprKeyName = a })

-- | The SHA-1 digest of the DER encoded private key.
ckprKeyFingerprint :: Lens' CreateKeyPairResponse Text
ckprKeyFingerprint =
    lens _ckprKeyFingerprint (\s a -> s { _ckprKeyFingerprint = a })

-- | An unencrypted PEM encoded RSA private key.
ckprKeyMaterial :: Lens' CreateKeyPairResponse Text
ckprKeyMaterial = lens _ckprKeyMaterial (\s a -> s { _ckprKeyMaterial = a })

instance FromXML CreateKeyPairResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateKeyPair where
    type Sv CreateKeyPair = EC2
    type Rs CreateKeyPair = CreateKeyPairResponse

    request = post "CreateKeyPair"
    response _ = xmlResponse
