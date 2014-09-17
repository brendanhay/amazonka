{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ImportKeyPair
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Imports the public key from an RSA key pair that you created with a
-- third-party tool. Compare this with CreateKeyPair, in which AWS creates the
-- key pair and gives the keys to you (AWS keeps a copy of the public key).
-- With ImportKeyPair, you create the key pair and give AWS just the public
-- key. The private key is never transferred between you and AWS. For more
-- information about key pairs, see Key Pairs in the Amazon Elastic Compute
-- Cloud User Guide. Example This example imports the public key named
-- my-key-pair. https://ec2.amazonaws.com/?Action=ImportKeyPair
-- &amp;KeyName=my-key-pair
-- &amp;PublicKeyMaterial=MIICiTCCAfICCQD6m7oRw0uXOjANBgkqhkiG9w0BAQUFADCBiDELMAkGA1UEBhMC
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
-- NYiytVbZPQUQ5Yaxu2jXnimvw3rrszlaEXAMPLE &amp;AUTHPARAMS
-- &lt;ImportKeyPairResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;keyName&gt;my-key-pair&lt;/keyName&gt;
-- &lt;keyFingerprint&gt;1f:51:ae:28:bf:89:e9:d8:1f:25:5d:37:2d:7d:b8:ca:9f:f5:f1:6f&lt;/keyFingerprint&gt;
-- &lt;/ImportKeyPairResponse&gt;.
module Network.AWS.EC2.ImportKeyPair
    (
    -- * Request
      ImportKeyPair
    -- ** Request constructor
    , mkImportKeyPair
    -- ** Request lenses
    , ikpKeyName
    , ikpPublicKeyMaterial

    -- * Response
    , ImportKeyPairResponse
    -- ** Response constructor
    , mkImportKeyPairResponse
    -- ** Response lenses
    , ikprKeyName
    , ikprKeyFingerprint
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data ImportKeyPair = ImportKeyPair
    { _ikpKeyName :: Text
    , _ikpPublicKeyMaterial :: ByteString
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ImportKeyPair' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @KeyName ::@ @Text@
--
-- * @PublicKeyMaterial ::@ @ByteString@
--
mkImportKeyPair :: Text -- ^ 'ikpKeyName'
                -> ByteString -- ^ 'ikpPublicKeyMaterial'
                -> ImportKeyPair
mkImportKeyPair p1 p2 = ImportKeyPair
    { _ikpKeyName = p1
    , _ikpPublicKeyMaterial = p2
    }

-- | A unique name for the key pair.
ikpKeyName :: Lens' ImportKeyPair Text
ikpKeyName = lens _ikpKeyName (\s a -> s { _ikpKeyName = a })

-- | The public key. You must base64 encode the public key material before
-- sending it to AWS.
ikpPublicKeyMaterial :: Lens' ImportKeyPair ByteString
ikpPublicKeyMaterial =
    lens _ikpPublicKeyMaterial (\s a -> s { _ikpPublicKeyMaterial = a })

instance ToQuery ImportKeyPair where
    toQuery = genericQuery def

data ImportKeyPairResponse = ImportKeyPairResponse
    { _ikprKeyName :: Maybe Text
    , _ikprKeyFingerprint :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ImportKeyPairResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @KeyName ::@ @Maybe Text@
--
-- * @KeyFingerprint ::@ @Maybe Text@
--
mkImportKeyPairResponse :: ImportKeyPairResponse
mkImportKeyPairResponse = ImportKeyPairResponse
    { _ikprKeyName = Nothing
    , _ikprKeyFingerprint = Nothing
    }

-- | The key pair name you provided.
ikprKeyName :: Lens' ImportKeyPairResponse (Maybe Text)
ikprKeyName = lens _ikprKeyName (\s a -> s { _ikprKeyName = a })

-- | The MD5 public key fingerprint as specified in section 4 of RFC 4716.
ikprKeyFingerprint :: Lens' ImportKeyPairResponse (Maybe Text)
ikprKeyFingerprint =
    lens _ikprKeyFingerprint (\s a -> s { _ikprKeyFingerprint = a })

instance FromXML ImportKeyPairResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ImportKeyPair where
    type Sv ImportKeyPair = EC2
    type Rs ImportKeyPair = ImportKeyPairResponse

    request = post "ImportKeyPair"
    response _ = xmlResponse
