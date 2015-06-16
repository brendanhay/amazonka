{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.CreateKeyPair
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a 2048-bit RSA key pair with the specified name. Amazon EC2
-- stores the public key and displays the private key for you to save to a
-- file. The private key is returned as an unencrypted PEM encoded PKCS#8
-- private key. If a key with the specified name already exists, Amazon EC2
-- returns an error.
--
-- You can have up to five thousand key pairs per region.
--
-- The key pair returned to you is available only in the region in which
-- you create it. To create a key pair that is available in all regions,
-- use ImportKeyPair.
--
-- For more information about key pairs, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Key Pairs>
-- in the /Amazon Elastic Compute Cloud User Guide/.
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
    , ckprKeyName
    , ckprKeyMaterial
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.EC2.Types

-- | /See:/ 'createKeyPair' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ckpDryRun'
--
-- * 'ckpKeyName'
data CreateKeyPair = CreateKeyPair'{_ckpDryRun :: Maybe Bool, _ckpKeyName :: Text} deriving (Eq, Read, Show)

-- | 'CreateKeyPair' smart constructor.
createKeyPair :: Text -> CreateKeyPair
createKeyPair pKeyName = CreateKeyPair'{_ckpDryRun = Nothing, _ckpKeyName = pKeyName};

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
ckpDryRun :: Lens' CreateKeyPair (Maybe Bool)
ckpDryRun = lens _ckpDryRun (\ s a -> s{_ckpDryRun = a});

-- | A unique name for the key pair.
--
-- Constraints: Up to 255 ASCII characters
ckpKeyName :: Lens' CreateKeyPair Text
ckpKeyName = lens _ckpKeyName (\ s a -> s{_ckpKeyName = a});

instance AWSRequest CreateKeyPair where
        type Sv CreateKeyPair = EC2
        type Rs CreateKeyPair = CreateKeyPairResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 CreateKeyPairResponse' <$>
                   (x .@? "keyFingerprint") <*> (x .@? "keyName") <*>
                     (x .@? "keyMaterial"))

instance ToHeaders CreateKeyPair where
        toHeaders = const mempty

instance ToPath CreateKeyPair where
        toPath = const "/"

instance ToQuery CreateKeyPair where
        toQuery CreateKeyPair'{..}
          = mconcat
              ["Action" =: ("CreateKeyPair" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _ckpDryRun, "KeyName" =: _ckpKeyName]

-- | /See:/ 'createKeyPairResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ckprKeyFingerprint'
--
-- * 'ckprKeyName'
--
-- * 'ckprKeyMaterial'
data CreateKeyPairResponse = CreateKeyPairResponse'{_ckprKeyFingerprint :: Maybe Text, _ckprKeyName :: Maybe Text, _ckprKeyMaterial :: Maybe Text} deriving (Eq, Read, Show)

-- | 'CreateKeyPairResponse' smart constructor.
createKeyPairResponse :: CreateKeyPairResponse
createKeyPairResponse = CreateKeyPairResponse'{_ckprKeyFingerprint = Nothing, _ckprKeyName = Nothing, _ckprKeyMaterial = Nothing};

-- | The SHA-1 digest of the DER encoded private key.
ckprKeyFingerprint :: Lens' CreateKeyPairResponse (Maybe Text)
ckprKeyFingerprint = lens _ckprKeyFingerprint (\ s a -> s{_ckprKeyFingerprint = a});

-- | The name of the key pair.
ckprKeyName :: Lens' CreateKeyPairResponse (Maybe Text)
ckprKeyName = lens _ckprKeyName (\ s a -> s{_ckprKeyName = a});

-- | An unencrypted PEM encoded RSA private key.
ckprKeyMaterial :: Lens' CreateKeyPairResponse (Maybe Text)
ckprKeyMaterial = lens _ckprKeyMaterial (\ s a -> s{_ckprKeyMaterial = a});
