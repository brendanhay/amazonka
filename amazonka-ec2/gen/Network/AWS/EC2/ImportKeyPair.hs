{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.ImportKeyPair
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

-- | Imports the public key from an RSA key pair that you created with a
-- third-party tool. Compare this with CreateKeyPair, in which AWS creates
-- the key pair and gives the keys to you (AWS keeps a copy of the public
-- key). With ImportKeyPair, you create the key pair and give AWS just the
-- public key. The private key is never transferred between you and AWS.
--
-- For more information about key pairs, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Key Pairs>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ImportKeyPair.html>
module Network.AWS.EC2.ImportKeyPair
    (
    -- * Request
      ImportKeyPair
    -- ** Request constructor
    , importKeyPair
    -- ** Request lenses
    , ikpDryRun
    , ikpKeyName
    , ikpPublicKeyMaterial

    -- * Response
    , ImportKeyPairResponse
    -- ** Response constructor
    , importKeyPairResponse
    -- ** Response lenses
    , ikprKeyFingerprint
    , ikprKeyName
    ) where

import Network.AWS.EC2.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'importKeyPair' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ikpDryRun'
--
-- * 'ikpKeyName'
--
-- * 'ikpPublicKeyMaterial'
data ImportKeyPair = ImportKeyPair'{_ikpDryRun :: Maybe Bool, _ikpKeyName :: Text, _ikpPublicKeyMaterial :: Base64} deriving (Eq, Read, Show)

-- | 'ImportKeyPair' smart constructor.
importKeyPair :: Text -> Base64 -> ImportKeyPair
importKeyPair pKeyName pPublicKeyMaterial = ImportKeyPair'{_ikpDryRun = Nothing, _ikpKeyName = pKeyName, _ikpPublicKeyMaterial = pPublicKeyMaterial};

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
ikpDryRun :: Lens' ImportKeyPair (Maybe Bool)
ikpDryRun = lens _ikpDryRun (\ s a -> s{_ikpDryRun = a});

-- | A unique name for the key pair.
ikpKeyName :: Lens' ImportKeyPair Text
ikpKeyName = lens _ikpKeyName (\ s a -> s{_ikpKeyName = a});

-- | The public key. You must base64 encode the public key material before
-- sending it to AWS.
ikpPublicKeyMaterial :: Lens' ImportKeyPair Base64
ikpPublicKeyMaterial = lens _ikpPublicKeyMaterial (\ s a -> s{_ikpPublicKeyMaterial = a});

instance AWSRequest ImportKeyPair where
        type Sv ImportKeyPair = EC2
        type Rs ImportKeyPair = ImportKeyPairResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 ImportKeyPairResponse' <$>
                   (x .@? "keyFingerprint") <*> (x .@? "keyName"))

instance ToHeaders ImportKeyPair where
        toHeaders = const mempty

instance ToPath ImportKeyPair where
        toPath = const "/"

instance ToQuery ImportKeyPair where
        toQuery ImportKeyPair'{..}
          = mconcat
              ["Action" =: ("ImportKeyPair" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _ikpDryRun, "KeyName" =: _ikpKeyName,
               "PublicKeyMaterial" =: _ikpPublicKeyMaterial]

-- | /See:/ 'importKeyPairResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ikprKeyFingerprint'
--
-- * 'ikprKeyName'
data ImportKeyPairResponse = ImportKeyPairResponse'{_ikprKeyFingerprint :: Maybe Text, _ikprKeyName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ImportKeyPairResponse' smart constructor.
importKeyPairResponse :: ImportKeyPairResponse
importKeyPairResponse = ImportKeyPairResponse'{_ikprKeyFingerprint = Nothing, _ikprKeyName = Nothing};

-- | The MD5 public key fingerprint as specified in section 4 of RFC 4716.
ikprKeyFingerprint :: Lens' ImportKeyPairResponse (Maybe Text)
ikprKeyFingerprint = lens _ikprKeyFingerprint (\ s a -> s{_ikprKeyFingerprint = a});

-- | The key pair name you provided.
ikprKeyName :: Lens' ImportKeyPairResponse (Maybe Text)
ikprKeyName = lens _ikprKeyName (\ s a -> s{_ikprKeyName = a});
