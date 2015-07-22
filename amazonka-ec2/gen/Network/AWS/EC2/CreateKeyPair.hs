{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateKeyPair
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a 2048-bit RSA key pair with the specified name. Amazon EC2
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
    , ckprqDryRun
    , ckprqKeyName

    -- * Response
    , CreateKeyPairResponse
    -- ** Response constructor
    , createKeyPairResponse
    -- ** Response lenses
    , ckprsStatus
    , ckprsKeyName
    , ckprsKeyFingerprint
    , ckprsKeyMaterial
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createKeyPair' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ckprqDryRun'
--
-- * 'ckprqKeyName'
data CreateKeyPair = CreateKeyPair'
    { _ckprqDryRun  :: !(Maybe Bool)
    , _ckprqKeyName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateKeyPair' smart constructor.
createKeyPair :: Text -> CreateKeyPair
createKeyPair pKeyName =
    CreateKeyPair'
    { _ckprqDryRun = Nothing
    , _ckprqKeyName = pKeyName
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
ckprqDryRun :: Lens' CreateKeyPair (Maybe Bool)
ckprqDryRun = lens _ckprqDryRun (\ s a -> s{_ckprqDryRun = a});

-- | A unique name for the key pair.
--
-- Constraints: Up to 255 ASCII characters
ckprqKeyName :: Lens' CreateKeyPair Text
ckprqKeyName = lens _ckprqKeyName (\ s a -> s{_ckprqKeyName = a});

instance AWSRequest CreateKeyPair where
        type Sv CreateKeyPair = EC2
        type Rs CreateKeyPair = CreateKeyPairResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 CreateKeyPairResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "keyName") <*>
                     (x .@ "keyFingerprint")
                     <*> (x .@ "keyMaterial"))

instance ToHeaders CreateKeyPair where
        toHeaders = const mempty

instance ToPath CreateKeyPair where
        toPath = const "/"

instance ToQuery CreateKeyPair where
        toQuery CreateKeyPair'{..}
          = mconcat
              ["Action" =: ("CreateKeyPair" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _ckprqDryRun, "KeyName" =: _ckprqKeyName]

-- | Describes a key pair.
--
-- /See:/ 'createKeyPairResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ckprsStatus'
--
-- * 'ckprsKeyName'
--
-- * 'ckprsKeyFingerprint'
--
-- * 'ckprsKeyMaterial'
data CreateKeyPairResponse = CreateKeyPairResponse'
    { _ckprsStatus         :: !Int
    , _ckprsKeyName        :: !Text
    , _ckprsKeyFingerprint :: !Text
    , _ckprsKeyMaterial    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateKeyPairResponse' smart constructor.
createKeyPairResponse :: Int -> Text -> Text -> Text -> CreateKeyPairResponse
createKeyPairResponse pStatus pKeyName pKeyFingerprint pKeyMaterial =
    CreateKeyPairResponse'
    { _ckprsStatus = pStatus
    , _ckprsKeyName = pKeyName
    , _ckprsKeyFingerprint = pKeyFingerprint
    , _ckprsKeyMaterial = pKeyMaterial
    }

-- | FIXME: Undocumented member.
ckprsStatus :: Lens' CreateKeyPairResponse Int
ckprsStatus = lens _ckprsStatus (\ s a -> s{_ckprsStatus = a});

-- | The name of the key pair.
ckprsKeyName :: Lens' CreateKeyPairResponse Text
ckprsKeyName = lens _ckprsKeyName (\ s a -> s{_ckprsKeyName = a});

-- | The SHA-1 digest of the DER encoded private key.
ckprsKeyFingerprint :: Lens' CreateKeyPairResponse Text
ckprsKeyFingerprint = lens _ckprsKeyFingerprint (\ s a -> s{_ckprsKeyFingerprint = a});

-- | An unencrypted PEM encoded RSA private key.
ckprsKeyMaterial :: Lens' CreateKeyPairResponse Text
ckprsKeyMaterial = lens _ckprsKeyMaterial (\ s a -> s{_ckprsKeyMaterial = a});
