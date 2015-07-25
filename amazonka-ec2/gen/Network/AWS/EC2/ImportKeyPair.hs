{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ImportKeyPair
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Imports the public key from an RSA key pair that you created with a
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
    , ikprsKeyFingerprint
    , ikprsKeyName
    , ikprsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'importKeyPair' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ikpDryRun'
--
-- * 'ikpKeyName'
--
-- * 'ikpPublicKeyMaterial'
data ImportKeyPair = ImportKeyPair'
    { _ikpDryRun            :: !(Maybe Bool)
    , _ikpKeyName           :: !Text
    , _ikpPublicKeyMaterial :: !Base64
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ImportKeyPair' smart constructor.
importKeyPair :: Text -> ByteString -> ImportKeyPair
importKeyPair pKeyName_ pPublicKeyMaterial_ =
    ImportKeyPair'
    { _ikpDryRun = Nothing
    , _ikpKeyName = pKeyName_
    , _ikpPublicKeyMaterial = _Base64 # pPublicKeyMaterial_
    }

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
ikpPublicKeyMaterial :: Lens' ImportKeyPair ByteString
ikpPublicKeyMaterial = lens _ikpPublicKeyMaterial (\ s a -> s{_ikpPublicKeyMaterial = a}) . _Base64;

instance AWSRequest ImportKeyPair where
        type Sv ImportKeyPair = EC2
        type Rs ImportKeyPair = ImportKeyPairResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 ImportKeyPairResponse' <$>
                   (x .@? "keyFingerprint") <*> (x .@? "keyName") <*>
                     (pure (fromEnum s)))

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
-- * 'ikprsKeyFingerprint'
--
-- * 'ikprsKeyName'
--
-- * 'ikprsStatus'
data ImportKeyPairResponse = ImportKeyPairResponse'
    { _ikprsKeyFingerprint :: !(Maybe Text)
    , _ikprsKeyName        :: !(Maybe Text)
    , _ikprsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ImportKeyPairResponse' smart constructor.
importKeyPairResponse :: Int -> ImportKeyPairResponse
importKeyPairResponse pStatus_ =
    ImportKeyPairResponse'
    { _ikprsKeyFingerprint = Nothing
    , _ikprsKeyName = Nothing
    , _ikprsStatus = pStatus_
    }

-- | The MD5 public key fingerprint as specified in section 4 of RFC 4716.
ikprsKeyFingerprint :: Lens' ImportKeyPairResponse (Maybe Text)
ikprsKeyFingerprint = lens _ikprsKeyFingerprint (\ s a -> s{_ikprsKeyFingerprint = a});

-- | The key pair name you provided.
ikprsKeyName :: Lens' ImportKeyPairResponse (Maybe Text)
ikprsKeyName = lens _ikprsKeyName (\ s a -> s{_ikprsKeyName = a});

-- | FIXME: Undocumented member.
ikprsStatus :: Lens' ImportKeyPairResponse Int
ikprsStatus = lens _ikprsStatus (\ s a -> s{_ikprsStatus = a});
