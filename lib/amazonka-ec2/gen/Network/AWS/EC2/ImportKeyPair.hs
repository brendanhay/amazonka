{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ImportKeyPair
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports the public key from an RSA key pair that you created with a third-party tool. Compare this with 'CreateKeyPair' , in which AWS creates the key pair and gives the keys to you (AWS keeps a copy of the public key). With ImportKeyPair, you create the key pair and give AWS just the public key. The private key is never transferred between you and AWS.
--
--
-- For more information about key pairs, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Key Pairs> in the /Amazon Elastic Compute Cloud User Guide/ .
--
module Network.AWS.EC2.ImportKeyPair
    (
    -- * Creating a Request
      importKeyPair
    , ImportKeyPair
    -- * Request Lenses
    , ikpDryRun
    , ikpKeyName
    , ikpPublicKeyMaterial

    -- * Destructuring the Response
    , importKeyPairResponse
    , ImportKeyPairResponse
    -- * Response Lenses
    , ikprsKeyFingerprint
    , ikprsKeyName
    , ikprsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for ImportKeyPair.
--
--
--
-- /See:/ 'importKeyPair' smart constructor.
data ImportKeyPair = ImportKeyPair'
  { _ikpDryRun            :: !(Maybe Bool)
  , _ikpKeyName           :: !Text
  , _ikpPublicKeyMaterial :: !Base64
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportKeyPair' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ikpDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'ikpKeyName' - A unique name for the key pair.
--
-- * 'ikpPublicKeyMaterial' - The public key. For API calls, the text must be base64-encoded. For command line tools, base64 encoding is performed for you.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
importKeyPair
    :: Text -- ^ 'ikpKeyName'
    -> ByteString -- ^ 'ikpPublicKeyMaterial'
    -> ImportKeyPair
importKeyPair pKeyName_ pPublicKeyMaterial_ =
  ImportKeyPair'
    { _ikpDryRun = Nothing
    , _ikpKeyName = pKeyName_
    , _ikpPublicKeyMaterial = _Base64 # pPublicKeyMaterial_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
ikpDryRun :: Lens' ImportKeyPair (Maybe Bool)
ikpDryRun = lens _ikpDryRun (\ s a -> s{_ikpDryRun = a})

-- | A unique name for the key pair.
ikpKeyName :: Lens' ImportKeyPair Text
ikpKeyName = lens _ikpKeyName (\ s a -> s{_ikpKeyName = a})

-- | The public key. For API calls, the text must be base64-encoded. For command line tools, base64 encoding is performed for you.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
ikpPublicKeyMaterial :: Lens' ImportKeyPair ByteString
ikpPublicKeyMaterial = lens _ikpPublicKeyMaterial (\ s a -> s{_ikpPublicKeyMaterial = a}) . _Base64

instance AWSRequest ImportKeyPair where
        type Rs ImportKeyPair = ImportKeyPairResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 ImportKeyPairResponse' <$>
                   (x .@? "keyFingerprint") <*> (x .@? "keyName") <*>
                     (pure (fromEnum s)))

instance Hashable ImportKeyPair where

instance NFData ImportKeyPair where

instance ToHeaders ImportKeyPair where
        toHeaders = const mempty

instance ToPath ImportKeyPair where
        toPath = const "/"

instance ToQuery ImportKeyPair where
        toQuery ImportKeyPair'{..}
          = mconcat
              ["Action" =: ("ImportKeyPair" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _ikpDryRun, "KeyName" =: _ikpKeyName,
               "PublicKeyMaterial" =: _ikpPublicKeyMaterial]

-- | Contains the output of ImportKeyPair.
--
--
--
-- /See:/ 'importKeyPairResponse' smart constructor.
data ImportKeyPairResponse = ImportKeyPairResponse'
  { _ikprsKeyFingerprint :: !(Maybe Text)
  , _ikprsKeyName        :: !(Maybe Text)
  , _ikprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportKeyPairResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ikprsKeyFingerprint' - The MD5 public key fingerprint as specified in section 4 of RFC 4716.
--
-- * 'ikprsKeyName' - The key pair name you provided.
--
-- * 'ikprsResponseStatus' - -- | The response status code.
importKeyPairResponse
    :: Int -- ^ 'ikprsResponseStatus'
    -> ImportKeyPairResponse
importKeyPairResponse pResponseStatus_ =
  ImportKeyPairResponse'
    { _ikprsKeyFingerprint = Nothing
    , _ikprsKeyName = Nothing
    , _ikprsResponseStatus = pResponseStatus_
    }


-- | The MD5 public key fingerprint as specified in section 4 of RFC 4716.
ikprsKeyFingerprint :: Lens' ImportKeyPairResponse (Maybe Text)
ikprsKeyFingerprint = lens _ikprsKeyFingerprint (\ s a -> s{_ikprsKeyFingerprint = a})

-- | The key pair name you provided.
ikprsKeyName :: Lens' ImportKeyPairResponse (Maybe Text)
ikprsKeyName = lens _ikprsKeyName (\ s a -> s{_ikprsKeyName = a})

-- | -- | The response status code.
ikprsResponseStatus :: Lens' ImportKeyPairResponse Int
ikprsResponseStatus = lens _ikprsResponseStatus (\ s a -> s{_ikprsResponseStatus = a})

instance NFData ImportKeyPairResponse where
