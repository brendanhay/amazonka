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
-- Module      : Network.AWS.StorageGateway.CreateNFSFileShare
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a file share on an existing file gateway. In Storage Gateway, a file share is a file system mount point backed by Amazon S3 cloud storage. Storage Gateway exposes file shares using a Network File System (NFS) interface.
--
--
module Network.AWS.StorageGateway.CreateNFSFileShare
    (
    -- * Creating a Request
      createNFSFileShare
    , CreateNFSFileShare
    -- * Request Lenses
    , cnfsfsKMSKey
    , cnfsfsKMSEncrypted
    , cnfsfsDefaultStorageClass
    , cnfsfsNFSFileShareDefaults
    , cnfsfsClientList
    , cnfsfsClientToken
    , cnfsfsGatewayARN
    , cnfsfsRole
    , cnfsfsLocationARN

    -- * Destructuring the Response
    , createNFSFileShareResponse
    , CreateNFSFileShareResponse
    -- * Response Lenses
    , cnfsfsrsFileShareARN
    , cnfsfsrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types
import           Network.AWS.StorageGateway.Types.Product

-- | CreateNFSFileShareInput
--
--
--
-- /See:/ 'createNFSFileShare' smart constructor.
data CreateNFSFileShare = CreateNFSFileShare'
    { _cnfsfsKMSKey               :: !(Maybe Text)
    , _cnfsfsKMSEncrypted         :: !(Maybe Bool)
    , _cnfsfsDefaultStorageClass  :: !(Maybe Text)
    , _cnfsfsNFSFileShareDefaults :: !(Maybe NFSFileShareDefaults)
    , _cnfsfsClientList           :: !(Maybe (List1 Text))
    , _cnfsfsClientToken          :: !Text
    , _cnfsfsGatewayARN           :: !Text
    , _cnfsfsRole                 :: !Text
    , _cnfsfsLocationARN          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateNFSFileShare' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cnfsfsKMSKey' - The KMS key used for Amazon S3 server side encryption. This value can only be set when KmsEncrypted is true. Optional.
--
-- * 'cnfsfsKMSEncrypted' - True to use Amazon S3 server side encryption with your own AWS KMS key, or false to use a key managed by Amazon S3. Optional.
--
-- * 'cnfsfsDefaultStorageClass' - The default storage class for objects put into an Amazon S3 bucket by file gateway. Possible values are S3_STANDARD or S3_STANDARD_IA. If this field is not populated, the default value S3_STANDARD is used. Optional.
--
-- * 'cnfsfsNFSFileShareDefaults' - File share default values. Optional.
--
-- * 'cnfsfsClientList' - The list of clients that are allowed to access the file gateway. The list must contain either valid IP addresses or valid CIDR blocks.
--
-- * 'cnfsfsClientToken' - A unique string value that you supply that is used by file gateway to ensure idempotent file share creation.
--
-- * 'cnfsfsGatewayARN' - The Amazon Resource Name (ARN) of the file gateway on which you want to create a file share.
--
-- * 'cnfsfsRole' - The ARN of the AWS Identity and Access Management (IAM) role that a file gateway assumes when it accesses the underlying storage.
--
-- * 'cnfsfsLocationARN' - The ARN of the backend storage used for storing file data.
createNFSFileShare
    :: Text -- ^ 'cnfsfsClientToken'
    -> Text -- ^ 'cnfsfsGatewayARN'
    -> Text -- ^ 'cnfsfsRole'
    -> Text -- ^ 'cnfsfsLocationARN'
    -> CreateNFSFileShare
createNFSFileShare pClientToken_ pGatewayARN_ pRole_ pLocationARN_ =
    CreateNFSFileShare'
    { _cnfsfsKMSKey = Nothing
    , _cnfsfsKMSEncrypted = Nothing
    , _cnfsfsDefaultStorageClass = Nothing
    , _cnfsfsNFSFileShareDefaults = Nothing
    , _cnfsfsClientList = Nothing
    , _cnfsfsClientToken = pClientToken_
    , _cnfsfsGatewayARN = pGatewayARN_
    , _cnfsfsRole = pRole_
    , _cnfsfsLocationARN = pLocationARN_
    }

-- | The KMS key used for Amazon S3 server side encryption. This value can only be set when KmsEncrypted is true. Optional.
cnfsfsKMSKey :: Lens' CreateNFSFileShare (Maybe Text)
cnfsfsKMSKey = lens _cnfsfsKMSKey (\ s a -> s{_cnfsfsKMSKey = a});

-- | True to use Amazon S3 server side encryption with your own AWS KMS key, or false to use a key managed by Amazon S3. Optional.
cnfsfsKMSEncrypted :: Lens' CreateNFSFileShare (Maybe Bool)
cnfsfsKMSEncrypted = lens _cnfsfsKMSEncrypted (\ s a -> s{_cnfsfsKMSEncrypted = a});

-- | The default storage class for objects put into an Amazon S3 bucket by file gateway. Possible values are S3_STANDARD or S3_STANDARD_IA. If this field is not populated, the default value S3_STANDARD is used. Optional.
cnfsfsDefaultStorageClass :: Lens' CreateNFSFileShare (Maybe Text)
cnfsfsDefaultStorageClass = lens _cnfsfsDefaultStorageClass (\ s a -> s{_cnfsfsDefaultStorageClass = a});

-- | File share default values. Optional.
cnfsfsNFSFileShareDefaults :: Lens' CreateNFSFileShare (Maybe NFSFileShareDefaults)
cnfsfsNFSFileShareDefaults = lens _cnfsfsNFSFileShareDefaults (\ s a -> s{_cnfsfsNFSFileShareDefaults = a});

-- | The list of clients that are allowed to access the file gateway. The list must contain either valid IP addresses or valid CIDR blocks.
cnfsfsClientList :: Lens' CreateNFSFileShare (Maybe (NonEmpty Text))
cnfsfsClientList = lens _cnfsfsClientList (\ s a -> s{_cnfsfsClientList = a}) . mapping _List1;

-- | A unique string value that you supply that is used by file gateway to ensure idempotent file share creation.
cnfsfsClientToken :: Lens' CreateNFSFileShare Text
cnfsfsClientToken = lens _cnfsfsClientToken (\ s a -> s{_cnfsfsClientToken = a});

-- | The Amazon Resource Name (ARN) of the file gateway on which you want to create a file share.
cnfsfsGatewayARN :: Lens' CreateNFSFileShare Text
cnfsfsGatewayARN = lens _cnfsfsGatewayARN (\ s a -> s{_cnfsfsGatewayARN = a});

-- | The ARN of the AWS Identity and Access Management (IAM) role that a file gateway assumes when it accesses the underlying storage.
cnfsfsRole :: Lens' CreateNFSFileShare Text
cnfsfsRole = lens _cnfsfsRole (\ s a -> s{_cnfsfsRole = a});

-- | The ARN of the backend storage used for storing file data.
cnfsfsLocationARN :: Lens' CreateNFSFileShare Text
cnfsfsLocationARN = lens _cnfsfsLocationARN (\ s a -> s{_cnfsfsLocationARN = a});

instance AWSRequest CreateNFSFileShare where
        type Rs CreateNFSFileShare =
             CreateNFSFileShareResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 CreateNFSFileShareResponse' <$>
                   (x .?> "FileShareARN") <*> (pure (fromEnum s)))

instance Hashable CreateNFSFileShare

instance NFData CreateNFSFileShare

instance ToHeaders CreateNFSFileShare where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.CreateNFSFileShare" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateNFSFileShare where
        toJSON CreateNFSFileShare'{..}
          = object
              (catMaybes
                 [("KMSKey" .=) <$> _cnfsfsKMSKey,
                  ("KMSEncrypted" .=) <$> _cnfsfsKMSEncrypted,
                  ("DefaultStorageClass" .=) <$>
                    _cnfsfsDefaultStorageClass,
                  ("NFSFileShareDefaults" .=) <$>
                    _cnfsfsNFSFileShareDefaults,
                  ("ClientList" .=) <$> _cnfsfsClientList,
                  Just ("ClientToken" .= _cnfsfsClientToken),
                  Just ("GatewayARN" .= _cnfsfsGatewayARN),
                  Just ("Role" .= _cnfsfsRole),
                  Just ("LocationARN" .= _cnfsfsLocationARN)])

instance ToPath CreateNFSFileShare where
        toPath = const "/"

instance ToQuery CreateNFSFileShare where
        toQuery = const mempty

-- | CreateNFSFileShareOutput
--
--
--
-- /See:/ 'createNFSFileShareResponse' smart constructor.
data CreateNFSFileShareResponse = CreateNFSFileShareResponse'
    { _cnfsfsrsFileShareARN   :: !(Maybe Text)
    , _cnfsfsrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateNFSFileShareResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cnfsfsrsFileShareARN' - The Amazon Resource Name (ARN) of the newly created file share.
--
-- * 'cnfsfsrsResponseStatus' - -- | The response status code.
createNFSFileShareResponse
    :: Int -- ^ 'cnfsfsrsResponseStatus'
    -> CreateNFSFileShareResponse
createNFSFileShareResponse pResponseStatus_ =
    CreateNFSFileShareResponse'
    { _cnfsfsrsFileShareARN = Nothing
    , _cnfsfsrsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the newly created file share.
cnfsfsrsFileShareARN :: Lens' CreateNFSFileShareResponse (Maybe Text)
cnfsfsrsFileShareARN = lens _cnfsfsrsFileShareARN (\ s a -> s{_cnfsfsrsFileShareARN = a});

-- | -- | The response status code.
cnfsfsrsResponseStatus :: Lens' CreateNFSFileShareResponse Int
cnfsfsrsResponseStatus = lens _cnfsfsrsResponseStatus (\ s a -> s{_cnfsfsrsResponseStatus = a});

instance NFData CreateNFSFileShareResponse
