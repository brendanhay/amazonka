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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a file share on an existing file gateway. In Storage Gateway, a file share is a file system mount point backed by Amazon S3 cloud storage. Storage Gateway exposes file shares using a Network File System (NFS) interface. This operation is only supported in the file gateway type.
--
--
-- /Important:/ File gateway requires AWS Security Token Service (AWS STS) to be activated to enable you create a file share. Make sure AWS STS is activated in the region you are creating your file gateway in. If AWS STS is not activated in the region, activate it. For information about how to activate AWS STS, see Activating and Deactivating AWS STS in an AWS Region in the AWS Identity and Access Management User Guide.
--
-- File gateway does not support creating hard or symbolic links on a file share.
--
module Network.AWS.StorageGateway.CreateNFSFileShare
    (
    -- * Creating a Request
      createNFSFileShare
    , CreateNFSFileShare
    -- * Request Lenses
    , cnfsfsKMSKey
    , cnfsfsObjectACL
    , cnfsfsKMSEncrypted
    , cnfsfsDefaultStorageClass
    , cnfsfsSquash
    , cnfsfsRequesterPays
    , cnfsfsNFSFileShareDefaults
    , cnfsfsClientList
    , cnfsfsGuessMIMETypeEnabled
    , cnfsfsReadOnly
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

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | CreateNFSFileShareInput
--
--
--
-- /See:/ 'createNFSFileShare' smart constructor.
data CreateNFSFileShare = CreateNFSFileShare'
  { _cnfsfsKMSKey               :: !(Maybe Text)
  , _cnfsfsObjectACL            :: !(Maybe ObjectACL)
  , _cnfsfsKMSEncrypted         :: !(Maybe Bool)
  , _cnfsfsDefaultStorageClass  :: !(Maybe Text)
  , _cnfsfsSquash               :: !(Maybe Text)
  , _cnfsfsRequesterPays        :: !(Maybe Bool)
  , _cnfsfsNFSFileShareDefaults :: !(Maybe NFSFileShareDefaults)
  , _cnfsfsClientList           :: !(Maybe (List1 Text))
  , _cnfsfsGuessMIMETypeEnabled :: !(Maybe Bool)
  , _cnfsfsReadOnly             :: !(Maybe Bool)
  , _cnfsfsClientToken          :: !Text
  , _cnfsfsGatewayARN           :: !Text
  , _cnfsfsRole                 :: !Text
  , _cnfsfsLocationARN          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateNFSFileShare' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cnfsfsKMSKey' - The KMS key used for Amazon S3 server side encryption. This value can only be set when KmsEncrypted is true. Optional.
--
-- * 'cnfsfsObjectACL' - Sets the access control list permission for objects in the Amazon S3 bucket that a file gateway puts objects into. The default value is "private".
--
-- * 'cnfsfsKMSEncrypted' - True to use Amazon S3 server side encryption with your own AWS KMS key, or false to use a key managed by Amazon S3. Optional.
--
-- * 'cnfsfsDefaultStorageClass' - The default storage class for objects put into an Amazon S3 bucket by file gateway. Possible values are S3_STANDARD or S3_STANDARD_IA. If this field is not populated, the default value S3_STANDARD is used. Optional.
--
-- * 'cnfsfsSquash' - Maps a user to anonymous user. Valid options are the following:      * "RootSquash" - Only root is mapped to anonymous user.     * "NoSquash" - No one is mapped to anonymous user.     * "AllSquash" - Everyone is mapped to anonymous user.
--
-- * 'cnfsfsRequesterPays' - Sets who pays the cost of the request and the data download from the Amazon S3 bucket. Set this value to true if you want the requester to pay instead of the bucket owner, and otherwise to false.
--
-- * 'cnfsfsNFSFileShareDefaults' - File share default values. Optional.
--
-- * 'cnfsfsClientList' - The list of clients that are allowed to access the file gateway. The list must contain either valid IP addresses or valid CIDR blocks.
--
-- * 'cnfsfsGuessMIMETypeEnabled' - Enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to true to enable MIME type guessing, and otherwise to false. The default value is true.
--
-- * 'cnfsfsReadOnly' - Sets the write status of a file share. This value is true if the write status is read-only, and otherwise false.
--
-- * 'cnfsfsClientToken' - A unique string value that you supply that is used by file gateway to ensure idempotent file share creation.
--
-- * 'cnfsfsGatewayARN' - The Amazon Resource Name (ARN) of the file gateway on which you want to create a file share.
--
-- * 'cnfsfsRole' - The ARN of the AWS Identity and Access Management (IAM) role that a file gateway assumes when it accesses the underlying storage.
--
-- * 'cnfsfsLocationARN' - The ARN of the backed storage used for storing file data.
createNFSFileShare
    :: Text -- ^ 'cnfsfsClientToken'
    -> Text -- ^ 'cnfsfsGatewayARN'
    -> Text -- ^ 'cnfsfsRole'
    -> Text -- ^ 'cnfsfsLocationARN'
    -> CreateNFSFileShare
createNFSFileShare pClientToken_ pGatewayARN_ pRole_ pLocationARN_ =
  CreateNFSFileShare'
    { _cnfsfsKMSKey = Nothing
    , _cnfsfsObjectACL = Nothing
    , _cnfsfsKMSEncrypted = Nothing
    , _cnfsfsDefaultStorageClass = Nothing
    , _cnfsfsSquash = Nothing
    , _cnfsfsRequesterPays = Nothing
    , _cnfsfsNFSFileShareDefaults = Nothing
    , _cnfsfsClientList = Nothing
    , _cnfsfsGuessMIMETypeEnabled = Nothing
    , _cnfsfsReadOnly = Nothing
    , _cnfsfsClientToken = pClientToken_
    , _cnfsfsGatewayARN = pGatewayARN_
    , _cnfsfsRole = pRole_
    , _cnfsfsLocationARN = pLocationARN_
    }


-- | The KMS key used for Amazon S3 server side encryption. This value can only be set when KmsEncrypted is true. Optional.
cnfsfsKMSKey :: Lens' CreateNFSFileShare (Maybe Text)
cnfsfsKMSKey = lens _cnfsfsKMSKey (\ s a -> s{_cnfsfsKMSKey = a})

-- | Sets the access control list permission for objects in the Amazon S3 bucket that a file gateway puts objects into. The default value is "private".
cnfsfsObjectACL :: Lens' CreateNFSFileShare (Maybe ObjectACL)
cnfsfsObjectACL = lens _cnfsfsObjectACL (\ s a -> s{_cnfsfsObjectACL = a})

-- | True to use Amazon S3 server side encryption with your own AWS KMS key, or false to use a key managed by Amazon S3. Optional.
cnfsfsKMSEncrypted :: Lens' CreateNFSFileShare (Maybe Bool)
cnfsfsKMSEncrypted = lens _cnfsfsKMSEncrypted (\ s a -> s{_cnfsfsKMSEncrypted = a})

-- | The default storage class for objects put into an Amazon S3 bucket by file gateway. Possible values are S3_STANDARD or S3_STANDARD_IA. If this field is not populated, the default value S3_STANDARD is used. Optional.
cnfsfsDefaultStorageClass :: Lens' CreateNFSFileShare (Maybe Text)
cnfsfsDefaultStorageClass = lens _cnfsfsDefaultStorageClass (\ s a -> s{_cnfsfsDefaultStorageClass = a})

-- | Maps a user to anonymous user. Valid options are the following:      * "RootSquash" - Only root is mapped to anonymous user.     * "NoSquash" - No one is mapped to anonymous user.     * "AllSquash" - Everyone is mapped to anonymous user.
cnfsfsSquash :: Lens' CreateNFSFileShare (Maybe Text)
cnfsfsSquash = lens _cnfsfsSquash (\ s a -> s{_cnfsfsSquash = a})

-- | Sets who pays the cost of the request and the data download from the Amazon S3 bucket. Set this value to true if you want the requester to pay instead of the bucket owner, and otherwise to false.
cnfsfsRequesterPays :: Lens' CreateNFSFileShare (Maybe Bool)
cnfsfsRequesterPays = lens _cnfsfsRequesterPays (\ s a -> s{_cnfsfsRequesterPays = a})

-- | File share default values. Optional.
cnfsfsNFSFileShareDefaults :: Lens' CreateNFSFileShare (Maybe NFSFileShareDefaults)
cnfsfsNFSFileShareDefaults = lens _cnfsfsNFSFileShareDefaults (\ s a -> s{_cnfsfsNFSFileShareDefaults = a})

-- | The list of clients that are allowed to access the file gateway. The list must contain either valid IP addresses or valid CIDR blocks.
cnfsfsClientList :: Lens' CreateNFSFileShare (Maybe (NonEmpty Text))
cnfsfsClientList = lens _cnfsfsClientList (\ s a -> s{_cnfsfsClientList = a}) . mapping _List1

-- | Enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to true to enable MIME type guessing, and otherwise to false. The default value is true.
cnfsfsGuessMIMETypeEnabled :: Lens' CreateNFSFileShare (Maybe Bool)
cnfsfsGuessMIMETypeEnabled = lens _cnfsfsGuessMIMETypeEnabled (\ s a -> s{_cnfsfsGuessMIMETypeEnabled = a})

-- | Sets the write status of a file share. This value is true if the write status is read-only, and otherwise false.
cnfsfsReadOnly :: Lens' CreateNFSFileShare (Maybe Bool)
cnfsfsReadOnly = lens _cnfsfsReadOnly (\ s a -> s{_cnfsfsReadOnly = a})

-- | A unique string value that you supply that is used by file gateway to ensure idempotent file share creation.
cnfsfsClientToken :: Lens' CreateNFSFileShare Text
cnfsfsClientToken = lens _cnfsfsClientToken (\ s a -> s{_cnfsfsClientToken = a})

-- | The Amazon Resource Name (ARN) of the file gateway on which you want to create a file share.
cnfsfsGatewayARN :: Lens' CreateNFSFileShare Text
cnfsfsGatewayARN = lens _cnfsfsGatewayARN (\ s a -> s{_cnfsfsGatewayARN = a})

-- | The ARN of the AWS Identity and Access Management (IAM) role that a file gateway assumes when it accesses the underlying storage.
cnfsfsRole :: Lens' CreateNFSFileShare Text
cnfsfsRole = lens _cnfsfsRole (\ s a -> s{_cnfsfsRole = a})

-- | The ARN of the backed storage used for storing file data.
cnfsfsLocationARN :: Lens' CreateNFSFileShare Text
cnfsfsLocationARN = lens _cnfsfsLocationARN (\ s a -> s{_cnfsfsLocationARN = a})

instance AWSRequest CreateNFSFileShare where
        type Rs CreateNFSFileShare =
             CreateNFSFileShareResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 CreateNFSFileShareResponse' <$>
                   (x .?> "FileShareARN") <*> (pure (fromEnum s)))

instance Hashable CreateNFSFileShare where

instance NFData CreateNFSFileShare where

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
                  ("ObjectACL" .=) <$> _cnfsfsObjectACL,
                  ("KMSEncrypted" .=) <$> _cnfsfsKMSEncrypted,
                  ("DefaultStorageClass" .=) <$>
                    _cnfsfsDefaultStorageClass,
                  ("Squash" .=) <$> _cnfsfsSquash,
                  ("RequesterPays" .=) <$> _cnfsfsRequesterPays,
                  ("NFSFileShareDefaults" .=) <$>
                    _cnfsfsNFSFileShareDefaults,
                  ("ClientList" .=) <$> _cnfsfsClientList,
                  ("GuessMIMETypeEnabled" .=) <$>
                    _cnfsfsGuessMIMETypeEnabled,
                  ("ReadOnly" .=) <$> _cnfsfsReadOnly,
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
cnfsfsrsFileShareARN = lens _cnfsfsrsFileShareARN (\ s a -> s{_cnfsfsrsFileShareARN = a})

-- | -- | The response status code.
cnfsfsrsResponseStatus :: Lens' CreateNFSFileShareResponse Int
cnfsfsrsResponseStatus = lens _cnfsfsrsResponseStatus (\ s a -> s{_cnfsfsrsResponseStatus = a})

instance NFData CreateNFSFileShareResponse where
