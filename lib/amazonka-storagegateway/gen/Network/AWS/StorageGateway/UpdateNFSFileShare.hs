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
-- Module      : Network.AWS.StorageGateway.UpdateNFSFileShare
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a file share. This operation is only supported in the file gateway type.
--
--
-- Updates the following file share setting:
--
--     * Default storage class for your S3 bucket
--
--     * Metadata defaults for your S3 bucket
--
--     * Allowed NFS clients for your file share
--
--     * Squash settings
--
--     * Write status of your file share
--
--
--
module Network.AWS.StorageGateway.UpdateNFSFileShare
    (
    -- * Creating a Request
      updateNFSFileShare
    , UpdateNFSFileShare
    -- * Request Lenses
    , unfsfsKMSKey
    , unfsfsObjectACL
    , unfsfsKMSEncrypted
    , unfsfsDefaultStorageClass
    , unfsfsSquash
    , unfsfsRequesterPays
    , unfsfsNFSFileShareDefaults
    , unfsfsClientList
    , unfsfsGuessMIMETypeEnabled
    , unfsfsReadOnly
    , unfsfsFileShareARN

    -- * Destructuring the Response
    , updateNFSFileShareResponse
    , UpdateNFSFileShareResponse
    -- * Response Lenses
    , unfsfsrsFileShareARN
    , unfsfsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | UpdateNFSFileShareInput
--
--
--
-- /See:/ 'updateNFSFileShare' smart constructor.
data UpdateNFSFileShare = UpdateNFSFileShare'
  { _unfsfsKMSKey               :: !(Maybe Text)
  , _unfsfsObjectACL            :: !(Maybe ObjectACL)
  , _unfsfsKMSEncrypted         :: !(Maybe Bool)
  , _unfsfsDefaultStorageClass  :: !(Maybe Text)
  , _unfsfsSquash               :: !(Maybe Text)
  , _unfsfsRequesterPays        :: !(Maybe Bool)
  , _unfsfsNFSFileShareDefaults :: !(Maybe NFSFileShareDefaults)
  , _unfsfsClientList           :: !(Maybe (List1 Text))
  , _unfsfsGuessMIMETypeEnabled :: !(Maybe Bool)
  , _unfsfsReadOnly             :: !(Maybe Bool)
  , _unfsfsFileShareARN         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateNFSFileShare' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'unfsfsKMSKey' - The KMS key used for Amazon S3 server side encryption. This value can only be set when KmsEncrypted is true. Optional.
--
-- * 'unfsfsObjectACL' - Sets the access control list permission for objects in the S3 bucket that a file gateway puts objects into. The default value is "private".
--
-- * 'unfsfsKMSEncrypted' - True to use Amazon S3 server side encryption with your own AWS KMS key, or false to use a key managed by Amazon S3. Optional.
--
-- * 'unfsfsDefaultStorageClass' - The default storage class for objects put into an Amazon S3 bucket by a file gateway. Possible values are S3_STANDARD or S3_STANDARD_IA. If this field is not populated, the default value S3_STANDARD is used. Optional.
--
-- * 'unfsfsSquash' - The user mapped to anonymous user. Valid options are the following:     * "RootSquash" - Only root is mapped to anonymous user.     * "NoSquash" - No one is mapped to anonymous user     * "AllSquash" - Everyone is mapped to anonymous user.
--
-- * 'unfsfsRequesterPays' - Sets who pays the cost of the request and the data download from the Amazon S3 bucket. Set this value to true if you want the requester to pay instead of the bucket owner, and otherwise to false.
--
-- * 'unfsfsNFSFileShareDefaults' - The default values for the file share. Optional.
--
-- * 'unfsfsClientList' - The list of clients that are allowed to access the file gateway. The list must contain either valid IP addresses or valid CIDR blocks.
--
-- * 'unfsfsGuessMIMETypeEnabled' - Enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to true to enable MIME type guessing, and otherwise to false. The default value is true.
--
-- * 'unfsfsReadOnly' - Sets the write status of a file share. This value is true if the write status is read-only, and otherwise false.
--
-- * 'unfsfsFileShareARN' - The Amazon Resource Name (ARN) of the file share to be updated.
updateNFSFileShare
    :: Text -- ^ 'unfsfsFileShareARN'
    -> UpdateNFSFileShare
updateNFSFileShare pFileShareARN_ =
  UpdateNFSFileShare'
    { _unfsfsKMSKey = Nothing
    , _unfsfsObjectACL = Nothing
    , _unfsfsKMSEncrypted = Nothing
    , _unfsfsDefaultStorageClass = Nothing
    , _unfsfsSquash = Nothing
    , _unfsfsRequesterPays = Nothing
    , _unfsfsNFSFileShareDefaults = Nothing
    , _unfsfsClientList = Nothing
    , _unfsfsGuessMIMETypeEnabled = Nothing
    , _unfsfsReadOnly = Nothing
    , _unfsfsFileShareARN = pFileShareARN_
    }


-- | The KMS key used for Amazon S3 server side encryption. This value can only be set when KmsEncrypted is true. Optional.
unfsfsKMSKey :: Lens' UpdateNFSFileShare (Maybe Text)
unfsfsKMSKey = lens _unfsfsKMSKey (\ s a -> s{_unfsfsKMSKey = a})

-- | Sets the access control list permission for objects in the S3 bucket that a file gateway puts objects into. The default value is "private".
unfsfsObjectACL :: Lens' UpdateNFSFileShare (Maybe ObjectACL)
unfsfsObjectACL = lens _unfsfsObjectACL (\ s a -> s{_unfsfsObjectACL = a})

-- | True to use Amazon S3 server side encryption with your own AWS KMS key, or false to use a key managed by Amazon S3. Optional.
unfsfsKMSEncrypted :: Lens' UpdateNFSFileShare (Maybe Bool)
unfsfsKMSEncrypted = lens _unfsfsKMSEncrypted (\ s a -> s{_unfsfsKMSEncrypted = a})

-- | The default storage class for objects put into an Amazon S3 bucket by a file gateway. Possible values are S3_STANDARD or S3_STANDARD_IA. If this field is not populated, the default value S3_STANDARD is used. Optional.
unfsfsDefaultStorageClass :: Lens' UpdateNFSFileShare (Maybe Text)
unfsfsDefaultStorageClass = lens _unfsfsDefaultStorageClass (\ s a -> s{_unfsfsDefaultStorageClass = a})

-- | The user mapped to anonymous user. Valid options are the following:     * "RootSquash" - Only root is mapped to anonymous user.     * "NoSquash" - No one is mapped to anonymous user     * "AllSquash" - Everyone is mapped to anonymous user.
unfsfsSquash :: Lens' UpdateNFSFileShare (Maybe Text)
unfsfsSquash = lens _unfsfsSquash (\ s a -> s{_unfsfsSquash = a})

-- | Sets who pays the cost of the request and the data download from the Amazon S3 bucket. Set this value to true if you want the requester to pay instead of the bucket owner, and otherwise to false.
unfsfsRequesterPays :: Lens' UpdateNFSFileShare (Maybe Bool)
unfsfsRequesterPays = lens _unfsfsRequesterPays (\ s a -> s{_unfsfsRequesterPays = a})

-- | The default values for the file share. Optional.
unfsfsNFSFileShareDefaults :: Lens' UpdateNFSFileShare (Maybe NFSFileShareDefaults)
unfsfsNFSFileShareDefaults = lens _unfsfsNFSFileShareDefaults (\ s a -> s{_unfsfsNFSFileShareDefaults = a})

-- | The list of clients that are allowed to access the file gateway. The list must contain either valid IP addresses or valid CIDR blocks.
unfsfsClientList :: Lens' UpdateNFSFileShare (Maybe (NonEmpty Text))
unfsfsClientList = lens _unfsfsClientList (\ s a -> s{_unfsfsClientList = a}) . mapping _List1

-- | Enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to true to enable MIME type guessing, and otherwise to false. The default value is true.
unfsfsGuessMIMETypeEnabled :: Lens' UpdateNFSFileShare (Maybe Bool)
unfsfsGuessMIMETypeEnabled = lens _unfsfsGuessMIMETypeEnabled (\ s a -> s{_unfsfsGuessMIMETypeEnabled = a})

-- | Sets the write status of a file share. This value is true if the write status is read-only, and otherwise false.
unfsfsReadOnly :: Lens' UpdateNFSFileShare (Maybe Bool)
unfsfsReadOnly = lens _unfsfsReadOnly (\ s a -> s{_unfsfsReadOnly = a})

-- | The Amazon Resource Name (ARN) of the file share to be updated.
unfsfsFileShareARN :: Lens' UpdateNFSFileShare Text
unfsfsFileShareARN = lens _unfsfsFileShareARN (\ s a -> s{_unfsfsFileShareARN = a})

instance AWSRequest UpdateNFSFileShare where
        type Rs UpdateNFSFileShare =
             UpdateNFSFileShareResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 UpdateNFSFileShareResponse' <$>
                   (x .?> "FileShareARN") <*> (pure (fromEnum s)))

instance Hashable UpdateNFSFileShare where

instance NFData UpdateNFSFileShare where

instance ToHeaders UpdateNFSFileShare where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.UpdateNFSFileShare" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateNFSFileShare where
        toJSON UpdateNFSFileShare'{..}
          = object
              (catMaybes
                 [("KMSKey" .=) <$> _unfsfsKMSKey,
                  ("ObjectACL" .=) <$> _unfsfsObjectACL,
                  ("KMSEncrypted" .=) <$> _unfsfsKMSEncrypted,
                  ("DefaultStorageClass" .=) <$>
                    _unfsfsDefaultStorageClass,
                  ("Squash" .=) <$> _unfsfsSquash,
                  ("RequesterPays" .=) <$> _unfsfsRequesterPays,
                  ("NFSFileShareDefaults" .=) <$>
                    _unfsfsNFSFileShareDefaults,
                  ("ClientList" .=) <$> _unfsfsClientList,
                  ("GuessMIMETypeEnabled" .=) <$>
                    _unfsfsGuessMIMETypeEnabled,
                  ("ReadOnly" .=) <$> _unfsfsReadOnly,
                  Just ("FileShareARN" .= _unfsfsFileShareARN)])

instance ToPath UpdateNFSFileShare where
        toPath = const "/"

instance ToQuery UpdateNFSFileShare where
        toQuery = const mempty

-- | UpdateNFSFileShareOutput
--
--
--
-- /See:/ 'updateNFSFileShareResponse' smart constructor.
data UpdateNFSFileShareResponse = UpdateNFSFileShareResponse'
  { _unfsfsrsFileShareARN   :: !(Maybe Text)
  , _unfsfsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateNFSFileShareResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'unfsfsrsFileShareARN' - The Amazon Resource Name (ARN) of the updated file share.
--
-- * 'unfsfsrsResponseStatus' - -- | The response status code.
updateNFSFileShareResponse
    :: Int -- ^ 'unfsfsrsResponseStatus'
    -> UpdateNFSFileShareResponse
updateNFSFileShareResponse pResponseStatus_ =
  UpdateNFSFileShareResponse'
    { _unfsfsrsFileShareARN = Nothing
    , _unfsfsrsResponseStatus = pResponseStatus_
    }


-- | The Amazon Resource Name (ARN) of the updated file share.
unfsfsrsFileShareARN :: Lens' UpdateNFSFileShareResponse (Maybe Text)
unfsfsrsFileShareARN = lens _unfsfsrsFileShareARN (\ s a -> s{_unfsfsrsFileShareARN = a})

-- | -- | The response status code.
unfsfsrsResponseStatus :: Lens' UpdateNFSFileShareResponse Int
unfsfsrsResponseStatus = lens _unfsfsrsResponseStatus (\ s a -> s{_unfsfsrsResponseStatus = a})

instance NFData UpdateNFSFileShareResponse where
