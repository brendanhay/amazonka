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
-- Module      : Network.AWS.StorageGateway.UpdateSMBFileShare
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Server Message Block (SMB) file share.
--
--
-- /Important:/ File gateways require AWS Security Token Service (AWS STS) to be activated to enable you to create a file share. Make sure that AWS STS is activated in the AWS Region you are creating your file gateway in. If AWS STS is not activated in this AWS Region, activate it. For information about how to activate AWS STS, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and Deactivating AWS STS in an AWS Region> in the /AWS Identity and Access Management User Guide./
--
-- File gateways don't support creating hard or symbolic links on a file share.
--
module Network.AWS.StorageGateway.UpdateSMBFileShare
    (
    -- * Creating a Request
      updateSMBFileShare
    , UpdateSMBFileShare
    -- * Request Lenses
    , usmbfsInvalidUserList
    , usmbfsKMSKey
    , usmbfsValidUserList
    , usmbfsObjectACL
    , usmbfsKMSEncrypted
    , usmbfsDefaultStorageClass
    , usmbfsRequesterPays
    , usmbfsGuessMIMETypeEnabled
    , usmbfsReadOnly
    , usmbfsFileShareARN

    -- * Destructuring the Response
    , updateSMBFileShareResponse
    , UpdateSMBFileShareResponse
    -- * Response Lenses
    , usmbfsrsFileShareARN
    , usmbfsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | UpdateSMBFileShareInput
--
--
--
-- /See:/ 'updateSMBFileShare' smart constructor.
data UpdateSMBFileShare = UpdateSMBFileShare'
  { _usmbfsInvalidUserList      :: !(Maybe [Text])
  , _usmbfsKMSKey               :: !(Maybe Text)
  , _usmbfsValidUserList        :: !(Maybe [Text])
  , _usmbfsObjectACL            :: !(Maybe ObjectACL)
  , _usmbfsKMSEncrypted         :: !(Maybe Bool)
  , _usmbfsDefaultStorageClass  :: !(Maybe Text)
  , _usmbfsRequesterPays        :: !(Maybe Bool)
  , _usmbfsGuessMIMETypeEnabled :: !(Maybe Bool)
  , _usmbfsReadOnly             :: !(Maybe Bool)
  , _usmbfsFileShareARN         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateSMBFileShare' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usmbfsInvalidUserList' - A list of users or groups in the Active Directory that are not allowed to access the file share. A group must be prefixed with the @ character. For example @@group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
--
-- * 'usmbfsKMSKey' - The Amazon Resource Name (ARN) of the AWS KMS key used for Amazon S3 server side encryption. This value can only be set when KMSEncrypted is true. Optional.
--
-- * 'usmbfsValidUserList' - A list of users or groups in the Active Directory that are allowed to access the file share. A group must be prefixed with the @ character. For example @@group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
--
-- * 'usmbfsObjectACL' - A value that sets the access control list permission for objects in the S3 bucket that a file gateway puts objects into. The default value is "private".
--
-- * 'usmbfsKMSEncrypted' - True to use Amazon S3 server side encryption with your own AWS KMS key, or false to use a key managed by Amazon S3. Optional.
--
-- * 'usmbfsDefaultStorageClass' - The default storage class for objects put into an Amazon S3 bucket by the file gateway. Possible values are @S3_STANDARD@ , @S3_STANDARD_IA@ , or @S3_ONEZONE_IA@ . If this field is not populated, the default value @S3_STANDARD@ is used. Optional.
--
-- * 'usmbfsRequesterPays' - A value that sets the access control list permission for objects in the Amazon S3 bucket that a file gateway puts objects into. The default value is @private@ .
--
-- * 'usmbfsGuessMIMETypeEnabled' - A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to true to enable MIME type guessing, and otherwise to false. The default value is true.
--
-- * 'usmbfsReadOnly' - A value that sets the write status of a file share. This value is true if the write status is read-only, and otherwise false.
--
-- * 'usmbfsFileShareARN' - The Amazon Resource Name (ARN) of the SMB file share that you want to update.
updateSMBFileShare
    :: Text -- ^ 'usmbfsFileShareARN'
    -> UpdateSMBFileShare
updateSMBFileShare pFileShareARN_ =
  UpdateSMBFileShare'
    { _usmbfsInvalidUserList = Nothing
    , _usmbfsKMSKey = Nothing
    , _usmbfsValidUserList = Nothing
    , _usmbfsObjectACL = Nothing
    , _usmbfsKMSEncrypted = Nothing
    , _usmbfsDefaultStorageClass = Nothing
    , _usmbfsRequesterPays = Nothing
    , _usmbfsGuessMIMETypeEnabled = Nothing
    , _usmbfsReadOnly = Nothing
    , _usmbfsFileShareARN = pFileShareARN_
    }


-- | A list of users or groups in the Active Directory that are not allowed to access the file share. A group must be prefixed with the @ character. For example @@group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
usmbfsInvalidUserList :: Lens' UpdateSMBFileShare [Text]
usmbfsInvalidUserList = lens _usmbfsInvalidUserList (\ s a -> s{_usmbfsInvalidUserList = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the AWS KMS key used for Amazon S3 server side encryption. This value can only be set when KMSEncrypted is true. Optional.
usmbfsKMSKey :: Lens' UpdateSMBFileShare (Maybe Text)
usmbfsKMSKey = lens _usmbfsKMSKey (\ s a -> s{_usmbfsKMSKey = a})

-- | A list of users or groups in the Active Directory that are allowed to access the file share. A group must be prefixed with the @ character. For example @@group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
usmbfsValidUserList :: Lens' UpdateSMBFileShare [Text]
usmbfsValidUserList = lens _usmbfsValidUserList (\ s a -> s{_usmbfsValidUserList = a}) . _Default . _Coerce

-- | A value that sets the access control list permission for objects in the S3 bucket that a file gateway puts objects into. The default value is "private".
usmbfsObjectACL :: Lens' UpdateSMBFileShare (Maybe ObjectACL)
usmbfsObjectACL = lens _usmbfsObjectACL (\ s a -> s{_usmbfsObjectACL = a})

-- | True to use Amazon S3 server side encryption with your own AWS KMS key, or false to use a key managed by Amazon S3. Optional.
usmbfsKMSEncrypted :: Lens' UpdateSMBFileShare (Maybe Bool)
usmbfsKMSEncrypted = lens _usmbfsKMSEncrypted (\ s a -> s{_usmbfsKMSEncrypted = a})

-- | The default storage class for objects put into an Amazon S3 bucket by the file gateway. Possible values are @S3_STANDARD@ , @S3_STANDARD_IA@ , or @S3_ONEZONE_IA@ . If this field is not populated, the default value @S3_STANDARD@ is used. Optional.
usmbfsDefaultStorageClass :: Lens' UpdateSMBFileShare (Maybe Text)
usmbfsDefaultStorageClass = lens _usmbfsDefaultStorageClass (\ s a -> s{_usmbfsDefaultStorageClass = a})

-- | A value that sets the access control list permission for objects in the Amazon S3 bucket that a file gateway puts objects into. The default value is @private@ .
usmbfsRequesterPays :: Lens' UpdateSMBFileShare (Maybe Bool)
usmbfsRequesterPays = lens _usmbfsRequesterPays (\ s a -> s{_usmbfsRequesterPays = a})

-- | A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to true to enable MIME type guessing, and otherwise to false. The default value is true.
usmbfsGuessMIMETypeEnabled :: Lens' UpdateSMBFileShare (Maybe Bool)
usmbfsGuessMIMETypeEnabled = lens _usmbfsGuessMIMETypeEnabled (\ s a -> s{_usmbfsGuessMIMETypeEnabled = a})

-- | A value that sets the write status of a file share. This value is true if the write status is read-only, and otherwise false.
usmbfsReadOnly :: Lens' UpdateSMBFileShare (Maybe Bool)
usmbfsReadOnly = lens _usmbfsReadOnly (\ s a -> s{_usmbfsReadOnly = a})

-- | The Amazon Resource Name (ARN) of the SMB file share that you want to update.
usmbfsFileShareARN :: Lens' UpdateSMBFileShare Text
usmbfsFileShareARN = lens _usmbfsFileShareARN (\ s a -> s{_usmbfsFileShareARN = a})

instance AWSRequest UpdateSMBFileShare where
        type Rs UpdateSMBFileShare =
             UpdateSMBFileShareResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 UpdateSMBFileShareResponse' <$>
                   (x .?> "FileShareARN") <*> (pure (fromEnum s)))

instance Hashable UpdateSMBFileShare where

instance NFData UpdateSMBFileShare where

instance ToHeaders UpdateSMBFileShare where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.UpdateSMBFileShare" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateSMBFileShare where
        toJSON UpdateSMBFileShare'{..}
          = object
              (catMaybes
                 [("InvalidUserList" .=) <$> _usmbfsInvalidUserList,
                  ("KMSKey" .=) <$> _usmbfsKMSKey,
                  ("ValidUserList" .=) <$> _usmbfsValidUserList,
                  ("ObjectACL" .=) <$> _usmbfsObjectACL,
                  ("KMSEncrypted" .=) <$> _usmbfsKMSEncrypted,
                  ("DefaultStorageClass" .=) <$>
                    _usmbfsDefaultStorageClass,
                  ("RequesterPays" .=) <$> _usmbfsRequesterPays,
                  ("GuessMIMETypeEnabled" .=) <$>
                    _usmbfsGuessMIMETypeEnabled,
                  ("ReadOnly" .=) <$> _usmbfsReadOnly,
                  Just ("FileShareARN" .= _usmbfsFileShareARN)])

instance ToPath UpdateSMBFileShare where
        toPath = const "/"

instance ToQuery UpdateSMBFileShare where
        toQuery = const mempty

-- | UpdateSMBFileShareOutput
--
--
--
-- /See:/ 'updateSMBFileShareResponse' smart constructor.
data UpdateSMBFileShareResponse = UpdateSMBFileShareResponse'
  { _usmbfsrsFileShareARN   :: !(Maybe Text)
  , _usmbfsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateSMBFileShareResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usmbfsrsFileShareARN' - The Amazon Resource Name (ARN) of the updated SMB file share.
--
-- * 'usmbfsrsResponseStatus' - -- | The response status code.
updateSMBFileShareResponse
    :: Int -- ^ 'usmbfsrsResponseStatus'
    -> UpdateSMBFileShareResponse
updateSMBFileShareResponse pResponseStatus_ =
  UpdateSMBFileShareResponse'
    { _usmbfsrsFileShareARN = Nothing
    , _usmbfsrsResponseStatus = pResponseStatus_
    }


-- | The Amazon Resource Name (ARN) of the updated SMB file share.
usmbfsrsFileShareARN :: Lens' UpdateSMBFileShareResponse (Maybe Text)
usmbfsrsFileShareARN = lens _usmbfsrsFileShareARN (\ s a -> s{_usmbfsrsFileShareARN = a})

-- | -- | The response status code.
usmbfsrsResponseStatus :: Lens' UpdateSMBFileShareResponse Int
usmbfsrsResponseStatus = lens _usmbfsrsResponseStatus (\ s a -> s{_usmbfsrsResponseStatus = a})

instance NFData UpdateSMBFileShareResponse where
