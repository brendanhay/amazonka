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
-- Module      : Network.AWS.StorageGateway.CreateSMBFileShare
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Server Message Block (SMB) file share on an existing file gateway. In Storage Gateway, a file share is a file system mount point backed by Amazon S3 cloud storage. Storage Gateway expose file shares using a SMB interface. This operation is only supported for file gateways.
--
--
-- /Important:/ File gateways require AWS Security Token Service (AWS STS) to be activated to enable you to create a file share. Make sure that AWS STS is activated in the AWS Region you are creating your file gateway in. If AWS STS is not activated in this AWS Region, activate it. For information about how to activate AWS STS, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and Deactivating AWS STS in an AWS Region> in the /AWS Identity and Access Management User Guide./
--
-- File gateways don't support creating hard or symbolic links on a file share.
--
module Network.AWS.StorageGateway.CreateSMBFileShare
    (
    -- * Creating a Request
      createSMBFileShare
    , CreateSMBFileShare
    -- * Request Lenses
    , csmbfsInvalidUserList
    , csmbfsKMSKey
    , csmbfsValidUserList
    , csmbfsAuthentication
    , csmbfsObjectACL
    , csmbfsKMSEncrypted
    , csmbfsDefaultStorageClass
    , csmbfsRequesterPays
    , csmbfsGuessMIMETypeEnabled
    , csmbfsReadOnly
    , csmbfsTags
    , csmbfsClientToken
    , csmbfsGatewayARN
    , csmbfsRole
    , csmbfsLocationARN

    -- * Destructuring the Response
    , createSMBFileShareResponse
    , CreateSMBFileShareResponse
    -- * Response Lenses
    , csmbfsrsFileShareARN
    , csmbfsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | CreateSMBFileShareInput
--
--
--
-- /See:/ 'createSMBFileShare' smart constructor.
data CreateSMBFileShare = CreateSMBFileShare'
  { _csmbfsInvalidUserList      :: !(Maybe [Text])
  , _csmbfsKMSKey               :: !(Maybe Text)
  , _csmbfsValidUserList        :: !(Maybe [Text])
  , _csmbfsAuthentication       :: !(Maybe Text)
  , _csmbfsObjectACL            :: !(Maybe ObjectACL)
  , _csmbfsKMSEncrypted         :: !(Maybe Bool)
  , _csmbfsDefaultStorageClass  :: !(Maybe Text)
  , _csmbfsRequesterPays        :: !(Maybe Bool)
  , _csmbfsGuessMIMETypeEnabled :: !(Maybe Bool)
  , _csmbfsReadOnly             :: !(Maybe Bool)
  , _csmbfsTags                 :: !(Maybe [Tag])
  , _csmbfsClientToken          :: !Text
  , _csmbfsGatewayARN           :: !Text
  , _csmbfsRole                 :: !Text
  , _csmbfsLocationARN          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSMBFileShare' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csmbfsInvalidUserList' - A list of users or groups in the Active Directory that are not allowed to access the file share. A group must be prefixed with the @ character. For example @@group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
--
-- * 'csmbfsKMSKey' - The Amazon Resource Name (ARN) of the AWS KMS key used for Amazon S3 server side encryption. This value can only be set when KMSEncrypted is true. Optional.
--
-- * 'csmbfsValidUserList' - A list of users or groups in the Active Directory that are allowed to access the file share. A group must be prefixed with the @ character. For example @@group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
--
-- * 'csmbfsAuthentication' - The authentication method that users use to access the file share. Valid values are @ActiveDirectory@ or @GuestAccess@ . The default is @ActiveDirectory@ .
--
-- * 'csmbfsObjectACL' - A value that sets the access control list permission for objects in the S3 bucket that a file gateway puts objects into. The default value is "private".
--
-- * 'csmbfsKMSEncrypted' - True to use Amazon S3 server side encryption with your own AWS KMS key, or false to use a key managed by Amazon S3. Optional.
--
-- * 'csmbfsDefaultStorageClass' - The default storage class for objects put into an Amazon S3 bucket by the file gateway. Possible values are @S3_STANDARD@ , @S3_STANDARD_IA@ , or @S3_ONEZONE_IA@ . If this field is not populated, the default value @S3_STANDARD@ is used. Optional.
--
-- * 'csmbfsRequesterPays' - A value that sets the access control list permission for objects in the Amazon S3 bucket that a file gateway puts objects into. The default value is @private@ .
--
-- * 'csmbfsGuessMIMETypeEnabled' - A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to true to enable MIME type guessing, and otherwise to false. The default value is true.
--
-- * 'csmbfsReadOnly' - A value that sets the write status of a file share. This value is true if the write status is read-only, and otherwise false.
--
-- * 'csmbfsTags' - A list of up to ten (10) tags can be assigned to the NFS file share. Every tag is a key-value pair.
--
-- * 'csmbfsClientToken' - A unique string value that you supply that is used by file gateway to ensure idempotent file share creation.
--
-- * 'csmbfsGatewayARN' - The Amazon Resource Name (ARN) of the file gateway on which you want to create a file share.
--
-- * 'csmbfsRole' - The ARN of the AWS Identity and Access Management (IAM) role that a file gateway assumes when it accesses the underlying storage.
--
-- * 'csmbfsLocationARN' - The ARN of the backed storage used for storing file data.
createSMBFileShare
    :: Text -- ^ 'csmbfsClientToken'
    -> Text -- ^ 'csmbfsGatewayARN'
    -> Text -- ^ 'csmbfsRole'
    -> Text -- ^ 'csmbfsLocationARN'
    -> CreateSMBFileShare
createSMBFileShare pClientToken_ pGatewayARN_ pRole_ pLocationARN_ =
  CreateSMBFileShare'
    { _csmbfsInvalidUserList = Nothing
    , _csmbfsKMSKey = Nothing
    , _csmbfsValidUserList = Nothing
    , _csmbfsAuthentication = Nothing
    , _csmbfsObjectACL = Nothing
    , _csmbfsKMSEncrypted = Nothing
    , _csmbfsDefaultStorageClass = Nothing
    , _csmbfsRequesterPays = Nothing
    , _csmbfsGuessMIMETypeEnabled = Nothing
    , _csmbfsReadOnly = Nothing
    , _csmbfsTags = Nothing
    , _csmbfsClientToken = pClientToken_
    , _csmbfsGatewayARN = pGatewayARN_
    , _csmbfsRole = pRole_
    , _csmbfsLocationARN = pLocationARN_
    }


-- | A list of users or groups in the Active Directory that are not allowed to access the file share. A group must be prefixed with the @ character. For example @@group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
csmbfsInvalidUserList :: Lens' CreateSMBFileShare [Text]
csmbfsInvalidUserList = lens _csmbfsInvalidUserList (\ s a -> s{_csmbfsInvalidUserList = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the AWS KMS key used for Amazon S3 server side encryption. This value can only be set when KMSEncrypted is true. Optional.
csmbfsKMSKey :: Lens' CreateSMBFileShare (Maybe Text)
csmbfsKMSKey = lens _csmbfsKMSKey (\ s a -> s{_csmbfsKMSKey = a})

-- | A list of users or groups in the Active Directory that are allowed to access the file share. A group must be prefixed with the @ character. For example @@group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
csmbfsValidUserList :: Lens' CreateSMBFileShare [Text]
csmbfsValidUserList = lens _csmbfsValidUserList (\ s a -> s{_csmbfsValidUserList = a}) . _Default . _Coerce

-- | The authentication method that users use to access the file share. Valid values are @ActiveDirectory@ or @GuestAccess@ . The default is @ActiveDirectory@ .
csmbfsAuthentication :: Lens' CreateSMBFileShare (Maybe Text)
csmbfsAuthentication = lens _csmbfsAuthentication (\ s a -> s{_csmbfsAuthentication = a})

-- | A value that sets the access control list permission for objects in the S3 bucket that a file gateway puts objects into. The default value is "private".
csmbfsObjectACL :: Lens' CreateSMBFileShare (Maybe ObjectACL)
csmbfsObjectACL = lens _csmbfsObjectACL (\ s a -> s{_csmbfsObjectACL = a})

-- | True to use Amazon S3 server side encryption with your own AWS KMS key, or false to use a key managed by Amazon S3. Optional.
csmbfsKMSEncrypted :: Lens' CreateSMBFileShare (Maybe Bool)
csmbfsKMSEncrypted = lens _csmbfsKMSEncrypted (\ s a -> s{_csmbfsKMSEncrypted = a})

-- | The default storage class for objects put into an Amazon S3 bucket by the file gateway. Possible values are @S3_STANDARD@ , @S3_STANDARD_IA@ , or @S3_ONEZONE_IA@ . If this field is not populated, the default value @S3_STANDARD@ is used. Optional.
csmbfsDefaultStorageClass :: Lens' CreateSMBFileShare (Maybe Text)
csmbfsDefaultStorageClass = lens _csmbfsDefaultStorageClass (\ s a -> s{_csmbfsDefaultStorageClass = a})

-- | A value that sets the access control list permission for objects in the Amazon S3 bucket that a file gateway puts objects into. The default value is @private@ .
csmbfsRequesterPays :: Lens' CreateSMBFileShare (Maybe Bool)
csmbfsRequesterPays = lens _csmbfsRequesterPays (\ s a -> s{_csmbfsRequesterPays = a})

-- | A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to true to enable MIME type guessing, and otherwise to false. The default value is true.
csmbfsGuessMIMETypeEnabled :: Lens' CreateSMBFileShare (Maybe Bool)
csmbfsGuessMIMETypeEnabled = lens _csmbfsGuessMIMETypeEnabled (\ s a -> s{_csmbfsGuessMIMETypeEnabled = a})

-- | A value that sets the write status of a file share. This value is true if the write status is read-only, and otherwise false.
csmbfsReadOnly :: Lens' CreateSMBFileShare (Maybe Bool)
csmbfsReadOnly = lens _csmbfsReadOnly (\ s a -> s{_csmbfsReadOnly = a})

-- | A list of up to ten (10) tags can be assigned to the NFS file share. Every tag is a key-value pair.
csmbfsTags :: Lens' CreateSMBFileShare [Tag]
csmbfsTags = lens _csmbfsTags (\ s a -> s{_csmbfsTags = a}) . _Default . _Coerce

-- | A unique string value that you supply that is used by file gateway to ensure idempotent file share creation.
csmbfsClientToken :: Lens' CreateSMBFileShare Text
csmbfsClientToken = lens _csmbfsClientToken (\ s a -> s{_csmbfsClientToken = a})

-- | The Amazon Resource Name (ARN) of the file gateway on which you want to create a file share.
csmbfsGatewayARN :: Lens' CreateSMBFileShare Text
csmbfsGatewayARN = lens _csmbfsGatewayARN (\ s a -> s{_csmbfsGatewayARN = a})

-- | The ARN of the AWS Identity and Access Management (IAM) role that a file gateway assumes when it accesses the underlying storage.
csmbfsRole :: Lens' CreateSMBFileShare Text
csmbfsRole = lens _csmbfsRole (\ s a -> s{_csmbfsRole = a})

-- | The ARN of the backed storage used for storing file data.
csmbfsLocationARN :: Lens' CreateSMBFileShare Text
csmbfsLocationARN = lens _csmbfsLocationARN (\ s a -> s{_csmbfsLocationARN = a})

instance AWSRequest CreateSMBFileShare where
        type Rs CreateSMBFileShare =
             CreateSMBFileShareResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 CreateSMBFileShareResponse' <$>
                   (x .?> "FileShareARN") <*> (pure (fromEnum s)))

instance Hashable CreateSMBFileShare where

instance NFData CreateSMBFileShare where

instance ToHeaders CreateSMBFileShare where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.CreateSMBFileShare" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateSMBFileShare where
        toJSON CreateSMBFileShare'{..}
          = object
              (catMaybes
                 [("InvalidUserList" .=) <$> _csmbfsInvalidUserList,
                  ("KMSKey" .=) <$> _csmbfsKMSKey,
                  ("ValidUserList" .=) <$> _csmbfsValidUserList,
                  ("Authentication" .=) <$> _csmbfsAuthentication,
                  ("ObjectACL" .=) <$> _csmbfsObjectACL,
                  ("KMSEncrypted" .=) <$> _csmbfsKMSEncrypted,
                  ("DefaultStorageClass" .=) <$>
                    _csmbfsDefaultStorageClass,
                  ("RequesterPays" .=) <$> _csmbfsRequesterPays,
                  ("GuessMIMETypeEnabled" .=) <$>
                    _csmbfsGuessMIMETypeEnabled,
                  ("ReadOnly" .=) <$> _csmbfsReadOnly,
                  ("Tags" .=) <$> _csmbfsTags,
                  Just ("ClientToken" .= _csmbfsClientToken),
                  Just ("GatewayARN" .= _csmbfsGatewayARN),
                  Just ("Role" .= _csmbfsRole),
                  Just ("LocationARN" .= _csmbfsLocationARN)])

instance ToPath CreateSMBFileShare where
        toPath = const "/"

instance ToQuery CreateSMBFileShare where
        toQuery = const mempty

-- | CreateSMBFileShareOutput
--
--
--
-- /See:/ 'createSMBFileShareResponse' smart constructor.
data CreateSMBFileShareResponse = CreateSMBFileShareResponse'
  { _csmbfsrsFileShareARN   :: !(Maybe Text)
  , _csmbfsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSMBFileShareResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csmbfsrsFileShareARN' - The Amazon Resource Name (ARN) of the newly created file share.
--
-- * 'csmbfsrsResponseStatus' - -- | The response status code.
createSMBFileShareResponse
    :: Int -- ^ 'csmbfsrsResponseStatus'
    -> CreateSMBFileShareResponse
createSMBFileShareResponse pResponseStatus_ =
  CreateSMBFileShareResponse'
    { _csmbfsrsFileShareARN = Nothing
    , _csmbfsrsResponseStatus = pResponseStatus_
    }


-- | The Amazon Resource Name (ARN) of the newly created file share.
csmbfsrsFileShareARN :: Lens' CreateSMBFileShareResponse (Maybe Text)
csmbfsrsFileShareARN = lens _csmbfsrsFileShareARN (\ s a -> s{_csmbfsrsFileShareARN = a})

-- | -- | The response status code.
csmbfsrsResponseStatus :: Lens' CreateSMBFileShareResponse Int
csmbfsrsResponseStatus = lens _csmbfsrsResponseStatus (\ s a -> s{_csmbfsrsResponseStatus = a})

instance NFData CreateSMBFileShareResponse where
