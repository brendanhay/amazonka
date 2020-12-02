{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.CreateNFSFileShare
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Network File System (NFS) file share on an existing file gateway. In Storage Gateway, a file share is a file system mount point backed by Amazon S3 cloud storage. Storage Gateway exposes file shares using an NFS interface. This operation is only supported for file gateways.
--
--
-- /Important:/ File gateway requires AWS Security Token Service (AWS STS) to be activated to enable you to create a file share. Make sure AWS STS is activated in the AWS Region you are creating your file gateway in. If AWS STS is not activated in the AWS Region, activate it. For information about how to activate AWS STS, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and deactivating AWS STS in an AWS Region> in the /AWS Identity and Access Management User Guide/ .
--
-- File gateway does not support creating hard or symbolic links on a file share.
module Network.AWS.StorageGateway.CreateNFSFileShare
  ( -- * Creating a Request
    createNFSFileShare,
    CreateNFSFileShare,

    -- * Request Lenses
    cnfsfsKMSKey,
    cnfsfsCacheAttributes,
    cnfsfsObjectACL,
    cnfsfsKMSEncrypted,
    cnfsfsDefaultStorageClass,
    cnfsfsFileShareName,
    cnfsfsNotificationPolicy,
    cnfsfsSquash,
    cnfsfsRequesterPays,
    cnfsfsNFSFileShareDefaults,
    cnfsfsClientList,
    cnfsfsGuessMIMETypeEnabled,
    cnfsfsReadOnly,
    cnfsfsTags,
    cnfsfsClientToken,
    cnfsfsGatewayARN,
    cnfsfsRole,
    cnfsfsLocationARN,

    -- * Destructuring the Response
    createNFSFileShareResponse,
    CreateNFSFileShareResponse,

    -- * Response Lenses
    cnfsfsrsFileShareARN,
    cnfsfsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types

-- | CreateNFSFileShareInput
--
--
--
-- /See:/ 'createNFSFileShare' smart constructor.
data CreateNFSFileShare = CreateNFSFileShare'
  { _cnfsfsKMSKey ::
      !(Maybe Text),
    _cnfsfsCacheAttributes :: !(Maybe CacheAttributes),
    _cnfsfsObjectACL :: !(Maybe ObjectACL),
    _cnfsfsKMSEncrypted :: !(Maybe Bool),
    _cnfsfsDefaultStorageClass :: !(Maybe Text),
    _cnfsfsFileShareName :: !(Maybe Text),
    _cnfsfsNotificationPolicy :: !(Maybe Text),
    _cnfsfsSquash :: !(Maybe Text),
    _cnfsfsRequesterPays :: !(Maybe Bool),
    _cnfsfsNFSFileShareDefaults ::
      !(Maybe NFSFileShareDefaults),
    _cnfsfsClientList :: !(Maybe (List1 Text)),
    _cnfsfsGuessMIMETypeEnabled :: !(Maybe Bool),
    _cnfsfsReadOnly :: !(Maybe Bool),
    _cnfsfsTags :: !(Maybe [Tag]),
    _cnfsfsClientToken :: !Text,
    _cnfsfsGatewayARN :: !Text,
    _cnfsfsRole :: !Text,
    _cnfsfsLocationARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateNFSFileShare' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cnfsfsKMSKey' - The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
--
-- * 'cnfsfsCacheAttributes' - Refresh cache information.
--
-- * 'cnfsfsObjectACL' - A value that sets the access control list (ACL) permission for objects in the S3 bucket that a file gateway puts objects into. The default value is @private@ .
--
-- * 'cnfsfsKMSEncrypted' - Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional. Valid Values: @true@ | @false@
--
-- * 'cnfsfsDefaultStorageClass' - The default storage class for objects put into an Amazon S3 bucket by the file gateway. The default value is @S3_INTELLIGENT_TIERING@ . Optional. Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ | @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
--
-- * 'cnfsfsFileShareName' - The name of the file share. Optional.
--
-- * 'cnfsfsNotificationPolicy' - The notification policy of the file share.
--
-- * 'cnfsfsSquash' - A value that maps a user to anonymous user. Valid values are the following:     * @RootSquash@ : Only root is mapped to anonymous user.     * @NoSquash@ : No one is mapped to anonymous user.     * @AllSquash@ : Everyone is mapped to anonymous user.
--
-- * 'cnfsfsRequesterPays' - A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to @true@ , the requester pays the costs; otherwise, the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data. Valid Values: @true@ | @false@
--
-- * 'cnfsfsNFSFileShareDefaults' - File share default values. Optional.
--
-- * 'cnfsfsClientList' - The list of clients that are allowed to access the file gateway. The list must contain either valid IP addresses or valid CIDR blocks.
--
-- * 'cnfsfsGuessMIMETypeEnabled' - A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to @true@ to enable MIME type guessing, otherwise set to @false@ . The default value is @true@ . Valid Values: @true@ | @false@
--
-- * 'cnfsfsReadOnly' - A value that sets the write status of a file share. Set this value to @true@ to set the write status to read-only, otherwise set to @false@ . Valid Values: @true@ | @false@
--
-- * 'cnfsfsTags' - A list of up to 50 tags that can be assigned to the NFS file share. Each tag is a key-value pair.
--
-- * 'cnfsfsClientToken' - A unique string value that you supply that is used by file gateway to ensure idempotent file share creation.
--
-- * 'cnfsfsGatewayARN' - The Amazon Resource Name (ARN) of the file gateway on which you want to create a file share.
--
-- * 'cnfsfsRole' - The ARN of the AWS Identity and Access Management (IAM) role that a file gateway assumes when it accesses the underlying storage.
--
-- * 'cnfsfsLocationARN' - The ARN of the backend storage used for storing file data. A prefix name can be added to the S3 bucket name. It must end with a "/".
createNFSFileShare ::
  -- | 'cnfsfsClientToken'
  Text ->
  -- | 'cnfsfsGatewayARN'
  Text ->
  -- | 'cnfsfsRole'
  Text ->
  -- | 'cnfsfsLocationARN'
  Text ->
  CreateNFSFileShare
createNFSFileShare pClientToken_ pGatewayARN_ pRole_ pLocationARN_ =
  CreateNFSFileShare'
    { _cnfsfsKMSKey = Nothing,
      _cnfsfsCacheAttributes = Nothing,
      _cnfsfsObjectACL = Nothing,
      _cnfsfsKMSEncrypted = Nothing,
      _cnfsfsDefaultStorageClass = Nothing,
      _cnfsfsFileShareName = Nothing,
      _cnfsfsNotificationPolicy = Nothing,
      _cnfsfsSquash = Nothing,
      _cnfsfsRequesterPays = Nothing,
      _cnfsfsNFSFileShareDefaults = Nothing,
      _cnfsfsClientList = Nothing,
      _cnfsfsGuessMIMETypeEnabled = Nothing,
      _cnfsfsReadOnly = Nothing,
      _cnfsfsTags = Nothing,
      _cnfsfsClientToken = pClientToken_,
      _cnfsfsGatewayARN = pGatewayARN_,
      _cnfsfsRole = pRole_,
      _cnfsfsLocationARN = pLocationARN_
    }

-- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
cnfsfsKMSKey :: Lens' CreateNFSFileShare (Maybe Text)
cnfsfsKMSKey = lens _cnfsfsKMSKey (\s a -> s {_cnfsfsKMSKey = a})

-- | Refresh cache information.
cnfsfsCacheAttributes :: Lens' CreateNFSFileShare (Maybe CacheAttributes)
cnfsfsCacheAttributes = lens _cnfsfsCacheAttributes (\s a -> s {_cnfsfsCacheAttributes = a})

-- | A value that sets the access control list (ACL) permission for objects in the S3 bucket that a file gateway puts objects into. The default value is @private@ .
cnfsfsObjectACL :: Lens' CreateNFSFileShare (Maybe ObjectACL)
cnfsfsObjectACL = lens _cnfsfsObjectACL (\s a -> s {_cnfsfsObjectACL = a})

-- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional. Valid Values: @true@ | @false@
cnfsfsKMSEncrypted :: Lens' CreateNFSFileShare (Maybe Bool)
cnfsfsKMSEncrypted = lens _cnfsfsKMSEncrypted (\s a -> s {_cnfsfsKMSEncrypted = a})

-- | The default storage class for objects put into an Amazon S3 bucket by the file gateway. The default value is @S3_INTELLIGENT_TIERING@ . Optional. Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ | @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
cnfsfsDefaultStorageClass :: Lens' CreateNFSFileShare (Maybe Text)
cnfsfsDefaultStorageClass = lens _cnfsfsDefaultStorageClass (\s a -> s {_cnfsfsDefaultStorageClass = a})

-- | The name of the file share. Optional.
cnfsfsFileShareName :: Lens' CreateNFSFileShare (Maybe Text)
cnfsfsFileShareName = lens _cnfsfsFileShareName (\s a -> s {_cnfsfsFileShareName = a})

-- | The notification policy of the file share.
cnfsfsNotificationPolicy :: Lens' CreateNFSFileShare (Maybe Text)
cnfsfsNotificationPolicy = lens _cnfsfsNotificationPolicy (\s a -> s {_cnfsfsNotificationPolicy = a})

-- | A value that maps a user to anonymous user. Valid values are the following:     * @RootSquash@ : Only root is mapped to anonymous user.     * @NoSquash@ : No one is mapped to anonymous user.     * @AllSquash@ : Everyone is mapped to anonymous user.
cnfsfsSquash :: Lens' CreateNFSFileShare (Maybe Text)
cnfsfsSquash = lens _cnfsfsSquash (\s a -> s {_cnfsfsSquash = a})

-- | A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to @true@ , the requester pays the costs; otherwise, the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data. Valid Values: @true@ | @false@
cnfsfsRequesterPays :: Lens' CreateNFSFileShare (Maybe Bool)
cnfsfsRequesterPays = lens _cnfsfsRequesterPays (\s a -> s {_cnfsfsRequesterPays = a})

-- | File share default values. Optional.
cnfsfsNFSFileShareDefaults :: Lens' CreateNFSFileShare (Maybe NFSFileShareDefaults)
cnfsfsNFSFileShareDefaults = lens _cnfsfsNFSFileShareDefaults (\s a -> s {_cnfsfsNFSFileShareDefaults = a})

-- | The list of clients that are allowed to access the file gateway. The list must contain either valid IP addresses or valid CIDR blocks.
cnfsfsClientList :: Lens' CreateNFSFileShare (Maybe (NonEmpty Text))
cnfsfsClientList = lens _cnfsfsClientList (\s a -> s {_cnfsfsClientList = a}) . mapping _List1

-- | A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to @true@ to enable MIME type guessing, otherwise set to @false@ . The default value is @true@ . Valid Values: @true@ | @false@
cnfsfsGuessMIMETypeEnabled :: Lens' CreateNFSFileShare (Maybe Bool)
cnfsfsGuessMIMETypeEnabled = lens _cnfsfsGuessMIMETypeEnabled (\s a -> s {_cnfsfsGuessMIMETypeEnabled = a})

-- | A value that sets the write status of a file share. Set this value to @true@ to set the write status to read-only, otherwise set to @false@ . Valid Values: @true@ | @false@
cnfsfsReadOnly :: Lens' CreateNFSFileShare (Maybe Bool)
cnfsfsReadOnly = lens _cnfsfsReadOnly (\s a -> s {_cnfsfsReadOnly = a})

-- | A list of up to 50 tags that can be assigned to the NFS file share. Each tag is a key-value pair.
cnfsfsTags :: Lens' CreateNFSFileShare [Tag]
cnfsfsTags = lens _cnfsfsTags (\s a -> s {_cnfsfsTags = a}) . _Default . _Coerce

-- | A unique string value that you supply that is used by file gateway to ensure idempotent file share creation.
cnfsfsClientToken :: Lens' CreateNFSFileShare Text
cnfsfsClientToken = lens _cnfsfsClientToken (\s a -> s {_cnfsfsClientToken = a})

-- | The Amazon Resource Name (ARN) of the file gateway on which you want to create a file share.
cnfsfsGatewayARN :: Lens' CreateNFSFileShare Text
cnfsfsGatewayARN = lens _cnfsfsGatewayARN (\s a -> s {_cnfsfsGatewayARN = a})

-- | The ARN of the AWS Identity and Access Management (IAM) role that a file gateway assumes when it accesses the underlying storage.
cnfsfsRole :: Lens' CreateNFSFileShare Text
cnfsfsRole = lens _cnfsfsRole (\s a -> s {_cnfsfsRole = a})

-- | The ARN of the backend storage used for storing file data. A prefix name can be added to the S3 bucket name. It must end with a "/".
cnfsfsLocationARN :: Lens' CreateNFSFileShare Text
cnfsfsLocationARN = lens _cnfsfsLocationARN (\s a -> s {_cnfsfsLocationARN = a})

instance AWSRequest CreateNFSFileShare where
  type Rs CreateNFSFileShare = CreateNFSFileShareResponse
  request = postJSON storageGateway
  response =
    receiveJSON
      ( \s h x ->
          CreateNFSFileShareResponse'
            <$> (x .?> "FileShareARN") <*> (pure (fromEnum s))
      )

instance Hashable CreateNFSFileShare

instance NFData CreateNFSFileShare

instance ToHeaders CreateNFSFileShare where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("StorageGateway_20130630.CreateNFSFileShare" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateNFSFileShare where
  toJSON CreateNFSFileShare' {..} =
    object
      ( catMaybes
          [ ("KMSKey" .=) <$> _cnfsfsKMSKey,
            ("CacheAttributes" .=) <$> _cnfsfsCacheAttributes,
            ("ObjectACL" .=) <$> _cnfsfsObjectACL,
            ("KMSEncrypted" .=) <$> _cnfsfsKMSEncrypted,
            ("DefaultStorageClass" .=) <$> _cnfsfsDefaultStorageClass,
            ("FileShareName" .=) <$> _cnfsfsFileShareName,
            ("NotificationPolicy" .=) <$> _cnfsfsNotificationPolicy,
            ("Squash" .=) <$> _cnfsfsSquash,
            ("RequesterPays" .=) <$> _cnfsfsRequesterPays,
            ("NFSFileShareDefaults" .=) <$> _cnfsfsNFSFileShareDefaults,
            ("ClientList" .=) <$> _cnfsfsClientList,
            ("GuessMIMETypeEnabled" .=) <$> _cnfsfsGuessMIMETypeEnabled,
            ("ReadOnly" .=) <$> _cnfsfsReadOnly,
            ("Tags" .=) <$> _cnfsfsTags,
            Just ("ClientToken" .= _cnfsfsClientToken),
            Just ("GatewayARN" .= _cnfsfsGatewayARN),
            Just ("Role" .= _cnfsfsRole),
            Just ("LocationARN" .= _cnfsfsLocationARN)
          ]
      )

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
  { _cnfsfsrsFileShareARN ::
      !(Maybe Text),
    _cnfsfsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateNFSFileShareResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cnfsfsrsFileShareARN' - The Amazon Resource Name (ARN) of the newly created file share.
--
-- * 'cnfsfsrsResponseStatus' - -- | The response status code.
createNFSFileShareResponse ::
  -- | 'cnfsfsrsResponseStatus'
  Int ->
  CreateNFSFileShareResponse
createNFSFileShareResponse pResponseStatus_ =
  CreateNFSFileShareResponse'
    { _cnfsfsrsFileShareARN = Nothing,
      _cnfsfsrsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the newly created file share.
cnfsfsrsFileShareARN :: Lens' CreateNFSFileShareResponse (Maybe Text)
cnfsfsrsFileShareARN = lens _cnfsfsrsFileShareARN (\s a -> s {_cnfsfsrsFileShareARN = a})

-- | -- | The response status code.
cnfsfsrsResponseStatus :: Lens' CreateNFSFileShareResponse Int
cnfsfsrsResponseStatus = lens _cnfsfsrsResponseStatus (\s a -> s {_cnfsfsrsResponseStatus = a})

instance NFData CreateNFSFileShareResponse
