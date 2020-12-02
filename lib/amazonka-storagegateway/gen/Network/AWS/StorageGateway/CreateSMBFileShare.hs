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
-- Module      : Network.AWS.StorageGateway.CreateSMBFileShare
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Server Message Block (SMB) file share on an existing file gateway. In Storage Gateway, a file share is a file system mount point backed by Amazon S3 cloud storage. Storage Gateway exposes file shares using an SMB interface. This operation is only supported for file gateways.
--
--
-- /Important:/ File gateways require AWS Security Token Service (AWS STS) to be activated to enable you to create a file share. Make sure that AWS STS is activated in the AWS Region you are creating your file gateway in. If AWS STS is not activated in this AWS Region, activate it. For information about how to activate AWS STS, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and deactivating AWS STS in an AWS Region> in the /AWS Identity and Access Management User Guide/ .
--
-- File gateways don't support creating hard or symbolic links on a file share.
module Network.AWS.StorageGateway.CreateSMBFileShare
  ( -- * Creating a Request
    createSMBFileShare,
    CreateSMBFileShare,

    -- * Request Lenses
    csmbfsAccessBasedEnumeration,
    csmbfsAdminUserList,
    csmbfsAuditDestinationARN,
    csmbfsInvalidUserList,
    csmbfsKMSKey,
    csmbfsValidUserList,
    csmbfsAuthentication,
    csmbfsCacheAttributes,
    csmbfsObjectACL,
    csmbfsKMSEncrypted,
    csmbfsDefaultStorageClass,
    csmbfsFileShareName,
    csmbfsSMBACLEnabled,
    csmbfsNotificationPolicy,
    csmbfsRequesterPays,
    csmbfsGuessMIMETypeEnabled,
    csmbfsReadOnly,
    csmbfsCaseSensitivity,
    csmbfsTags,
    csmbfsClientToken,
    csmbfsGatewayARN,
    csmbfsRole,
    csmbfsLocationARN,

    -- * Destructuring the Response
    createSMBFileShareResponse,
    CreateSMBFileShareResponse,

    -- * Response Lenses
    csmbfsrsFileShareARN,
    csmbfsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types

-- | CreateSMBFileShareInput
--
--
--
-- /See:/ 'createSMBFileShare' smart constructor.
data CreateSMBFileShare = CreateSMBFileShare'
  { _csmbfsAccessBasedEnumeration ::
      !(Maybe Bool),
    _csmbfsAdminUserList :: !(Maybe [Text]),
    _csmbfsAuditDestinationARN :: !(Maybe Text),
    _csmbfsInvalidUserList :: !(Maybe [Text]),
    _csmbfsKMSKey :: !(Maybe Text),
    _csmbfsValidUserList :: !(Maybe [Text]),
    _csmbfsAuthentication :: !(Maybe Text),
    _csmbfsCacheAttributes :: !(Maybe CacheAttributes),
    _csmbfsObjectACL :: !(Maybe ObjectACL),
    _csmbfsKMSEncrypted :: !(Maybe Bool),
    _csmbfsDefaultStorageClass :: !(Maybe Text),
    _csmbfsFileShareName :: !(Maybe Text),
    _csmbfsSMBACLEnabled :: !(Maybe Bool),
    _csmbfsNotificationPolicy :: !(Maybe Text),
    _csmbfsRequesterPays :: !(Maybe Bool),
    _csmbfsGuessMIMETypeEnabled :: !(Maybe Bool),
    _csmbfsReadOnly :: !(Maybe Bool),
    _csmbfsCaseSensitivity :: !(Maybe CaseSensitivity),
    _csmbfsTags :: !(Maybe [Tag]),
    _csmbfsClientToken :: !Text,
    _csmbfsGatewayARN :: !Text,
    _csmbfsRole :: !Text,
    _csmbfsLocationARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateSMBFileShare' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csmbfsAccessBasedEnumeration' - The files and folders on this share will only be visible to users with read access.
--
-- * 'csmbfsAdminUserList' - A list of users or groups in the Active Directory that will be granted administrator privileges on the file share. These users can do all file operations as the super-user. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . /Important:/ Use this option very carefully, because any user in this list can do anything they like on the file share, regardless of file permissions.
--
-- * 'csmbfsAuditDestinationARN' - The Amazon Resource Name (ARN) of the storage used for the audit logs.
--
-- * 'csmbfsInvalidUserList' - A list of users or groups in the Active Directory that are not allowed to access the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
--
-- * 'csmbfsKMSKey' - The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
--
-- * 'csmbfsValidUserList' - A list of users or groups in the Active Directory that are allowed to access the file < > share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
--
-- * 'csmbfsAuthentication' - The authentication method that users use to access the file share. The default is @ActiveDirectory@ . Valid Values: @ActiveDirectory@ | @GuestAccess@
--
-- * 'csmbfsCacheAttributes' - Refresh cache information.
--
-- * 'csmbfsObjectACL' - A value that sets the access control list (ACL) permission for objects in the S3 bucket that a file gateway puts objects into. The default value is @private@ .
--
-- * 'csmbfsKMSEncrypted' - Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional. Valid Values: @true@ | @false@
--
-- * 'csmbfsDefaultStorageClass' - The default storage class for objects put into an Amazon S3 bucket by the file gateway. The default value is @S3_INTELLIGENT_TIERING@ . Optional. Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ | @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
--
-- * 'csmbfsFileShareName' - The name of the file share. Optional.
--
-- * 'csmbfsSMBACLEnabled' - Set this value to @true@ to enable access control list (ACL) on the SMB file share. Set it to @false@ to map file and directory permissions to the POSIX permissions. For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share> in the /AWS Storage Gateway User Guide/ . Valid Values: @true@ | @false@
--
-- * 'csmbfsNotificationPolicy' - The notification policy of the file share.
--
-- * 'csmbfsRequesterPays' - A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to @true@ , the requester pays the costs; otherwise, the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data. Valid Values: @true@ | @false@
--
-- * 'csmbfsGuessMIMETypeEnabled' - A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to @true@ to enable MIME type guessing, otherwise set to @false@ . The default value is @true@ . Valid Values: @true@ | @false@
--
-- * 'csmbfsReadOnly' - A value that sets the write status of a file share. Set this value to @true@ to set the write status to read-only, otherwise set to @false@ . Valid Values: @true@ | @false@
--
-- * 'csmbfsCaseSensitivity' - The case of an object name in an Amazon S3 bucket. For @ClientSpecified@ , the client determines the case sensitivity. For @CaseSensitive@ , the gateway determines the case sensitivity. The default value is @ClientSpecified@ .
--
-- * 'csmbfsTags' - A list of up to 50 tags that can be assigned to the NFS file share. Each tag is a key-value pair.
--
-- * 'csmbfsClientToken' - A unique string value that you supply that is used by file gateway to ensure idempotent file share creation.
--
-- * 'csmbfsGatewayARN' - The ARN of the file gateway on which you want to create a file share.
--
-- * 'csmbfsRole' - The ARN of the AWS Identity and Access Management (IAM) role that a file gateway assumes when it accesses the underlying storage.
--
-- * 'csmbfsLocationARN' - The ARN of the backend storage used for storing file data. A prefix name can be added to the S3 bucket name. It must end with a "/".
createSMBFileShare ::
  -- | 'csmbfsClientToken'
  Text ->
  -- | 'csmbfsGatewayARN'
  Text ->
  -- | 'csmbfsRole'
  Text ->
  -- | 'csmbfsLocationARN'
  Text ->
  CreateSMBFileShare
createSMBFileShare pClientToken_ pGatewayARN_ pRole_ pLocationARN_ =
  CreateSMBFileShare'
    { _csmbfsAccessBasedEnumeration = Nothing,
      _csmbfsAdminUserList = Nothing,
      _csmbfsAuditDestinationARN = Nothing,
      _csmbfsInvalidUserList = Nothing,
      _csmbfsKMSKey = Nothing,
      _csmbfsValidUserList = Nothing,
      _csmbfsAuthentication = Nothing,
      _csmbfsCacheAttributes = Nothing,
      _csmbfsObjectACL = Nothing,
      _csmbfsKMSEncrypted = Nothing,
      _csmbfsDefaultStorageClass = Nothing,
      _csmbfsFileShareName = Nothing,
      _csmbfsSMBACLEnabled = Nothing,
      _csmbfsNotificationPolicy = Nothing,
      _csmbfsRequesterPays = Nothing,
      _csmbfsGuessMIMETypeEnabled = Nothing,
      _csmbfsReadOnly = Nothing,
      _csmbfsCaseSensitivity = Nothing,
      _csmbfsTags = Nothing,
      _csmbfsClientToken = pClientToken_,
      _csmbfsGatewayARN = pGatewayARN_,
      _csmbfsRole = pRole_,
      _csmbfsLocationARN = pLocationARN_
    }

-- | The files and folders on this share will only be visible to users with read access.
csmbfsAccessBasedEnumeration :: Lens' CreateSMBFileShare (Maybe Bool)
csmbfsAccessBasedEnumeration = lens _csmbfsAccessBasedEnumeration (\s a -> s {_csmbfsAccessBasedEnumeration = a})

-- | A list of users or groups in the Active Directory that will be granted administrator privileges on the file share. These users can do all file operations as the super-user. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . /Important:/ Use this option very carefully, because any user in this list can do anything they like on the file share, regardless of file permissions.
csmbfsAdminUserList :: Lens' CreateSMBFileShare [Text]
csmbfsAdminUserList = lens _csmbfsAdminUserList (\s a -> s {_csmbfsAdminUserList = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the storage used for the audit logs.
csmbfsAuditDestinationARN :: Lens' CreateSMBFileShare (Maybe Text)
csmbfsAuditDestinationARN = lens _csmbfsAuditDestinationARN (\s a -> s {_csmbfsAuditDestinationARN = a})

-- | A list of users or groups in the Active Directory that are not allowed to access the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
csmbfsInvalidUserList :: Lens' CreateSMBFileShare [Text]
csmbfsInvalidUserList = lens _csmbfsInvalidUserList (\s a -> s {_csmbfsInvalidUserList = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
csmbfsKMSKey :: Lens' CreateSMBFileShare (Maybe Text)
csmbfsKMSKey = lens _csmbfsKMSKey (\s a -> s {_csmbfsKMSKey = a})

-- | A list of users or groups in the Active Directory that are allowed to access the file < > share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
csmbfsValidUserList :: Lens' CreateSMBFileShare [Text]
csmbfsValidUserList = lens _csmbfsValidUserList (\s a -> s {_csmbfsValidUserList = a}) . _Default . _Coerce

-- | The authentication method that users use to access the file share. The default is @ActiveDirectory@ . Valid Values: @ActiveDirectory@ | @GuestAccess@
csmbfsAuthentication :: Lens' CreateSMBFileShare (Maybe Text)
csmbfsAuthentication = lens _csmbfsAuthentication (\s a -> s {_csmbfsAuthentication = a})

-- | Refresh cache information.
csmbfsCacheAttributes :: Lens' CreateSMBFileShare (Maybe CacheAttributes)
csmbfsCacheAttributes = lens _csmbfsCacheAttributes (\s a -> s {_csmbfsCacheAttributes = a})

-- | A value that sets the access control list (ACL) permission for objects in the S3 bucket that a file gateway puts objects into. The default value is @private@ .
csmbfsObjectACL :: Lens' CreateSMBFileShare (Maybe ObjectACL)
csmbfsObjectACL = lens _csmbfsObjectACL (\s a -> s {_csmbfsObjectACL = a})

-- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional. Valid Values: @true@ | @false@
csmbfsKMSEncrypted :: Lens' CreateSMBFileShare (Maybe Bool)
csmbfsKMSEncrypted = lens _csmbfsKMSEncrypted (\s a -> s {_csmbfsKMSEncrypted = a})

-- | The default storage class for objects put into an Amazon S3 bucket by the file gateway. The default value is @S3_INTELLIGENT_TIERING@ . Optional. Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ | @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
csmbfsDefaultStorageClass :: Lens' CreateSMBFileShare (Maybe Text)
csmbfsDefaultStorageClass = lens _csmbfsDefaultStorageClass (\s a -> s {_csmbfsDefaultStorageClass = a})

-- | The name of the file share. Optional.
csmbfsFileShareName :: Lens' CreateSMBFileShare (Maybe Text)
csmbfsFileShareName = lens _csmbfsFileShareName (\s a -> s {_csmbfsFileShareName = a})

-- | Set this value to @true@ to enable access control list (ACL) on the SMB file share. Set it to @false@ to map file and directory permissions to the POSIX permissions. For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share> in the /AWS Storage Gateway User Guide/ . Valid Values: @true@ | @false@
csmbfsSMBACLEnabled :: Lens' CreateSMBFileShare (Maybe Bool)
csmbfsSMBACLEnabled = lens _csmbfsSMBACLEnabled (\s a -> s {_csmbfsSMBACLEnabled = a})

-- | The notification policy of the file share.
csmbfsNotificationPolicy :: Lens' CreateSMBFileShare (Maybe Text)
csmbfsNotificationPolicy = lens _csmbfsNotificationPolicy (\s a -> s {_csmbfsNotificationPolicy = a})

-- | A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to @true@ , the requester pays the costs; otherwise, the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data. Valid Values: @true@ | @false@
csmbfsRequesterPays :: Lens' CreateSMBFileShare (Maybe Bool)
csmbfsRequesterPays = lens _csmbfsRequesterPays (\s a -> s {_csmbfsRequesterPays = a})

-- | A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to @true@ to enable MIME type guessing, otherwise set to @false@ . The default value is @true@ . Valid Values: @true@ | @false@
csmbfsGuessMIMETypeEnabled :: Lens' CreateSMBFileShare (Maybe Bool)
csmbfsGuessMIMETypeEnabled = lens _csmbfsGuessMIMETypeEnabled (\s a -> s {_csmbfsGuessMIMETypeEnabled = a})

-- | A value that sets the write status of a file share. Set this value to @true@ to set the write status to read-only, otherwise set to @false@ . Valid Values: @true@ | @false@
csmbfsReadOnly :: Lens' CreateSMBFileShare (Maybe Bool)
csmbfsReadOnly = lens _csmbfsReadOnly (\s a -> s {_csmbfsReadOnly = a})

-- | The case of an object name in an Amazon S3 bucket. For @ClientSpecified@ , the client determines the case sensitivity. For @CaseSensitive@ , the gateway determines the case sensitivity. The default value is @ClientSpecified@ .
csmbfsCaseSensitivity :: Lens' CreateSMBFileShare (Maybe CaseSensitivity)
csmbfsCaseSensitivity = lens _csmbfsCaseSensitivity (\s a -> s {_csmbfsCaseSensitivity = a})

-- | A list of up to 50 tags that can be assigned to the NFS file share. Each tag is a key-value pair.
csmbfsTags :: Lens' CreateSMBFileShare [Tag]
csmbfsTags = lens _csmbfsTags (\s a -> s {_csmbfsTags = a}) . _Default . _Coerce

-- | A unique string value that you supply that is used by file gateway to ensure idempotent file share creation.
csmbfsClientToken :: Lens' CreateSMBFileShare Text
csmbfsClientToken = lens _csmbfsClientToken (\s a -> s {_csmbfsClientToken = a})

-- | The ARN of the file gateway on which you want to create a file share.
csmbfsGatewayARN :: Lens' CreateSMBFileShare Text
csmbfsGatewayARN = lens _csmbfsGatewayARN (\s a -> s {_csmbfsGatewayARN = a})

-- | The ARN of the AWS Identity and Access Management (IAM) role that a file gateway assumes when it accesses the underlying storage.
csmbfsRole :: Lens' CreateSMBFileShare Text
csmbfsRole = lens _csmbfsRole (\s a -> s {_csmbfsRole = a})

-- | The ARN of the backend storage used for storing file data. A prefix name can be added to the S3 bucket name. It must end with a "/".
csmbfsLocationARN :: Lens' CreateSMBFileShare Text
csmbfsLocationARN = lens _csmbfsLocationARN (\s a -> s {_csmbfsLocationARN = a})

instance AWSRequest CreateSMBFileShare where
  type Rs CreateSMBFileShare = CreateSMBFileShareResponse
  request = postJSON storageGateway
  response =
    receiveJSON
      ( \s h x ->
          CreateSMBFileShareResponse'
            <$> (x .?> "FileShareARN") <*> (pure (fromEnum s))
      )

instance Hashable CreateSMBFileShare

instance NFData CreateSMBFileShare

instance ToHeaders CreateSMBFileShare where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("StorageGateway_20130630.CreateSMBFileShare" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateSMBFileShare where
  toJSON CreateSMBFileShare' {..} =
    object
      ( catMaybes
          [ ("AccessBasedEnumeration" .=) <$> _csmbfsAccessBasedEnumeration,
            ("AdminUserList" .=) <$> _csmbfsAdminUserList,
            ("AuditDestinationARN" .=) <$> _csmbfsAuditDestinationARN,
            ("InvalidUserList" .=) <$> _csmbfsInvalidUserList,
            ("KMSKey" .=) <$> _csmbfsKMSKey,
            ("ValidUserList" .=) <$> _csmbfsValidUserList,
            ("Authentication" .=) <$> _csmbfsAuthentication,
            ("CacheAttributes" .=) <$> _csmbfsCacheAttributes,
            ("ObjectACL" .=) <$> _csmbfsObjectACL,
            ("KMSEncrypted" .=) <$> _csmbfsKMSEncrypted,
            ("DefaultStorageClass" .=) <$> _csmbfsDefaultStorageClass,
            ("FileShareName" .=) <$> _csmbfsFileShareName,
            ("SMBACLEnabled" .=) <$> _csmbfsSMBACLEnabled,
            ("NotificationPolicy" .=) <$> _csmbfsNotificationPolicy,
            ("RequesterPays" .=) <$> _csmbfsRequesterPays,
            ("GuessMIMETypeEnabled" .=) <$> _csmbfsGuessMIMETypeEnabled,
            ("ReadOnly" .=) <$> _csmbfsReadOnly,
            ("CaseSensitivity" .=) <$> _csmbfsCaseSensitivity,
            ("Tags" .=) <$> _csmbfsTags,
            Just ("ClientToken" .= _csmbfsClientToken),
            Just ("GatewayARN" .= _csmbfsGatewayARN),
            Just ("Role" .= _csmbfsRole),
            Just ("LocationARN" .= _csmbfsLocationARN)
          ]
      )

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
  { _csmbfsrsFileShareARN ::
      !(Maybe Text),
    _csmbfsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateSMBFileShareResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csmbfsrsFileShareARN' - The Amazon Resource Name (ARN) of the newly created file share.
--
-- * 'csmbfsrsResponseStatus' - -- | The response status code.
createSMBFileShareResponse ::
  -- | 'csmbfsrsResponseStatus'
  Int ->
  CreateSMBFileShareResponse
createSMBFileShareResponse pResponseStatus_ =
  CreateSMBFileShareResponse'
    { _csmbfsrsFileShareARN = Nothing,
      _csmbfsrsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the newly created file share.
csmbfsrsFileShareARN :: Lens' CreateSMBFileShareResponse (Maybe Text)
csmbfsrsFileShareARN = lens _csmbfsrsFileShareARN (\s a -> s {_csmbfsrsFileShareARN = a})

-- | -- | The response status code.
csmbfsrsResponseStatus :: Lens' CreateSMBFileShareResponse Int
csmbfsrsResponseStatus = lens _csmbfsrsResponseStatus (\s a -> s {_csmbfsrsResponseStatus = a})

instance NFData CreateSMBFileShareResponse
