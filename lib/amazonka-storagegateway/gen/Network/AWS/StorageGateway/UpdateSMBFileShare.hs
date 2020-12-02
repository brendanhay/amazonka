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
-- Module      : Network.AWS.StorageGateway.UpdateSMBFileShare
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Server Message Block (SMB) file share. This operation is only supported for file gateways.
--
--
-- /Important:/ File gateways require AWS Security Token Service (AWS STS) to be activated to enable you to create a file share. Make sure that AWS STS is activated in the AWS Region you are creating your file gateway in. If AWS STS is not activated in this AWS Region, activate it. For information about how to activate AWS STS, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and deactivating AWS STS in an AWS Region> in the /AWS Identity and Access Management User Guide/ .
--
-- File gateways don't support creating hard or symbolic links on a file share.
module Network.AWS.StorageGateway.UpdateSMBFileShare
  ( -- * Creating a Request
    updateSMBFileShare,
    UpdateSMBFileShare,

    -- * Request Lenses
    usmbfsAccessBasedEnumeration,
    usmbfsAdminUserList,
    usmbfsAuditDestinationARN,
    usmbfsInvalidUserList,
    usmbfsKMSKey,
    usmbfsValidUserList,
    usmbfsCacheAttributes,
    usmbfsObjectACL,
    usmbfsKMSEncrypted,
    usmbfsDefaultStorageClass,
    usmbfsFileShareName,
    usmbfsSMBACLEnabled,
    usmbfsNotificationPolicy,
    usmbfsRequesterPays,
    usmbfsGuessMIMETypeEnabled,
    usmbfsReadOnly,
    usmbfsCaseSensitivity,
    usmbfsFileShareARN,

    -- * Destructuring the Response
    updateSMBFileShareResponse,
    UpdateSMBFileShareResponse,

    -- * Response Lenses
    usmbfsrsFileShareARN,
    usmbfsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types

-- | UpdateSMBFileShareInput
--
--
--
-- /See:/ 'updateSMBFileShare' smart constructor.
data UpdateSMBFileShare = UpdateSMBFileShare'
  { _usmbfsAccessBasedEnumeration ::
      !(Maybe Bool),
    _usmbfsAdminUserList :: !(Maybe [Text]),
    _usmbfsAuditDestinationARN :: !(Maybe Text),
    _usmbfsInvalidUserList :: !(Maybe [Text]),
    _usmbfsKMSKey :: !(Maybe Text),
    _usmbfsValidUserList :: !(Maybe [Text]),
    _usmbfsCacheAttributes :: !(Maybe CacheAttributes),
    _usmbfsObjectACL :: !(Maybe ObjectACL),
    _usmbfsKMSEncrypted :: !(Maybe Bool),
    _usmbfsDefaultStorageClass :: !(Maybe Text),
    _usmbfsFileShareName :: !(Maybe Text),
    _usmbfsSMBACLEnabled :: !(Maybe Bool),
    _usmbfsNotificationPolicy :: !(Maybe Text),
    _usmbfsRequesterPays :: !(Maybe Bool),
    _usmbfsGuessMIMETypeEnabled :: !(Maybe Bool),
    _usmbfsReadOnly :: !(Maybe Bool),
    _usmbfsCaseSensitivity :: !(Maybe CaseSensitivity),
    _usmbfsFileShareARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateSMBFileShare' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usmbfsAccessBasedEnumeration' - The files and folders on this share will only be visible to users with read access.
--
-- * 'usmbfsAdminUserList' - A list of users or groups in the Active Directory that have administrator rights to the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
--
-- * 'usmbfsAuditDestinationARN' - The Amazon Resource Name (ARN) of the storage used for the audit logs.
--
-- * 'usmbfsInvalidUserList' - A list of users or groups in the Active Directory that are not allowed to access the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
--
-- * 'usmbfsKMSKey' - The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
--
-- * 'usmbfsValidUserList' - A list of users or groups in the Active Directory that are allowed to access the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
--
-- * 'usmbfsCacheAttributes' - Refresh cache information.
--
-- * 'usmbfsObjectACL' - A value that sets the access control list (ACL) permission for objects in the S3 bucket that a file gateway puts objects into. The default value is @private@ .
--
-- * 'usmbfsKMSEncrypted' - Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional. Valid Values: @true@ | @false@
--
-- * 'usmbfsDefaultStorageClass' - The default storage class for objects put into an Amazon S3 bucket by the file gateway. The default value is @S3_INTELLIGENT_TIERING@ . Optional. Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ | @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
--
-- * 'usmbfsFileShareName' - The name of the file share. Optional.
--
-- * 'usmbfsSMBACLEnabled' - Set this value to @true@ to enable access control list (ACL) on the SMB file share. Set it to @false@ to map file and directory permissions to the POSIX permissions. For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share> in the /AWS Storage Gateway User Guide/ . Valid Values: @true@ | @false@
--
-- * 'usmbfsNotificationPolicy' - The notification policy of the file share.
--
-- * 'usmbfsRequesterPays' - A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to @true@ , the requester pays the costs; otherwise, the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data. Valid Values: @true@ | @false@
--
-- * 'usmbfsGuessMIMETypeEnabled' - A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to @true@ to enable MIME type guessing, otherwise set to @false@ . The default value is @true@ . Valid Values: @true@ | @false@
--
-- * 'usmbfsReadOnly' - A value that sets the write status of a file share. Set this value to @true@ to set write status to read-only, otherwise set to @false@ . Valid Values: @true@ | @false@
--
-- * 'usmbfsCaseSensitivity' - The case of an object name in an Amazon S3 bucket. For @ClientSpecified@ , the client determines the case sensitivity. For @CaseSensitive@ , the gateway determines the case sensitivity. The default value is @ClientSpecified@ .
--
-- * 'usmbfsFileShareARN' - The Amazon Resource Name (ARN) of the SMB file share that you want to update.
updateSMBFileShare ::
  -- | 'usmbfsFileShareARN'
  Text ->
  UpdateSMBFileShare
updateSMBFileShare pFileShareARN_ =
  UpdateSMBFileShare'
    { _usmbfsAccessBasedEnumeration = Nothing,
      _usmbfsAdminUserList = Nothing,
      _usmbfsAuditDestinationARN = Nothing,
      _usmbfsInvalidUserList = Nothing,
      _usmbfsKMSKey = Nothing,
      _usmbfsValidUserList = Nothing,
      _usmbfsCacheAttributes = Nothing,
      _usmbfsObjectACL = Nothing,
      _usmbfsKMSEncrypted = Nothing,
      _usmbfsDefaultStorageClass = Nothing,
      _usmbfsFileShareName = Nothing,
      _usmbfsSMBACLEnabled = Nothing,
      _usmbfsNotificationPolicy = Nothing,
      _usmbfsRequesterPays = Nothing,
      _usmbfsGuessMIMETypeEnabled = Nothing,
      _usmbfsReadOnly = Nothing,
      _usmbfsCaseSensitivity = Nothing,
      _usmbfsFileShareARN = pFileShareARN_
    }

-- | The files and folders on this share will only be visible to users with read access.
usmbfsAccessBasedEnumeration :: Lens' UpdateSMBFileShare (Maybe Bool)
usmbfsAccessBasedEnumeration = lens _usmbfsAccessBasedEnumeration (\s a -> s {_usmbfsAccessBasedEnumeration = a})

-- | A list of users or groups in the Active Directory that have administrator rights to the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
usmbfsAdminUserList :: Lens' UpdateSMBFileShare [Text]
usmbfsAdminUserList = lens _usmbfsAdminUserList (\s a -> s {_usmbfsAdminUserList = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the storage used for the audit logs.
usmbfsAuditDestinationARN :: Lens' UpdateSMBFileShare (Maybe Text)
usmbfsAuditDestinationARN = lens _usmbfsAuditDestinationARN (\s a -> s {_usmbfsAuditDestinationARN = a})

-- | A list of users or groups in the Active Directory that are not allowed to access the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
usmbfsInvalidUserList :: Lens' UpdateSMBFileShare [Text]
usmbfsInvalidUserList = lens _usmbfsInvalidUserList (\s a -> s {_usmbfsInvalidUserList = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
usmbfsKMSKey :: Lens' UpdateSMBFileShare (Maybe Text)
usmbfsKMSKey = lens _usmbfsKMSKey (\s a -> s {_usmbfsKMSKey = a})

-- | A list of users or groups in the Active Directory that are allowed to access the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
usmbfsValidUserList :: Lens' UpdateSMBFileShare [Text]
usmbfsValidUserList = lens _usmbfsValidUserList (\s a -> s {_usmbfsValidUserList = a}) . _Default . _Coerce

-- | Refresh cache information.
usmbfsCacheAttributes :: Lens' UpdateSMBFileShare (Maybe CacheAttributes)
usmbfsCacheAttributes = lens _usmbfsCacheAttributes (\s a -> s {_usmbfsCacheAttributes = a})

-- | A value that sets the access control list (ACL) permission for objects in the S3 bucket that a file gateway puts objects into. The default value is @private@ .
usmbfsObjectACL :: Lens' UpdateSMBFileShare (Maybe ObjectACL)
usmbfsObjectACL = lens _usmbfsObjectACL (\s a -> s {_usmbfsObjectACL = a})

-- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional. Valid Values: @true@ | @false@
usmbfsKMSEncrypted :: Lens' UpdateSMBFileShare (Maybe Bool)
usmbfsKMSEncrypted = lens _usmbfsKMSEncrypted (\s a -> s {_usmbfsKMSEncrypted = a})

-- | The default storage class for objects put into an Amazon S3 bucket by the file gateway. The default value is @S3_INTELLIGENT_TIERING@ . Optional. Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ | @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
usmbfsDefaultStorageClass :: Lens' UpdateSMBFileShare (Maybe Text)
usmbfsDefaultStorageClass = lens _usmbfsDefaultStorageClass (\s a -> s {_usmbfsDefaultStorageClass = a})

-- | The name of the file share. Optional.
usmbfsFileShareName :: Lens' UpdateSMBFileShare (Maybe Text)
usmbfsFileShareName = lens _usmbfsFileShareName (\s a -> s {_usmbfsFileShareName = a})

-- | Set this value to @true@ to enable access control list (ACL) on the SMB file share. Set it to @false@ to map file and directory permissions to the POSIX permissions. For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share> in the /AWS Storage Gateway User Guide/ . Valid Values: @true@ | @false@
usmbfsSMBACLEnabled :: Lens' UpdateSMBFileShare (Maybe Bool)
usmbfsSMBACLEnabled = lens _usmbfsSMBACLEnabled (\s a -> s {_usmbfsSMBACLEnabled = a})

-- | The notification policy of the file share.
usmbfsNotificationPolicy :: Lens' UpdateSMBFileShare (Maybe Text)
usmbfsNotificationPolicy = lens _usmbfsNotificationPolicy (\s a -> s {_usmbfsNotificationPolicy = a})

-- | A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to @true@ , the requester pays the costs; otherwise, the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data. Valid Values: @true@ | @false@
usmbfsRequesterPays :: Lens' UpdateSMBFileShare (Maybe Bool)
usmbfsRequesterPays = lens _usmbfsRequesterPays (\s a -> s {_usmbfsRequesterPays = a})

-- | A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to @true@ to enable MIME type guessing, otherwise set to @false@ . The default value is @true@ . Valid Values: @true@ | @false@
usmbfsGuessMIMETypeEnabled :: Lens' UpdateSMBFileShare (Maybe Bool)
usmbfsGuessMIMETypeEnabled = lens _usmbfsGuessMIMETypeEnabled (\s a -> s {_usmbfsGuessMIMETypeEnabled = a})

-- | A value that sets the write status of a file share. Set this value to @true@ to set write status to read-only, otherwise set to @false@ . Valid Values: @true@ | @false@
usmbfsReadOnly :: Lens' UpdateSMBFileShare (Maybe Bool)
usmbfsReadOnly = lens _usmbfsReadOnly (\s a -> s {_usmbfsReadOnly = a})

-- | The case of an object name in an Amazon S3 bucket. For @ClientSpecified@ , the client determines the case sensitivity. For @CaseSensitive@ , the gateway determines the case sensitivity. The default value is @ClientSpecified@ .
usmbfsCaseSensitivity :: Lens' UpdateSMBFileShare (Maybe CaseSensitivity)
usmbfsCaseSensitivity = lens _usmbfsCaseSensitivity (\s a -> s {_usmbfsCaseSensitivity = a})

-- | The Amazon Resource Name (ARN) of the SMB file share that you want to update.
usmbfsFileShareARN :: Lens' UpdateSMBFileShare Text
usmbfsFileShareARN = lens _usmbfsFileShareARN (\s a -> s {_usmbfsFileShareARN = a})

instance AWSRequest UpdateSMBFileShare where
  type Rs UpdateSMBFileShare = UpdateSMBFileShareResponse
  request = postJSON storageGateway
  response =
    receiveJSON
      ( \s h x ->
          UpdateSMBFileShareResponse'
            <$> (x .?> "FileShareARN") <*> (pure (fromEnum s))
      )

instance Hashable UpdateSMBFileShare

instance NFData UpdateSMBFileShare

instance ToHeaders UpdateSMBFileShare where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("StorageGateway_20130630.UpdateSMBFileShare" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateSMBFileShare where
  toJSON UpdateSMBFileShare' {..} =
    object
      ( catMaybes
          [ ("AccessBasedEnumeration" .=) <$> _usmbfsAccessBasedEnumeration,
            ("AdminUserList" .=) <$> _usmbfsAdminUserList,
            ("AuditDestinationARN" .=) <$> _usmbfsAuditDestinationARN,
            ("InvalidUserList" .=) <$> _usmbfsInvalidUserList,
            ("KMSKey" .=) <$> _usmbfsKMSKey,
            ("ValidUserList" .=) <$> _usmbfsValidUserList,
            ("CacheAttributes" .=) <$> _usmbfsCacheAttributes,
            ("ObjectACL" .=) <$> _usmbfsObjectACL,
            ("KMSEncrypted" .=) <$> _usmbfsKMSEncrypted,
            ("DefaultStorageClass" .=) <$> _usmbfsDefaultStorageClass,
            ("FileShareName" .=) <$> _usmbfsFileShareName,
            ("SMBACLEnabled" .=) <$> _usmbfsSMBACLEnabled,
            ("NotificationPolicy" .=) <$> _usmbfsNotificationPolicy,
            ("RequesterPays" .=) <$> _usmbfsRequesterPays,
            ("GuessMIMETypeEnabled" .=) <$> _usmbfsGuessMIMETypeEnabled,
            ("ReadOnly" .=) <$> _usmbfsReadOnly,
            ("CaseSensitivity" .=) <$> _usmbfsCaseSensitivity,
            Just ("FileShareARN" .= _usmbfsFileShareARN)
          ]
      )

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
  { _usmbfsrsFileShareARN ::
      !(Maybe Text),
    _usmbfsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateSMBFileShareResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usmbfsrsFileShareARN' - The Amazon Resource Name (ARN) of the updated SMB file share.
--
-- * 'usmbfsrsResponseStatus' - -- | The response status code.
updateSMBFileShareResponse ::
  -- | 'usmbfsrsResponseStatus'
  Int ->
  UpdateSMBFileShareResponse
updateSMBFileShareResponse pResponseStatus_ =
  UpdateSMBFileShareResponse'
    { _usmbfsrsFileShareARN = Nothing,
      _usmbfsrsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the updated SMB file share.
usmbfsrsFileShareARN :: Lens' UpdateSMBFileShareResponse (Maybe Text)
usmbfsrsFileShareARN = lens _usmbfsrsFileShareARN (\s a -> s {_usmbfsrsFileShareARN = a})

-- | -- | The response status code.
usmbfsrsResponseStatus :: Lens' UpdateSMBFileShareResponse Int
usmbfsrsResponseStatus = lens _usmbfsrsResponseStatus (\s a -> s {_usmbfsrsResponseStatus = a})

instance NFData UpdateSMBFileShareResponse
