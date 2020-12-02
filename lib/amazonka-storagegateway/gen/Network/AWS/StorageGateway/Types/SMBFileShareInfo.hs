{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.SMBFileShareInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.SMBFileShareInfo where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.StorageGateway.Types.CacheAttributes
import Network.AWS.StorageGateway.Types.CaseSensitivity
import Network.AWS.StorageGateway.Types.ObjectACL
import Network.AWS.StorageGateway.Types.Tag

-- | The Windows file permissions and ownership information assigned, by default, to native S3 objects when file gateway discovers them in S3 buckets. This operation is only supported for file gateways.
--
--
--
-- /See:/ 'sMBFileShareInfo' smart constructor.
data SMBFileShareInfo = SMBFileShareInfo'
  { _smbfsiAccessBasedEnumeration ::
      !(Maybe Bool),
    _smbfsiAdminUserList :: !(Maybe [Text]),
    _smbfsiAuditDestinationARN :: !(Maybe Text),
    _smbfsiFileShareStatus :: !(Maybe Text),
    _smbfsiInvalidUserList :: !(Maybe [Text]),
    _smbfsiKMSKey :: !(Maybe Text),
    _smbfsiValidUserList :: !(Maybe [Text]),
    _smbfsiGatewayARN :: !(Maybe Text),
    _smbfsiPath :: !(Maybe Text),
    _smbfsiAuthentication :: !(Maybe Text),
    _smbfsiCacheAttributes :: !(Maybe CacheAttributes),
    _smbfsiObjectACL :: !(Maybe ObjectACL),
    _smbfsiKMSEncrypted :: !(Maybe Bool),
    _smbfsiFileShareId :: !(Maybe Text),
    _smbfsiFileShareARN :: !(Maybe Text),
    _smbfsiDefaultStorageClass :: !(Maybe Text),
    _smbfsiFileShareName :: !(Maybe Text),
    _smbfsiRole :: !(Maybe Text),
    _smbfsiSMBACLEnabled :: !(Maybe Bool),
    _smbfsiNotificationPolicy :: !(Maybe Text),
    _smbfsiRequesterPays :: !(Maybe Bool),
    _smbfsiLocationARN :: !(Maybe Text),
    _smbfsiGuessMIMETypeEnabled :: !(Maybe Bool),
    _smbfsiReadOnly :: !(Maybe Bool),
    _smbfsiCaseSensitivity :: !(Maybe CaseSensitivity),
    _smbfsiTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SMBFileShareInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smbfsiAccessBasedEnumeration' - Indicates whether @AccessBasedEnumeration@ is enabled.
--
-- * 'smbfsiAdminUserList' - A list of users or groups in the Active Directory that have administrator rights to the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
--
-- * 'smbfsiAuditDestinationARN' - The Amazon Resource Name (ARN) of the storage used for the audit logs.
--
-- * 'smbfsiFileShareStatus' - Undocumented member.
--
-- * 'smbfsiInvalidUserList' - A list of users or groups in the Active Directory that are not allowed to access the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
--
-- * 'smbfsiKMSKey' - Undocumented member.
--
-- * 'smbfsiValidUserList' - A list of users or groups in the Active Directory that are allowed to access the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
--
-- * 'smbfsiGatewayARN' - Undocumented member.
--
-- * 'smbfsiPath' - The file share path used by the SMB client to identify the mount point.
--
-- * 'smbfsiAuthentication' - Undocumented member.
--
-- * 'smbfsiCacheAttributes' - Refresh cache information.
--
-- * 'smbfsiObjectACL' - Undocumented member.
--
-- * 'smbfsiKMSEncrypted' - Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional. Valid Values: @true@ | @false@
--
-- * 'smbfsiFileShareId' - Undocumented member.
--
-- * 'smbfsiFileShareARN' - Undocumented member.
--
-- * 'smbfsiDefaultStorageClass' - The default storage class for objects put into an Amazon S3 bucket by the file gateway. The default value is @S3_INTELLIGENT_TIERING@ . Optional. Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ | @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
--
-- * 'smbfsiFileShareName' - The name of the file share. Optional.
--
-- * 'smbfsiRole' - Undocumented member.
--
-- * 'smbfsiSMBACLEnabled' - If this value is set to @true@ , it indicates that access control list (ACL) is enabled on the SMB file share. If it is set to @false@ , it indicates that file and directory permissions are mapped to the POSIX permission. For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share> in the /AWS Storage Gateway User Guide/ .
--
-- * 'smbfsiNotificationPolicy' - The notification policy of the file share.
--
-- * 'smbfsiRequesterPays' - A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to @true@ , the requester pays the costs; otherwise, the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data. Valid Values: @true@ | @false@
--
-- * 'smbfsiLocationARN' - Undocumented member.
--
-- * 'smbfsiGuessMIMETypeEnabled' - A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to @true@ to enable MIME type guessing, otherwise set to @false@ . The default value is @true@ . Valid Values: @true@ | @false@
--
-- * 'smbfsiReadOnly' - A value that sets the write status of a file share. Set this value to @true@ to set the write status to read-only, otherwise set to @false@ . Valid Values: @true@ | @false@
--
-- * 'smbfsiCaseSensitivity' - The case of an object name in an Amazon S3 bucket. For @ClientSpecified@ , the client determines the case sensitivity. For @CaseSensitive@ , the gateway determines the case sensitivity. The default value is @ClientSpecified@ .
--
-- * 'smbfsiTags' - A list of up to 50 tags assigned to the SMB file share, sorted alphabetically by key name. Each tag is a key-value pair. For a gateway with more than 10 tags assigned, you can view all tags using the @ListTagsForResource@ API operation.
sMBFileShareInfo ::
  SMBFileShareInfo
sMBFileShareInfo =
  SMBFileShareInfo'
    { _smbfsiAccessBasedEnumeration = Nothing,
      _smbfsiAdminUserList = Nothing,
      _smbfsiAuditDestinationARN = Nothing,
      _smbfsiFileShareStatus = Nothing,
      _smbfsiInvalidUserList = Nothing,
      _smbfsiKMSKey = Nothing,
      _smbfsiValidUserList = Nothing,
      _smbfsiGatewayARN = Nothing,
      _smbfsiPath = Nothing,
      _smbfsiAuthentication = Nothing,
      _smbfsiCacheAttributes = Nothing,
      _smbfsiObjectACL = Nothing,
      _smbfsiKMSEncrypted = Nothing,
      _smbfsiFileShareId = Nothing,
      _smbfsiFileShareARN = Nothing,
      _smbfsiDefaultStorageClass = Nothing,
      _smbfsiFileShareName = Nothing,
      _smbfsiRole = Nothing,
      _smbfsiSMBACLEnabled = Nothing,
      _smbfsiNotificationPolicy = Nothing,
      _smbfsiRequesterPays = Nothing,
      _smbfsiLocationARN = Nothing,
      _smbfsiGuessMIMETypeEnabled = Nothing,
      _smbfsiReadOnly = Nothing,
      _smbfsiCaseSensitivity = Nothing,
      _smbfsiTags = Nothing
    }

-- | Indicates whether @AccessBasedEnumeration@ is enabled.
smbfsiAccessBasedEnumeration :: Lens' SMBFileShareInfo (Maybe Bool)
smbfsiAccessBasedEnumeration = lens _smbfsiAccessBasedEnumeration (\s a -> s {_smbfsiAccessBasedEnumeration = a})

-- | A list of users or groups in the Active Directory that have administrator rights to the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
smbfsiAdminUserList :: Lens' SMBFileShareInfo [Text]
smbfsiAdminUserList = lens _smbfsiAdminUserList (\s a -> s {_smbfsiAdminUserList = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the storage used for the audit logs.
smbfsiAuditDestinationARN :: Lens' SMBFileShareInfo (Maybe Text)
smbfsiAuditDestinationARN = lens _smbfsiAuditDestinationARN (\s a -> s {_smbfsiAuditDestinationARN = a})

-- | Undocumented member.
smbfsiFileShareStatus :: Lens' SMBFileShareInfo (Maybe Text)
smbfsiFileShareStatus = lens _smbfsiFileShareStatus (\s a -> s {_smbfsiFileShareStatus = a})

-- | A list of users or groups in the Active Directory that are not allowed to access the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
smbfsiInvalidUserList :: Lens' SMBFileShareInfo [Text]
smbfsiInvalidUserList = lens _smbfsiInvalidUserList (\s a -> s {_smbfsiInvalidUserList = a}) . _Default . _Coerce

-- | Undocumented member.
smbfsiKMSKey :: Lens' SMBFileShareInfo (Maybe Text)
smbfsiKMSKey = lens _smbfsiKMSKey (\s a -> s {_smbfsiKMSKey = a})

-- | A list of users or groups in the Active Directory that are allowed to access the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
smbfsiValidUserList :: Lens' SMBFileShareInfo [Text]
smbfsiValidUserList = lens _smbfsiValidUserList (\s a -> s {_smbfsiValidUserList = a}) . _Default . _Coerce

-- | Undocumented member.
smbfsiGatewayARN :: Lens' SMBFileShareInfo (Maybe Text)
smbfsiGatewayARN = lens _smbfsiGatewayARN (\s a -> s {_smbfsiGatewayARN = a})

-- | The file share path used by the SMB client to identify the mount point.
smbfsiPath :: Lens' SMBFileShareInfo (Maybe Text)
smbfsiPath = lens _smbfsiPath (\s a -> s {_smbfsiPath = a})

-- | Undocumented member.
smbfsiAuthentication :: Lens' SMBFileShareInfo (Maybe Text)
smbfsiAuthentication = lens _smbfsiAuthentication (\s a -> s {_smbfsiAuthentication = a})

-- | Refresh cache information.
smbfsiCacheAttributes :: Lens' SMBFileShareInfo (Maybe CacheAttributes)
smbfsiCacheAttributes = lens _smbfsiCacheAttributes (\s a -> s {_smbfsiCacheAttributes = a})

-- | Undocumented member.
smbfsiObjectACL :: Lens' SMBFileShareInfo (Maybe ObjectACL)
smbfsiObjectACL = lens _smbfsiObjectACL (\s a -> s {_smbfsiObjectACL = a})

-- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional. Valid Values: @true@ | @false@
smbfsiKMSEncrypted :: Lens' SMBFileShareInfo (Maybe Bool)
smbfsiKMSEncrypted = lens _smbfsiKMSEncrypted (\s a -> s {_smbfsiKMSEncrypted = a})

-- | Undocumented member.
smbfsiFileShareId :: Lens' SMBFileShareInfo (Maybe Text)
smbfsiFileShareId = lens _smbfsiFileShareId (\s a -> s {_smbfsiFileShareId = a})

-- | Undocumented member.
smbfsiFileShareARN :: Lens' SMBFileShareInfo (Maybe Text)
smbfsiFileShareARN = lens _smbfsiFileShareARN (\s a -> s {_smbfsiFileShareARN = a})

-- | The default storage class for objects put into an Amazon S3 bucket by the file gateway. The default value is @S3_INTELLIGENT_TIERING@ . Optional. Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ | @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
smbfsiDefaultStorageClass :: Lens' SMBFileShareInfo (Maybe Text)
smbfsiDefaultStorageClass = lens _smbfsiDefaultStorageClass (\s a -> s {_smbfsiDefaultStorageClass = a})

-- | The name of the file share. Optional.
smbfsiFileShareName :: Lens' SMBFileShareInfo (Maybe Text)
smbfsiFileShareName = lens _smbfsiFileShareName (\s a -> s {_smbfsiFileShareName = a})

-- | Undocumented member.
smbfsiRole :: Lens' SMBFileShareInfo (Maybe Text)
smbfsiRole = lens _smbfsiRole (\s a -> s {_smbfsiRole = a})

-- | If this value is set to @true@ , it indicates that access control list (ACL) is enabled on the SMB file share. If it is set to @false@ , it indicates that file and directory permissions are mapped to the POSIX permission. For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share> in the /AWS Storage Gateway User Guide/ .
smbfsiSMBACLEnabled :: Lens' SMBFileShareInfo (Maybe Bool)
smbfsiSMBACLEnabled = lens _smbfsiSMBACLEnabled (\s a -> s {_smbfsiSMBACLEnabled = a})

-- | The notification policy of the file share.
smbfsiNotificationPolicy :: Lens' SMBFileShareInfo (Maybe Text)
smbfsiNotificationPolicy = lens _smbfsiNotificationPolicy (\s a -> s {_smbfsiNotificationPolicy = a})

-- | A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to @true@ , the requester pays the costs; otherwise, the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data. Valid Values: @true@ | @false@
smbfsiRequesterPays :: Lens' SMBFileShareInfo (Maybe Bool)
smbfsiRequesterPays = lens _smbfsiRequesterPays (\s a -> s {_smbfsiRequesterPays = a})

-- | Undocumented member.
smbfsiLocationARN :: Lens' SMBFileShareInfo (Maybe Text)
smbfsiLocationARN = lens _smbfsiLocationARN (\s a -> s {_smbfsiLocationARN = a})

-- | A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to @true@ to enable MIME type guessing, otherwise set to @false@ . The default value is @true@ . Valid Values: @true@ | @false@
smbfsiGuessMIMETypeEnabled :: Lens' SMBFileShareInfo (Maybe Bool)
smbfsiGuessMIMETypeEnabled = lens _smbfsiGuessMIMETypeEnabled (\s a -> s {_smbfsiGuessMIMETypeEnabled = a})

-- | A value that sets the write status of a file share. Set this value to @true@ to set the write status to read-only, otherwise set to @false@ . Valid Values: @true@ | @false@
smbfsiReadOnly :: Lens' SMBFileShareInfo (Maybe Bool)
smbfsiReadOnly = lens _smbfsiReadOnly (\s a -> s {_smbfsiReadOnly = a})

-- | The case of an object name in an Amazon S3 bucket. For @ClientSpecified@ , the client determines the case sensitivity. For @CaseSensitive@ , the gateway determines the case sensitivity. The default value is @ClientSpecified@ .
smbfsiCaseSensitivity :: Lens' SMBFileShareInfo (Maybe CaseSensitivity)
smbfsiCaseSensitivity = lens _smbfsiCaseSensitivity (\s a -> s {_smbfsiCaseSensitivity = a})

-- | A list of up to 50 tags assigned to the SMB file share, sorted alphabetically by key name. Each tag is a key-value pair. For a gateway with more than 10 tags assigned, you can view all tags using the @ListTagsForResource@ API operation.
smbfsiTags :: Lens' SMBFileShareInfo [Tag]
smbfsiTags = lens _smbfsiTags (\s a -> s {_smbfsiTags = a}) . _Default . _Coerce

instance FromJSON SMBFileShareInfo where
  parseJSON =
    withObject
      "SMBFileShareInfo"
      ( \x ->
          SMBFileShareInfo'
            <$> (x .:? "AccessBasedEnumeration")
            <*> (x .:? "AdminUserList" .!= mempty)
            <*> (x .:? "AuditDestinationARN")
            <*> (x .:? "FileShareStatus")
            <*> (x .:? "InvalidUserList" .!= mempty)
            <*> (x .:? "KMSKey")
            <*> (x .:? "ValidUserList" .!= mempty)
            <*> (x .:? "GatewayARN")
            <*> (x .:? "Path")
            <*> (x .:? "Authentication")
            <*> (x .:? "CacheAttributes")
            <*> (x .:? "ObjectACL")
            <*> (x .:? "KMSEncrypted")
            <*> (x .:? "FileShareId")
            <*> (x .:? "FileShareARN")
            <*> (x .:? "DefaultStorageClass")
            <*> (x .:? "FileShareName")
            <*> (x .:? "Role")
            <*> (x .:? "SMBACLEnabled")
            <*> (x .:? "NotificationPolicy")
            <*> (x .:? "RequesterPays")
            <*> (x .:? "LocationARN")
            <*> (x .:? "GuessMIMETypeEnabled")
            <*> (x .:? "ReadOnly")
            <*> (x .:? "CaseSensitivity")
            <*> (x .:? "Tags" .!= mempty)
      )

instance Hashable SMBFileShareInfo

instance NFData SMBFileShareInfo
