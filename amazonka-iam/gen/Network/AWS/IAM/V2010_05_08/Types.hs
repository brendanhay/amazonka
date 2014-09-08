{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | AWS Identity and Access Management (IAM) enables you to securely control
-- access to AWS services and resources for your users. Using IAM, you can
-- create and manage AWS users and groups and use permissions to allow and
-- deny their access to AWS resources.
module Network.AWS.IAM.V2010_05_08.Types
    (
    -- * Service
      IAM
    -- ** XML
    , xmlOptions

    -- * AssignmentStatusType
    , AssignmentStatusType (..)

    -- * ReportFormatType
    , ReportFormatType (..)

    -- * ReportStateType
    , ReportStateType (..)

    -- * StatusType
    , StatusType (..)

    -- * SummaryKeyType
    , SummaryKeyType (..)

    -- * AccessKey
    , AccessKey
    , mkAccessKey
    , akUserName
    , akAccessKeyId
    , akStatus
    , akSecretAccessKey
    , akCreateDate

    -- * AccessKeyMetadata
    , AccessKeyMetadata
    , mkAccessKeyMetadata
    , akmUserName
    , akmAccessKeyId
    , akmStatus
    , akmCreateDate

    -- * Group
    , Group
    , mkGroup
    , gPath
    , gGroupName
    , gGroupId
    , gArn
    , gCreateDate

    -- * InstanceProfile
    , InstanceProfile
    , mkInstanceProfile
    , ipPath
    , ipInstanceProfileName
    , ipInstanceProfileId
    , ipArn
    , ipCreateDate
    , ipRoles

    -- * LoginProfile
    , LoginProfile
    , mkLoginProfile
    , lpUserName
    , lpCreateDate
    , lpPasswordResetRequired

    -- * MFADevice
    , MFADevice
    , mkMFADevice
    , mfadUserName
    , mfadSerialNumber
    , mfadEnableDate

    -- * PasswordPolicy
    , PasswordPolicy
    , mkPasswordPolicy
    , ppMinimumPasswordLength
    , ppRequireSymbols
    , ppRequireNumbers
    , ppRequireUppercaseCharacters
    , ppRequireLowercaseCharacters
    , ppAllowUsersToChangePassword
    , ppExpirePasswords
    , ppMaxPasswordAge
    , ppPasswordReusePrevention
    , ppHardExpiry

    -- * Role
    , Role
    , mkRole
    , rPath
    , rRoleName
    , rRoleId
    , rArn
    , rCreateDate
    , rAssumeRolePolicyDocument

    -- * SAMLProviderListEntry
    , SAMLProviderListEntry
    , mkSAMLProviderListEntry
    , samlpleArn
    , samlpleValidUntil
    , samlpleCreateDate

    -- * ServerCertificate
    , ServerCertificate
    , mkServerCertificate
    , scServerCertificateMetadata
    , scCertificateBody
    , scCertificateChain

    -- * ServerCertificateMetadata
    , ServerCertificateMetadata
    , mkServerCertificateMetadata
    , scmPath
    , scmServerCertificateName
    , scmServerCertificateId
    , scmArn
    , scmUploadDate
    , scmExpiration

    -- * SigningCertificate
    , SigningCertificate
    , mkSigningCertificate
    , scrUserName
    , scrCertificateId
    , scrCertificateBody
    , scrStatus
    , scrUploadDate

    -- * User
    , User
    , mkUser
    , uPath
    , uUserName
    , uUserId
    , uArn
    , uCreateDate

    -- * VirtualMFADevice
    , VirtualMFADevice
    , mkVirtualMFADevice
    , vmfadSerialNumber
    , vmfadBase32StringSeed
    , vmfadQRCodePNG
    , vmfadUser
    , vmfadEnableDate
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2010-05-08@) of the
-- @AWS Identity and Access Management@ service.
data IAM deriving (Typeable)

instance AWSService IAM where
    type Sg IAM = V4
    data Er IAM
        = CredentialReportExpiredException
            { _creeMessage :: Maybe Text
            }
        | CredentialReportNotPresentException
            { _crnpeMessage :: Maybe Text
            }
        | CredentialReportNotReadyException
            { _crnreMessage :: Maybe Text
            }
        | DeleteConflictException
            { _dceMessage :: Maybe Text
            }
        | DuplicateCertificateException
            { _dceMessage :: Maybe Text
            }
        | EntityAlreadyExistsException
            { _eaeeMessage :: Maybe Text
            }
        | EntityTemporarilyUnmodifiableException
            { _etueMessage :: Maybe Text
            }
        | IAMClient HttpException
        | IAMSerializer String
        | IAMService String
        | InvalidAuthenticationCodeException
            { _iaceMessage :: Maybe Text
            }
        | InvalidCertificateException
            { _iceMessage :: Maybe Text
            }
        | InvalidInputException
            { _iieMessage :: Maybe Text
            }
        | InvalidUserTypeException
            { _iuteMessage :: Maybe Text
            }
        | KeyPairMismatchException
            { _kpmeMessage :: Maybe Text
            }
        | LimitExceededException
            { _leeMessage :: Maybe Text
            }
        | MalformedCertificateException
            { _mceMessage :: Maybe Text
            }
        | MalformedPolicyDocumentException
            { _mpdeMessage :: Maybe Text
            }
        | NoSuchEntityException
            { _nseeMessage :: Maybe Text
            }
        | PasswordPolicyViolationException
            { _ppveMessage :: Maybe Text
            }

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "iam"
        , _svcVersion  = "2010-05-08"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er IAM)
deriving instance Generic (Er IAM)

instance AWSError (Er IAM) where
    awsError = const "IAMError"

instance AWSServiceError (Er IAM) where
    serviceError    = IAMService
    clientError     = IAMClient
    serializerError = IAMSerializer

instance Exception (Er IAM)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlNamespace = Just "https://iam.amazonaws.com/doc/2010-05-08/"
    }

data AssignmentStatusType
    = AssignmentStatusTypeAny -- ^ Any
    | AssignmentStatusTypeAssigned -- ^ Assigned
    | AssignmentStatusTypeUnassigned -- ^ Unassigned
      deriving (Eq, Show, Generic)

instance Hashable AssignmentStatusType

instance FromText AssignmentStatusType where
    parser = match "Any" AssignmentStatusTypeAny
         <|> match "Assigned" AssignmentStatusTypeAssigned
         <|> match "Unassigned" AssignmentStatusTypeUnassigned

instance ToText AssignmentStatusType where
    toText AssignmentStatusTypeAny = "Any"
    toText AssignmentStatusTypeAssigned = "Assigned"
    toText AssignmentStatusTypeUnassigned = "Unassigned"

instance ToByteString AssignmentStatusType

instance ToQuery AssignmentStatusType where
    toQuery = genericQuery def

data ReportFormatType
    = ReportFormatTypeTextCsv -- ^ text/csv
      deriving (Eq, Show, Generic)

instance Hashable ReportFormatType

instance FromText ReportFormatType where
    parser = match "text/csv" ReportFormatTypeTextCsv

instance ToText ReportFormatType where
    toText ReportFormatTypeTextCsv = "text/csv"

instance ToByteString ReportFormatType

instance FromXML ReportFormatType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReportFormatType"

data ReportStateType
    = ReportStateTypeComplete -- ^ COMPLETE
    | ReportStateTypeInprogress -- ^ INPROGRESS
    | ReportStateTypeStarted -- ^ STARTED
      deriving (Eq, Show, Generic)

instance Hashable ReportStateType

instance FromText ReportStateType where
    parser = match "COMPLETE" ReportStateTypeComplete
         <|> match "INPROGRESS" ReportStateTypeInprogress
         <|> match "STARTED" ReportStateTypeStarted

instance ToText ReportStateType where
    toText ReportStateTypeComplete = "COMPLETE"
    toText ReportStateTypeInprogress = "INPROGRESS"
    toText ReportStateTypeStarted = "STARTED"

instance ToByteString ReportStateType

instance FromXML ReportStateType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReportStateType"

data StatusType
    = StatusTypeActive -- ^ Active
    | StatusTypeInactive -- ^ Inactive
      deriving (Eq, Show, Generic)

instance Hashable StatusType

instance FromText StatusType where
    parser = match "Active" StatusTypeActive
         <|> match "Inactive" StatusTypeInactive

instance ToText StatusType where
    toText StatusTypeActive = "Active"
    toText StatusTypeInactive = "Inactive"

instance ToByteString StatusType

instance FromXML StatusType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "StatusType"

instance ToQuery StatusType where
    toQuery = genericQuery def

data SummaryKeyType
    = SummaryKeyTypeAccessKeysPerUserQuota -- ^ AccessKeysPerUserQuota
    | SummaryKeyTypeAccountMFAEnabled -- ^ AccountMFAEnabled
    | SummaryKeyTypeGroupPolicySizeQuota -- ^ GroupPolicySizeQuota
    | SummaryKeyTypeGroups -- ^ Groups
    | SummaryKeyTypeGroupsPerUserQuota -- ^ GroupsPerUserQuota
    | SummaryKeyTypeGroupsQuota -- ^ GroupsQuota
    | SummaryKeyTypeMFADevices -- ^ MFADevices
    | SummaryKeyTypeMFADevicesInUse -- ^ MFADevicesInUse
    | SummaryKeyTypeServerCertificates -- ^ ServerCertificates
    | SummaryKeyTypeServerCertificatesQuota -- ^ ServerCertificatesQuota
    | SummaryKeyTypeSigningCertificatesPerUserQuota -- ^ SigningCertificatesPerUserQuota
    | SummaryKeyTypeUserPolicySizeQuota -- ^ UserPolicySizeQuota
    | SummaryKeyTypeUsers -- ^ Users
    | SummaryKeyTypeUsersQuota -- ^ UsersQuota
      deriving (Eq, Show, Generic)

instance Hashable SummaryKeyType

instance FromText SummaryKeyType where
    parser = match "AccessKeysPerUserQuota" SummaryKeyTypeAccessKeysPerUserQuota
         <|> match "AccountMFAEnabled" SummaryKeyTypeAccountMFAEnabled
         <|> match "GroupPolicySizeQuota" SummaryKeyTypeGroupPolicySizeQuota
         <|> match "Groups" SummaryKeyTypeGroups
         <|> match "GroupsPerUserQuota" SummaryKeyTypeGroupsPerUserQuota
         <|> match "GroupsQuota" SummaryKeyTypeGroupsQuota
         <|> match "MFADevices" SummaryKeyTypeMFADevices
         <|> match "MFADevicesInUse" SummaryKeyTypeMFADevicesInUse
         <|> match "ServerCertificates" SummaryKeyTypeServerCertificates
         <|> match "ServerCertificatesQuota" SummaryKeyTypeServerCertificatesQuota
         <|> match "SigningCertificatesPerUserQuota" SummaryKeyTypeSigningCertificatesPerUserQuota
         <|> match "UserPolicySizeQuota" SummaryKeyTypeUserPolicySizeQuota
         <|> match "Users" SummaryKeyTypeUsers
         <|> match "UsersQuota" SummaryKeyTypeUsersQuota

instance ToText SummaryKeyType where
    toText SummaryKeyTypeAccessKeysPerUserQuota = "AccessKeysPerUserQuota"
    toText SummaryKeyTypeAccountMFAEnabled = "AccountMFAEnabled"
    toText SummaryKeyTypeGroupPolicySizeQuota = "GroupPolicySizeQuota"
    toText SummaryKeyTypeGroups = "Groups"
    toText SummaryKeyTypeGroupsPerUserQuota = "GroupsPerUserQuota"
    toText SummaryKeyTypeGroupsQuota = "GroupsQuota"
    toText SummaryKeyTypeMFADevices = "MFADevices"
    toText SummaryKeyTypeMFADevicesInUse = "MFADevicesInUse"
    toText SummaryKeyTypeServerCertificates = "ServerCertificates"
    toText SummaryKeyTypeServerCertificatesQuota = "ServerCertificatesQuota"
    toText SummaryKeyTypeSigningCertificatesPerUserQuota = "SigningCertificatesPerUserQuota"
    toText SummaryKeyTypeUserPolicySizeQuota = "UserPolicySizeQuota"
    toText SummaryKeyTypeUsers = "Users"
    toText SummaryKeyTypeUsersQuota = "UsersQuota"

instance ToByteString SummaryKeyType

instance FromXML SummaryKeyType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SummaryKeyType"

-- | Information about the access key.
data AccessKey = AccessKey
    { _akUserName :: Text
    , _akAccessKeyId :: Text
    , _akStatus :: StatusType
    , _akSecretAccessKey :: Text
    , _akCreateDate :: Maybe ISO8601
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AccessKey' data type.
--
-- 'AccessKey' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
mkAccessKey :: Text -- ^ 'akUserName'
            -> Text -- ^ 'akAccessKeyId'
            -> StatusType -- ^ 'akStatus'
            -> Text -- ^ 'akSecretAccessKey'
            -> AccessKey
mkAccessKey p1 p2 p3 p4 = AccessKey
    { _akUserName = p1
    , _akAccessKeyId = p2
    , _akStatus = p3
    , _akSecretAccessKey = p4
    , _akCreateDate = Nothing
    }

-- | Name of the user the key is associated with.
akUserName :: Lens' AccessKey Text
akUserName = lens _akUserName (\s a -> s { _akUserName = a })

-- | The ID for this access key.
akAccessKeyId :: Lens' AccessKey Text
akAccessKeyId = lens _akAccessKeyId (\s a -> s { _akAccessKeyId = a })

-- | The status of the access key. Active means the key is valid for API calls,
-- while Inactive means it is not.
akStatus :: Lens' AccessKey StatusType
akStatus = lens _akStatus (\s a -> s { _akStatus = a })

-- | The secret key used to sign requests.
akSecretAccessKey :: Lens' AccessKey Text
akSecretAccessKey =
    lens _akSecretAccessKey (\s a -> s { _akSecretAccessKey = a })

-- | The date when the access key was created.
akCreateDate :: Lens' AccessKey (Maybe ISO8601)
akCreateDate = lens _akCreateDate (\s a -> s { _akCreateDate = a })

instance FromXML AccessKey where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AccessKey"

-- | The AccessKey data type contains information about an AWS access key,
-- without its secret key. This data type is used as a response element in the
-- action ListAccessKeys.
data AccessKeyMetadata = AccessKeyMetadata
    { _akmUserName :: Maybe Text
    , _akmAccessKeyId :: Maybe Text
    , _akmStatus :: Maybe StatusType
    , _akmCreateDate :: Maybe ISO8601
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AccessKeyMetadata' data type.
--
-- 'AccessKeyMetadata' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
mkAccessKeyMetadata :: AccessKeyMetadata
mkAccessKeyMetadata = AccessKeyMetadata
    { _akmUserName = Nothing
    , _akmAccessKeyId = Nothing
    , _akmStatus = Nothing
    , _akmCreateDate = Nothing
    }

-- | Name of the user the key is associated with.
akmUserName :: Lens' AccessKeyMetadata (Maybe Text)
akmUserName = lens _akmUserName (\s a -> s { _akmUserName = a })

-- | The ID for this access key.
akmAccessKeyId :: Lens' AccessKeyMetadata (Maybe Text)
akmAccessKeyId = lens _akmAccessKeyId (\s a -> s { _akmAccessKeyId = a })

-- | The status of the access key. Active means the key is valid for API calls,
-- while Inactive means it is not.
akmStatus :: Lens' AccessKeyMetadata (Maybe StatusType)
akmStatus = lens _akmStatus (\s a -> s { _akmStatus = a })

-- | The date when the access key was created.
akmCreateDate :: Lens' AccessKeyMetadata (Maybe ISO8601)
akmCreateDate = lens _akmCreateDate (\s a -> s { _akmCreateDate = a })

instance FromXML AccessKeyMetadata where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AccessKeyMetadata"

-- | Information about the group.
data Group = Group
    { _gPath :: Text
    , _gGroupName :: Text
    , _gGroupId :: Text
    , _gArn :: Text
    , _gCreateDate :: ISO8601
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Group' data type.
--
-- 'Group' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
mkGroup :: Text -- ^ 'gPath'
        -> Text -- ^ 'gGroupName'
        -> Text -- ^ 'gGroupId'
        -> Text -- ^ 'gArn'
        -> ISO8601 -- ^ 'gCreateDate'
        -> Group
mkGroup p1 p2 p3 p4 p5 = Group
    { _gPath = p1
    , _gGroupName = p2
    , _gGroupId = p3
    , _gArn = p4
    , _gCreateDate = p5
    }

-- | Path to the group. For more information about paths, see Identifiers for
-- IAM Entities in the Using IAM guide.
gPath :: Lens' Group Text
gPath = lens _gPath (\s a -> s { _gPath = a })

-- | The name that identifies the group.
gGroupName :: Lens' Group Text
gGroupName = lens _gGroupName (\s a -> s { _gGroupName = a })

-- | The stable and unique string identifying the group. For more information
-- about IDs, see Identifiers for IAM Entities in the Using IAM guide.
gGroupId :: Lens' Group Text
gGroupId = lens _gGroupId (\s a -> s { _gGroupId = a })

-- | The Amazon Resource Name (ARN) specifying the group. For more information
-- about ARNs and how to use them in policies, see Identifiers for IAM
-- Entities in the Using IAM guide.
gArn :: Lens' Group Text
gArn = lens _gArn (\s a -> s { _gArn = a })

-- | The date when the group was created.
gCreateDate :: Lens' Group ISO8601
gCreateDate = lens _gCreateDate (\s a -> s { _gCreateDate = a })

instance FromXML Group where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Group"

-- | Information about the instance profile.
data InstanceProfile = InstanceProfile
    { _ipPath :: Text
    , _ipInstanceProfileName :: Text
    , _ipInstanceProfileId :: Text
    , _ipArn :: Text
    , _ipCreateDate :: ISO8601
    , _ipRoles :: [Role]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceProfile' data type.
--
-- 'InstanceProfile' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
mkInstanceProfile :: Text -- ^ 'ipPath'
                  -> Text -- ^ 'ipInstanceProfileName'
                  -> Text -- ^ 'ipInstanceProfileId'
                  -> Text -- ^ 'ipArn'
                  -> ISO8601 -- ^ 'ipCreateDate'
                  -> [Role] -- ^ 'ipRoles'
                  -> InstanceProfile
mkInstanceProfile p1 p2 p3 p4 p5 p6 = InstanceProfile
    { _ipPath = p1
    , _ipInstanceProfileName = p2
    , _ipInstanceProfileId = p3
    , _ipArn = p4
    , _ipCreateDate = p5
    , _ipRoles = p6
    }

-- | Path to the instance profile. For more information about paths, see
-- Identifiers for IAM Entities in the Using IAM guide.
ipPath :: Lens' InstanceProfile Text
ipPath = lens _ipPath (\s a -> s { _ipPath = a })

-- | The name identifying the instance profile.
ipInstanceProfileName :: Lens' InstanceProfile Text
ipInstanceProfileName =
    lens _ipInstanceProfileName (\s a -> s { _ipInstanceProfileName = a })

-- | The stable and unique string identifying the instance profile. For more
-- information about IDs, see Identifiers for IAM Entities in the Using IAM
-- guide.
ipInstanceProfileId :: Lens' InstanceProfile Text
ipInstanceProfileId =
    lens _ipInstanceProfileId (\s a -> s { _ipInstanceProfileId = a })

-- | The Amazon Resource Name (ARN) specifying the instance profile. For more
-- information about ARNs and how to use them in policies, see Identifiers for
-- IAM Entities in the Using IAM guide.
ipArn :: Lens' InstanceProfile Text
ipArn = lens _ipArn (\s a -> s { _ipArn = a })

-- | The date when the instance profile was created.
ipCreateDate :: Lens' InstanceProfile ISO8601
ipCreateDate = lens _ipCreateDate (\s a -> s { _ipCreateDate = a })

-- | The role associated with the instance profile.
ipRoles :: Lens' InstanceProfile [Role]
ipRoles = lens _ipRoles (\s a -> s { _ipRoles = a })

instance FromXML InstanceProfile where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "InstanceProfile"

-- | The user name and password create date.
data LoginProfile = LoginProfile
    { _lpUserName :: Text
    , _lpCreateDate :: ISO8601
    , _lpPasswordResetRequired :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'LoginProfile' data type.
--
-- 'LoginProfile' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
mkLoginProfile :: Text -- ^ 'lpUserName'
               -> ISO8601 -- ^ 'lpCreateDate'
               -> LoginProfile
mkLoginProfile p1 p2 = LoginProfile
    { _lpUserName = p1
    , _lpCreateDate = p2
    , _lpPasswordResetRequired = Nothing
    }

-- | The name of the user, which can be used for signing in to the AWS
-- Management Console.
lpUserName :: Lens' LoginProfile Text
lpUserName = lens _lpUserName (\s a -> s { _lpUserName = a })

-- | The date when the password for the user was created.
lpCreateDate :: Lens' LoginProfile ISO8601
lpCreateDate = lens _lpCreateDate (\s a -> s { _lpCreateDate = a })

-- | Specifies whether the user is required to set a new password on next
-- sign-in.
lpPasswordResetRequired :: Lens' LoginProfile (Maybe Bool)
lpPasswordResetRequired =
    lens _lpPasswordResetRequired
         (\s a -> s { _lpPasswordResetRequired = a })

instance FromXML LoginProfile where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LoginProfile"

-- | The MFADevice data type contains information about an MFA device. This data
-- type is used as a response element in the action ListMFADevices.
data MFADevice = MFADevice
    { _mfadUserName :: Text
    , _mfadSerialNumber :: Text
    , _mfadEnableDate :: ISO8601
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'MFADevice' data type.
--
-- 'MFADevice' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
mkMFADevice :: Text -- ^ 'mfadUserName'
            -> Text -- ^ 'mfadSerialNumber'
            -> ISO8601 -- ^ 'mfadEnableDate'
            -> MFADevice
mkMFADevice p1 p2 p3 = MFADevice
    { _mfadUserName = p1
    , _mfadSerialNumber = p2
    , _mfadEnableDate = p3
    }

-- | The user with whom the MFA device is associated.
mfadUserName :: Lens' MFADevice Text
mfadUserName = lens _mfadUserName (\s a -> s { _mfadUserName = a })

-- | The serial number that uniquely identifies the MFA device. For virtual MFA
-- devices, the serial number is the device ARN.
mfadSerialNumber :: Lens' MFADevice Text
mfadSerialNumber =
    lens _mfadSerialNumber (\s a -> s { _mfadSerialNumber = a })

-- | The date when the MFA device was enabled for the user.
mfadEnableDate :: Lens' MFADevice ISO8601
mfadEnableDate = lens _mfadEnableDate (\s a -> s { _mfadEnableDate = a })

instance FromXML MFADevice where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "MFADevice"

-- | The PasswordPolicy data type contains information about the account
-- password policy. This data type is used as a response element in the action
-- GetAccountPasswordPolicy.
data PasswordPolicy = PasswordPolicy
    { _ppMinimumPasswordLength :: Maybe Integer
    , _ppRequireSymbols :: Maybe Bool
    , _ppRequireNumbers :: Maybe Bool
    , _ppRequireUppercaseCharacters :: Maybe Bool
    , _ppRequireLowercaseCharacters :: Maybe Bool
    , _ppAllowUsersToChangePassword :: Maybe Bool
    , _ppExpirePasswords :: Maybe Bool
    , _ppMaxPasswordAge :: Maybe Integer
    , _ppPasswordReusePrevention :: Maybe Integer
    , _ppHardExpiry :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PasswordPolicy' data type.
--
-- 'PasswordPolicy' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
mkPasswordPolicy :: PasswordPolicy
mkPasswordPolicy = PasswordPolicy
    { _ppMinimumPasswordLength = Nothing
    , _ppRequireSymbols = Nothing
    , _ppRequireNumbers = Nothing
    , _ppRequireUppercaseCharacters = Nothing
    , _ppRequireLowercaseCharacters = Nothing
    , _ppAllowUsersToChangePassword = Nothing
    , _ppExpirePasswords = Nothing
    , _ppMaxPasswordAge = Nothing
    , _ppPasswordReusePrevention = Nothing
    , _ppHardExpiry = Nothing
    }

-- | Minimum length to require for IAM user passwords.
ppMinimumPasswordLength :: Lens' PasswordPolicy (Maybe Integer)
ppMinimumPasswordLength =
    lens _ppMinimumPasswordLength
         (\s a -> s { _ppMinimumPasswordLength = a })

-- | Specifies whether to require symbols for IAM user passwords.
ppRequireSymbols :: Lens' PasswordPolicy (Maybe Bool)
ppRequireSymbols =
    lens _ppRequireSymbols (\s a -> s { _ppRequireSymbols = a })

-- | Specifies whether to require numbers for IAM user passwords.
ppRequireNumbers :: Lens' PasswordPolicy (Maybe Bool)
ppRequireNumbers =
    lens _ppRequireNumbers (\s a -> s { _ppRequireNumbers = a })

-- | Specifies whether to require uppercase characters for IAM user passwords.
ppRequireUppercaseCharacters :: Lens' PasswordPolicy (Maybe Bool)
ppRequireUppercaseCharacters =
    lens _ppRequireUppercaseCharacters
         (\s a -> s { _ppRequireUppercaseCharacters = a })

-- | Specifies whether to require lowercase characters for IAM user passwords.
ppRequireLowercaseCharacters :: Lens' PasswordPolicy (Maybe Bool)
ppRequireLowercaseCharacters =
    lens _ppRequireLowercaseCharacters
         (\s a -> s { _ppRequireLowercaseCharacters = a })

-- | Specifies whether IAM users are allowed to change their own password.
ppAllowUsersToChangePassword :: Lens' PasswordPolicy (Maybe Bool)
ppAllowUsersToChangePassword =
    lens _ppAllowUsersToChangePassword
         (\s a -> s { _ppAllowUsersToChangePassword = a })

-- | Specifies whether IAM users are required to change their password after a
-- specified number of days.
ppExpirePasswords :: Lens' PasswordPolicy (Maybe Bool)
ppExpirePasswords =
    lens _ppExpirePasswords (\s a -> s { _ppExpirePasswords = a })

-- | The number of days that an IAM user password is valid.
ppMaxPasswordAge :: Lens' PasswordPolicy (Maybe Integer)
ppMaxPasswordAge =
    lens _ppMaxPasswordAge (\s a -> s { _ppMaxPasswordAge = a })

-- | Specifies the number of previous passwords that IAM users are prevented
-- from reusing.
ppPasswordReusePrevention :: Lens' PasswordPolicy (Maybe Integer)
ppPasswordReusePrevention =
    lens _ppPasswordReusePrevention
         (\s a -> s { _ppPasswordReusePrevention = a })

-- | Specifies whether IAM users are prevented from setting a new password after
-- their password has expired.
ppHardExpiry :: Lens' PasswordPolicy (Maybe Bool)
ppHardExpiry = lens _ppHardExpiry (\s a -> s { _ppHardExpiry = a })

instance FromXML PasswordPolicy where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PasswordPolicy"

-- | The Role data type contains information about a role. This data type is
-- used as a response element in the following actions: CreateRole GetRole
-- ListRoles.
data Role = Role
    { _rPath :: Text
    , _rRoleName :: Text
    , _rRoleId :: Text
    , _rArn :: Text
    , _rCreateDate :: ISO8601
    , _rAssumeRolePolicyDocument :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Role' data type to populate a request.
mkRole :: Text -- ^ 'rPath'
       -> Text -- ^ 'rRoleName'
       -> Text -- ^ 'rRoleId'
       -> Text -- ^ 'rArn'
       -> ISO8601 -- ^ 'rCreateDate'
       -> Role
mkRole p1 p2 p3 p4 p5 = Role
    { _rPath = p1
    , _rRoleName = p2
    , _rRoleId = p3
    , _rArn = p4
    , _rCreateDate = p5
    , _rAssumeRolePolicyDocument = Nothing
    }

-- | Path to the role. For more information about paths, see Identifiers for IAM
-- Entities in the Using IAM guide.
rPath :: Lens' Role Text
rPath = lens _rPath (\s a -> s { _rPath = a })

-- | The name identifying the role.
rRoleName :: Lens' Role Text
rRoleName = lens _rRoleName (\s a -> s { _rRoleName = a })

-- | The stable and unique string identifying the role. For more information
-- about IDs, see Identifiers for IAM Entities in the Using IAM guide.
rRoleId :: Lens' Role Text
rRoleId = lens _rRoleId (\s a -> s { _rRoleId = a })

-- | The Amazon Resource Name (ARN) specifying the role. For more information
-- about ARNs and how to use them in policies, see Identifiers for IAM
-- Entities in the Using IAM guide.
rArn :: Lens' Role Text
rArn = lens _rArn (\s a -> s { _rArn = a })

-- | The date when the role was created.
rCreateDate :: Lens' Role ISO8601
rCreateDate = lens _rCreateDate (\s a -> s { _rCreateDate = a })

-- | The policy that grants an entity permission to assume the role. The
-- returned policy is URL-encoded according to RFC 3986. For more information
-- about RFC 3986, go to http://www.faqs.org/rfcs/rfc3986.html.
rAssumeRolePolicyDocument :: Lens' Role (Maybe Text)
rAssumeRolePolicyDocument =
    lens _rAssumeRolePolicyDocument
         (\s a -> s { _rAssumeRolePolicyDocument = a })

instance FromXML Role where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Role"

instance ToQuery Role where
    toQuery = genericQuery def

-- | The list of SAML providers for this account.
data SAMLProviderListEntry = SAMLProviderListEntry
    { _samlpleArn :: Maybe Text
    , _samlpleValidUntil :: Maybe ISO8601
    , _samlpleCreateDate :: Maybe ISO8601
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SAMLProviderListEntry' data type.
--
-- 'SAMLProviderListEntry' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
mkSAMLProviderListEntry :: SAMLProviderListEntry
mkSAMLProviderListEntry = SAMLProviderListEntry
    { _samlpleArn = Nothing
    , _samlpleValidUntil = Nothing
    , _samlpleCreateDate = Nothing
    }

-- | The Amazon Resource Name (ARN) of the SAML provider.
samlpleArn :: Lens' SAMLProviderListEntry (Maybe Text)
samlpleArn = lens _samlpleArn (\s a -> s { _samlpleArn = a })

-- | The expiration date and time for the SAML provider.
samlpleValidUntil :: Lens' SAMLProviderListEntry (Maybe ISO8601)
samlpleValidUntil =
    lens _samlpleValidUntil (\s a -> s { _samlpleValidUntil = a })

-- | The date and time when the SAML provider was created.
samlpleCreateDate :: Lens' SAMLProviderListEntry (Maybe ISO8601)
samlpleCreateDate =
    lens _samlpleCreateDate (\s a -> s { _samlpleCreateDate = a })

instance FromXML SAMLProviderListEntry where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SAMLProviderListEntry"

-- | Information about the server certificate.
data ServerCertificate = ServerCertificate
    { _scServerCertificateMetadata :: ServerCertificateMetadata
    , _scCertificateBody :: Text
    , _scCertificateChain :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ServerCertificate' data type.
--
-- 'ServerCertificate' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
mkServerCertificate :: ServerCertificateMetadata -- ^ 'scServerCertificateMetadata'
                    -> Text -- ^ 'scCertificateBody'
                    -> ServerCertificate
mkServerCertificate p1 p2 = ServerCertificate
    { _scServerCertificateMetadata = p1
    , _scCertificateBody = p2
    , _scCertificateChain = Nothing
    }

-- | The meta information of the server certificate, such as its name, path, ID,
-- and ARN.
scServerCertificateMetadata :: Lens' ServerCertificate ServerCertificateMetadata
scServerCertificateMetadata =
    lens _scServerCertificateMetadata
         (\s a -> s { _scServerCertificateMetadata = a })

-- | The contents of the public key certificate.
scCertificateBody :: Lens' ServerCertificate Text
scCertificateBody =
    lens _scCertificateBody (\s a -> s { _scCertificateBody = a })

-- | The contents of the public key certificate chain.
scCertificateChain :: Lens' ServerCertificate (Maybe Text)
scCertificateChain =
    lens _scCertificateChain (\s a -> s { _scCertificateChain = a })

instance FromXML ServerCertificate where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ServerCertificate"

-- | The meta information of the server certificate, such as its name, path, ID,
-- and ARN.
data ServerCertificateMetadata = ServerCertificateMetadata
    { _scmPath :: Text
    , _scmServerCertificateName :: Text
    , _scmServerCertificateId :: Text
    , _scmArn :: Text
    , _scmUploadDate :: Maybe ISO8601
    , _scmExpiration :: Maybe ISO8601
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ServerCertificateMetadata' data type.
--
-- 'ServerCertificateMetadata' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
mkServerCertificateMetadata :: Text -- ^ 'scmPath'
                            -> Text -- ^ 'scmServerCertificateName'
                            -> Text -- ^ 'scmServerCertificateId'
                            -> Text -- ^ 'scmArn'
                            -> ServerCertificateMetadata
mkServerCertificateMetadata p1 p2 p3 p4 = ServerCertificateMetadata
    { _scmPath = p1
    , _scmServerCertificateName = p2
    , _scmServerCertificateId = p3
    , _scmArn = p4
    , _scmUploadDate = Nothing
    , _scmExpiration = Nothing
    }

-- | Path to the server certificate. For more information about paths, see
-- Identifiers for IAM Entities in the Using IAM guide.
scmPath :: Lens' ServerCertificateMetadata Text
scmPath = lens _scmPath (\s a -> s { _scmPath = a })

-- | The name that identifies the server certificate.
scmServerCertificateName :: Lens' ServerCertificateMetadata Text
scmServerCertificateName =
    lens _scmServerCertificateName
         (\s a -> s { _scmServerCertificateName = a })

-- | The stable and unique string identifying the server certificate. For more
-- information about IDs, see Identifiers for IAM Entities in the Using IAM
-- guide.
scmServerCertificateId :: Lens' ServerCertificateMetadata Text
scmServerCertificateId =
    lens _scmServerCertificateId (\s a -> s { _scmServerCertificateId = a })

-- | The Amazon Resource Name (ARN) specifying the server certificate. For more
-- information about ARNs and how to use them in policies, see Identifiers for
-- IAM Entities in the Using IAM guide.
scmArn :: Lens' ServerCertificateMetadata Text
scmArn = lens _scmArn (\s a -> s { _scmArn = a })

-- | The date when the server certificate was uploaded.
scmUploadDate :: Lens' ServerCertificateMetadata (Maybe ISO8601)
scmUploadDate = lens _scmUploadDate (\s a -> s { _scmUploadDate = a })

-- | The date on which the certificate is set to expire.
scmExpiration :: Lens' ServerCertificateMetadata (Maybe ISO8601)
scmExpiration = lens _scmExpiration (\s a -> s { _scmExpiration = a })

instance FromXML ServerCertificateMetadata where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ServerCertificateMetadata"

-- | The SigningCertificate data type contains information about an X.509
-- signing certificate. This data type is used as a response element in the
-- actions UploadSigningCertificate and ListSigningCertificates.
data SigningCertificate = SigningCertificate
    { _scrUserName :: Text
    , _scrCertificateId :: Text
    , _scrCertificateBody :: Text
    , _scrStatus :: StatusType
    , _scrUploadDate :: Maybe ISO8601
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SigningCertificate' data type.
--
-- 'SigningCertificate' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
mkSigningCertificate :: Text -- ^ 'scrUserName'
                     -> Text -- ^ 'scrCertificateId'
                     -> Text -- ^ 'scrCertificateBody'
                     -> StatusType -- ^ 'scrStatus'
                     -> SigningCertificate
mkSigningCertificate p1 p2 p3 p4 = SigningCertificate
    { _scrUserName = p1
    , _scrCertificateId = p2
    , _scrCertificateBody = p3
    , _scrStatus = p4
    , _scrUploadDate = Nothing
    }

-- | Name of the user the signing certificate is associated with.
scrUserName :: Lens' SigningCertificate Text
scrUserName = lens _scrUserName (\s a -> s { _scrUserName = a })

-- | The ID for the signing certificate.
scrCertificateId :: Lens' SigningCertificate Text
scrCertificateId =
    lens _scrCertificateId (\s a -> s { _scrCertificateId = a })

-- | The contents of the signing certificate.
scrCertificateBody :: Lens' SigningCertificate Text
scrCertificateBody =
    lens _scrCertificateBody (\s a -> s { _scrCertificateBody = a })

-- | The status of the signing certificate. Active means the key is valid for
-- API calls, while Inactive means it is not.
scrStatus :: Lens' SigningCertificate StatusType
scrStatus = lens _scrStatus (\s a -> s { _scrStatus = a })

-- | The date when the signing certificate was uploaded.
scrUploadDate :: Lens' SigningCertificate (Maybe ISO8601)
scrUploadDate = lens _scrUploadDate (\s a -> s { _scrUploadDate = a })

instance FromXML SigningCertificate where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SigningCertificate"

-- | Information about the user.
data User = User
    { _uPath :: Text
    , _uUserName :: Text
    , _uUserId :: Text
    , _uArn :: Text
    , _uCreateDate :: ISO8601
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'User' data type to populate a request.
mkUser :: Text -- ^ 'uPath'
       -> Text -- ^ 'uUserName'
       -> Text -- ^ 'uUserId'
       -> Text -- ^ 'uArn'
       -> ISO8601 -- ^ 'uCreateDate'
       -> User
mkUser p1 p2 p3 p4 p5 = User
    { _uPath = p1
    , _uUserName = p2
    , _uUserId = p3
    , _uArn = p4
    , _uCreateDate = p5
    }

-- | Path to the user. For more information about paths, see Identifiers for IAM
-- Entities in the Using IAM guide.
uPath :: Lens' User Text
uPath = lens _uPath (\s a -> s { _uPath = a })

-- | The name identifying the user.
uUserName :: Lens' User Text
uUserName = lens _uUserName (\s a -> s { _uUserName = a })

-- | The stable and unique string identifying the user. For more information
-- about IDs, see Identifiers for IAM Entities in the Using IAM guide.
uUserId :: Lens' User Text
uUserId = lens _uUserId (\s a -> s { _uUserId = a })

-- | The Amazon Resource Name (ARN) specifying the user. For more information
-- about ARNs and how to use them in policies, see Identifiers for IAM
-- Entities in the Using IAM guide.
uArn :: Lens' User Text
uArn = lens _uArn (\s a -> s { _uArn = a })

-- | The date when the user was created.
uCreateDate :: Lens' User ISO8601
uCreateDate = lens _uCreateDate (\s a -> s { _uCreateDate = a })

instance FromXML User where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "User"

instance ToQuery User where
    toQuery = genericQuery def

-- | A newly created virtual MFA device.
data VirtualMFADevice = VirtualMFADevice
    { _vmfadSerialNumber :: Text
    , _vmfadBase32StringSeed :: Maybe ByteString
    , _vmfadQRCodePNG :: Maybe ByteString
    , _vmfadUser :: Maybe User
    , _vmfadEnableDate :: Maybe ISO8601
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VirtualMFADevice' data type.
--
-- 'VirtualMFADevice' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
mkVirtualMFADevice :: Text -- ^ 'vmfadSerialNumber'
                   -> VirtualMFADevice
mkVirtualMFADevice p1 = VirtualMFADevice
    { _vmfadSerialNumber = p1
    , _vmfadBase32StringSeed = Nothing
    , _vmfadQRCodePNG = Nothing
    , _vmfadUser = Nothing
    , _vmfadEnableDate = Nothing
    }

-- | The serial number associated with VirtualMFADevice.
vmfadSerialNumber :: Lens' VirtualMFADevice Text
vmfadSerialNumber =
    lens _vmfadSerialNumber (\s a -> s { _vmfadSerialNumber = a })

-- | The Base32 seed defined as specified in RFC3548. The Base32StringSeed is
-- Base64-encoded.
vmfadBase32StringSeed :: Lens' VirtualMFADevice (Maybe ByteString)
vmfadBase32StringSeed =
    lens _vmfadBase32StringSeed (\s a -> s { _vmfadBase32StringSeed = a })

-- | A QR code PNG image that encodes
-- otpauth://totp/$virtualMFADeviceName@$AccountName? secret=$Base32String
-- where $virtualMFADeviceName is one of the create call arguments,
-- AccountName is the user name if set (accountId otherwise), and Base32String
-- is the seed in Base32 format. The Base32String is Base64-encoded.
vmfadQRCodePNG :: Lens' VirtualMFADevice (Maybe ByteString)
vmfadQRCodePNG = lens _vmfadQRCodePNG (\s a -> s { _vmfadQRCodePNG = a })

-- | The User data type contains information about a user. This data type is
-- used as a response element in the following actions: CreateUser GetUser
-- ListUsers.
vmfadUser :: Lens' VirtualMFADevice (Maybe User)
vmfadUser = lens _vmfadUser (\s a -> s { _vmfadUser = a })

vmfadEnableDate :: Lens' VirtualMFADevice (Maybe ISO8601)
vmfadEnableDate = lens _vmfadEnableDate (\s a -> s { _vmfadEnableDate = a })

instance FromXML VirtualMFADevice where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "VirtualMFADevice"
