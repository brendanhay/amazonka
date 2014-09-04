{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
    -- ** Errors
    , Er (..)
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
    , AccessKey (..)
    , akUserName
    , akAccessKeyId
    , akStatus
    , akSecretAccessKey
    , akCreateDate

    -- * AccessKeyMetadata
    , AccessKeyMetadata (..)
    , akmUserName
    , akmAccessKeyId
    , akmStatus
    , akmCreateDate

    -- * Group
    , Group (..)
    , gPath
    , gGroupName
    , gGroupId
    , gArn
    , gCreateDate

    -- * InstanceProfile
    , InstanceProfile (..)
    , ipPath
    , ipInstanceProfileName
    , ipInstanceProfileId
    , ipArn
    , ipCreateDate
    , ipRoles

    -- * LoginProfile
    , LoginProfile (..)
    , lpUserName
    , lpCreateDate
    , lpPasswordResetRequired

    -- * MFADevice
    , MFADevice (..)
    , mfadUserName
    , mfadSerialNumber
    , mfadEnableDate

    -- * PasswordPolicy
    , PasswordPolicy (..)
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
    , Role (..)
    , rPath
    , rRoleName
    , rRoleId
    , rArn
    , rCreateDate
    , rAssumeRolePolicyDocument

    -- * SAMLProviderListEntry
    , SAMLProviderListEntry (..)
    , samlpleArn
    , samlpleValidUntil
    , samlpleCreateDate

    -- * ServerCertificate
    , ServerCertificate (..)
    , scServerCertificateMetadata
    , scCertificateBody
    , scCertificateChain

    -- * ServerCertificateMetadata
    , ServerCertificateMetadata (..)
    , scmPath
    , scmServerCertificateName
    , scmServerCertificateId
    , scmArn
    , scmUploadDate
    , scmExpiration

    -- * SigningCertificate
    , SigningCertificate (..)
    , sdUserName
    , sdCertificateId
    , sdCertificateBody
    , sdStatus
    , sdUploadDate

    -- * User
    , User (..)
    , uPath
    , uUserName
    , uUserId
    , uArn
    , uCreateDate

    -- * VirtualMFADevice
    , VirtualMFADevice (..)
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

-- | The status (unassigned or assigned) of the devices to list. If you do not
-- specify an AssignmentStatus, the action defaults to Any which lists both
-- assigned and unassigned virtual MFA devices.
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

-- | The format (MIME type) of the credential report.
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

-- | Information about the state of a credential report.
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

-- | The status of the access key. Active means the key is valid for API calls,
-- while Inactive means it is not.
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
      -- ^ Name of the user the key is associated with.
    , _akAccessKeyId :: Text
      -- ^ The ID for this access key.
    , _akStatus :: StatusType
      -- ^ The status of the access key. Active means the key is valid for
      -- API calls, while Inactive means it is not.
    , _akSecretAccessKey :: Text
      -- ^ The secret key used to sign requests.
    , _akCreateDate :: Maybe ISO8601
      -- ^ The date when the access key was created.
    } deriving (Show, Generic)

-- | Name of the user the key is associated with.
akUserName :: Lens' AccessKey (Text)
akUserName f x =
    f (_akUserName x)
        <&> \y -> x { _akUserName = y }
{-# INLINE akUserName #-}

-- | The ID for this access key.
akAccessKeyId :: Lens' AccessKey (Text)
akAccessKeyId f x =
    f (_akAccessKeyId x)
        <&> \y -> x { _akAccessKeyId = y }
{-# INLINE akAccessKeyId #-}

-- | The status of the access key. Active means the key is valid for API calls,
-- while Inactive means it is not.
akStatus :: Lens' AccessKey (StatusType)
akStatus f x =
    f (_akStatus x)
        <&> \y -> x { _akStatus = y }
{-# INLINE akStatus #-}

-- | The secret key used to sign requests.
akSecretAccessKey :: Lens' AccessKey (Text)
akSecretAccessKey f x =
    f (_akSecretAccessKey x)
        <&> \y -> x { _akSecretAccessKey = y }
{-# INLINE akSecretAccessKey #-}

-- | The date when the access key was created.
akCreateDate :: Lens' AccessKey (Maybe ISO8601)
akCreateDate f x =
    f (_akCreateDate x)
        <&> \y -> x { _akCreateDate = y }
{-# INLINE akCreateDate #-}

instance FromXML AccessKey where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AccessKey"

-- | The AccessKey data type contains information about an AWS access key,
-- without its secret key. This data type is used as a response element in the
-- action ListAccessKeys.
data AccessKeyMetadata = AccessKeyMetadata
    { _akmUserName :: Maybe Text
      -- ^ Name of the user the key is associated with.
    , _akmAccessKeyId :: Maybe Text
      -- ^ The ID for this access key.
    , _akmStatus :: Maybe StatusType
      -- ^ The status of the access key. Active means the key is valid for
      -- API calls, while Inactive means it is not.
    , _akmCreateDate :: Maybe ISO8601
      -- ^ The date when the access key was created.
    } deriving (Show, Generic)

-- | Name of the user the key is associated with.
akmUserName :: Lens' AccessKeyMetadata (Maybe Text)
akmUserName f x =
    f (_akmUserName x)
        <&> \y -> x { _akmUserName = y }
{-# INLINE akmUserName #-}

-- | The ID for this access key.
akmAccessKeyId :: Lens' AccessKeyMetadata (Maybe Text)
akmAccessKeyId f x =
    f (_akmAccessKeyId x)
        <&> \y -> x { _akmAccessKeyId = y }
{-# INLINE akmAccessKeyId #-}

-- | The status of the access key. Active means the key is valid for API calls,
-- while Inactive means it is not.
akmStatus :: Lens' AccessKeyMetadata (Maybe StatusType)
akmStatus f x =
    f (_akmStatus x)
        <&> \y -> x { _akmStatus = y }
{-# INLINE akmStatus #-}

-- | The date when the access key was created.
akmCreateDate :: Lens' AccessKeyMetadata (Maybe ISO8601)
akmCreateDate f x =
    f (_akmCreateDate x)
        <&> \y -> x { _akmCreateDate = y }
{-# INLINE akmCreateDate #-}

instance FromXML AccessKeyMetadata where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AccessKeyMetadata"

-- | Information about the group.
data Group = Group
    { _gPath :: Text
      -- ^ Path to the group. For more information about paths, see
      -- Identifiers for IAM Entities in the Using IAM guide.
    , _gGroupName :: Text
      -- ^ The name that identifies the group.
    , _gGroupId :: Text
      -- ^ The stable and unique string identifying the group. For more
      -- information about IDs, see Identifiers for IAM Entities in the
      -- Using IAM guide.
    , _gArn :: Text
      -- ^ The Amazon Resource Name (ARN) specifying the group. For more
      -- information about ARNs and how to use them in policies, see
      -- Identifiers for IAM Entities in the Using IAM guide.
    , _gCreateDate :: ISO8601
      -- ^ The date when the group was created.
    } deriving (Show, Generic)

-- | Path to the group. For more information about paths, see Identifiers for
-- IAM Entities in the Using IAM guide.
gPath :: Lens' Group (Text)
gPath f x =
    f (_gPath x)
        <&> \y -> x { _gPath = y }
{-# INLINE gPath #-}

-- | The name that identifies the group.
gGroupName :: Lens' Group (Text)
gGroupName f x =
    f (_gGroupName x)
        <&> \y -> x { _gGroupName = y }
{-# INLINE gGroupName #-}

-- | The stable and unique string identifying the group. For more information
-- about IDs, see Identifiers for IAM Entities in the Using IAM guide.
gGroupId :: Lens' Group (Text)
gGroupId f x =
    f (_gGroupId x)
        <&> \y -> x { _gGroupId = y }
{-# INLINE gGroupId #-}

-- | The Amazon Resource Name (ARN) specifying the group. For more information
-- about ARNs and how to use them in policies, see Identifiers for IAM
-- Entities in the Using IAM guide.
gArn :: Lens' Group (Text)
gArn f x =
    f (_gArn x)
        <&> \y -> x { _gArn = y }
{-# INLINE gArn #-}

-- | The date when the group was created.
gCreateDate :: Lens' Group (ISO8601)
gCreateDate f x =
    f (_gCreateDate x)
        <&> \y -> x { _gCreateDate = y }
{-# INLINE gCreateDate #-}

instance FromXML Group where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Group"

-- | Information about the instance profile.
data InstanceProfile = InstanceProfile
    { _ipPath :: Text
      -- ^ Path to the instance profile. For more information about paths,
      -- see Identifiers for IAM Entities in the Using IAM guide.
    , _ipInstanceProfileName :: Text
      -- ^ The name identifying the instance profile.
    , _ipInstanceProfileId :: Text
      -- ^ The stable and unique string identifying the instance profile.
      -- For more information about IDs, see Identifiers for IAM Entities
      -- in the Using IAM guide.
    , _ipArn :: Text
      -- ^ The Amazon Resource Name (ARN) specifying the instance profile.
      -- For more information about ARNs and how to use them in policies,
      -- see Identifiers for IAM Entities in the Using IAM guide.
    , _ipCreateDate :: ISO8601
      -- ^ The date when the instance profile was created.
    , _ipRoles :: [Role]
      -- ^ The role associated with the instance profile.
    } deriving (Show, Generic)

-- | Path to the instance profile. For more information about paths, see
-- Identifiers for IAM Entities in the Using IAM guide.
ipPath :: Lens' InstanceProfile (Text)
ipPath f x =
    f (_ipPath x)
        <&> \y -> x { _ipPath = y }
{-# INLINE ipPath #-}

-- | The name identifying the instance profile.
ipInstanceProfileName :: Lens' InstanceProfile (Text)
ipInstanceProfileName f x =
    f (_ipInstanceProfileName x)
        <&> \y -> x { _ipInstanceProfileName = y }
{-# INLINE ipInstanceProfileName #-}

-- | The stable and unique string identifying the instance profile. For more
-- information about IDs, see Identifiers for IAM Entities in the Using IAM
-- guide.
ipInstanceProfileId :: Lens' InstanceProfile (Text)
ipInstanceProfileId f x =
    f (_ipInstanceProfileId x)
        <&> \y -> x { _ipInstanceProfileId = y }
{-# INLINE ipInstanceProfileId #-}

-- | The Amazon Resource Name (ARN) specifying the instance profile. For more
-- information about ARNs and how to use them in policies, see Identifiers for
-- IAM Entities in the Using IAM guide.
ipArn :: Lens' InstanceProfile (Text)
ipArn f x =
    f (_ipArn x)
        <&> \y -> x { _ipArn = y }
{-# INLINE ipArn #-}

-- | The date when the instance profile was created.
ipCreateDate :: Lens' InstanceProfile (ISO8601)
ipCreateDate f x =
    f (_ipCreateDate x)
        <&> \y -> x { _ipCreateDate = y }
{-# INLINE ipCreateDate #-}

-- | The role associated with the instance profile.
ipRoles :: Lens' InstanceProfile ([Role])
ipRoles f x =
    f (_ipRoles x)
        <&> \y -> x { _ipRoles = y }
{-# INLINE ipRoles #-}

instance FromXML InstanceProfile where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "InstanceProfile"

-- | The user name and password create date.
data LoginProfile = LoginProfile
    { _lpUserName :: Text
      -- ^ The name of the user, which can be used for signing in to the AWS
      -- Management Console.
    , _lpCreateDate :: ISO8601
      -- ^ The date when the password for the user was created.
    , _lpPasswordResetRequired :: Maybe Bool
      -- ^ Specifies whether the user is required to set a new password on
      -- next sign-in.
    } deriving (Show, Generic)

-- | The name of the user, which can be used for signing in to the AWS
-- Management Console.
lpUserName :: Lens' LoginProfile (Text)
lpUserName f x =
    f (_lpUserName x)
        <&> \y -> x { _lpUserName = y }
{-# INLINE lpUserName #-}

-- | The date when the password for the user was created.
lpCreateDate :: Lens' LoginProfile (ISO8601)
lpCreateDate f x =
    f (_lpCreateDate x)
        <&> \y -> x { _lpCreateDate = y }
{-# INLINE lpCreateDate #-}

-- | Specifies whether the user is required to set a new password on next
-- sign-in.
lpPasswordResetRequired :: Lens' LoginProfile (Maybe Bool)
lpPasswordResetRequired f x =
    f (_lpPasswordResetRequired x)
        <&> \y -> x { _lpPasswordResetRequired = y }
{-# INLINE lpPasswordResetRequired #-}

instance FromXML LoginProfile where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LoginProfile"

-- | The MFADevice data type contains information about an MFA device. This data
-- type is used as a response element in the action ListMFADevices.
data MFADevice = MFADevice
    { _mfadUserName :: Text
      -- ^ The user with whom the MFA device is associated.
    , _mfadSerialNumber :: Text
      -- ^ The serial number that uniquely identifies the MFA device. For
      -- virtual MFA devices, the serial number is the device ARN.
    , _mfadEnableDate :: ISO8601
      -- ^ The date when the MFA device was enabled for the user.
    } deriving (Show, Generic)

-- | The user with whom the MFA device is associated.
mfadUserName :: Lens' MFADevice (Text)
mfadUserName f x =
    f (_mfadUserName x)
        <&> \y -> x { _mfadUserName = y }
{-# INLINE mfadUserName #-}

-- | The serial number that uniquely identifies the MFA device. For virtual MFA
-- devices, the serial number is the device ARN.
mfadSerialNumber :: Lens' MFADevice (Text)
mfadSerialNumber f x =
    f (_mfadSerialNumber x)
        <&> \y -> x { _mfadSerialNumber = y }
{-# INLINE mfadSerialNumber #-}

-- | The date when the MFA device was enabled for the user.
mfadEnableDate :: Lens' MFADevice (ISO8601)
mfadEnableDate f x =
    f (_mfadEnableDate x)
        <&> \y -> x { _mfadEnableDate = y }
{-# INLINE mfadEnableDate #-}

instance FromXML MFADevice where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "MFADevice"

-- | The PasswordPolicy data type contains information about the account
-- password policy. This data type is used as a response element in the action
-- GetAccountPasswordPolicy.
data PasswordPolicy = PasswordPolicy
    { _ppMinimumPasswordLength :: Maybe Integer
      -- ^ Minimum length to require for IAM user passwords.
    , _ppRequireSymbols :: Maybe Bool
      -- ^ Specifies whether to require symbols for IAM user passwords.
    , _ppRequireNumbers :: Maybe Bool
      -- ^ Specifies whether to require numbers for IAM user passwords.
    , _ppRequireUppercaseCharacters :: Maybe Bool
      -- ^ Specifies whether to require uppercase characters for IAM user
      -- passwords.
    , _ppRequireLowercaseCharacters :: Maybe Bool
      -- ^ Specifies whether to require lowercase characters for IAM user
      -- passwords.
    , _ppAllowUsersToChangePassword :: Maybe Bool
      -- ^ Specifies whether IAM users are allowed to change their own
      -- password.
    , _ppExpirePasswords :: Maybe Bool
      -- ^ Specifies whether IAM users are required to change their password
      -- after a specified number of days.
    , _ppMaxPasswordAge :: Maybe Integer
      -- ^ The number of days that an IAM user password is valid.
    , _ppPasswordReusePrevention :: Maybe Integer
      -- ^ Specifies the number of previous passwords that IAM users are
      -- prevented from reusing.
    , _ppHardExpiry :: Maybe Bool
      -- ^ Specifies whether IAM users are prevented from setting a new
      -- password after their password has expired.
    } deriving (Show, Generic)

-- | Minimum length to require for IAM user passwords.
ppMinimumPasswordLength :: Lens' PasswordPolicy (Maybe Integer)
ppMinimumPasswordLength f x =
    f (_ppMinimumPasswordLength x)
        <&> \y -> x { _ppMinimumPasswordLength = y }
{-# INLINE ppMinimumPasswordLength #-}

-- | Specifies whether to require symbols for IAM user passwords.
ppRequireSymbols :: Lens' PasswordPolicy (Maybe Bool)
ppRequireSymbols f x =
    f (_ppRequireSymbols x)
        <&> \y -> x { _ppRequireSymbols = y }
{-# INLINE ppRequireSymbols #-}

-- | Specifies whether to require numbers for IAM user passwords.
ppRequireNumbers :: Lens' PasswordPolicy (Maybe Bool)
ppRequireNumbers f x =
    f (_ppRequireNumbers x)
        <&> \y -> x { _ppRequireNumbers = y }
{-# INLINE ppRequireNumbers #-}

-- | Specifies whether to require uppercase characters for IAM user passwords.
ppRequireUppercaseCharacters :: Lens' PasswordPolicy (Maybe Bool)
ppRequireUppercaseCharacters f x =
    f (_ppRequireUppercaseCharacters x)
        <&> \y -> x { _ppRequireUppercaseCharacters = y }
{-# INLINE ppRequireUppercaseCharacters #-}

-- | Specifies whether to require lowercase characters for IAM user passwords.
ppRequireLowercaseCharacters :: Lens' PasswordPolicy (Maybe Bool)
ppRequireLowercaseCharacters f x =
    f (_ppRequireLowercaseCharacters x)
        <&> \y -> x { _ppRequireLowercaseCharacters = y }
{-# INLINE ppRequireLowercaseCharacters #-}

-- | Specifies whether IAM users are allowed to change their own password.
ppAllowUsersToChangePassword :: Lens' PasswordPolicy (Maybe Bool)
ppAllowUsersToChangePassword f x =
    f (_ppAllowUsersToChangePassword x)
        <&> \y -> x { _ppAllowUsersToChangePassword = y }
{-# INLINE ppAllowUsersToChangePassword #-}

-- | Specifies whether IAM users are required to change their password after a
-- specified number of days.
ppExpirePasswords :: Lens' PasswordPolicy (Maybe Bool)
ppExpirePasswords f x =
    f (_ppExpirePasswords x)
        <&> \y -> x { _ppExpirePasswords = y }
{-# INLINE ppExpirePasswords #-}

-- | The number of days that an IAM user password is valid.
ppMaxPasswordAge :: Lens' PasswordPolicy (Maybe Integer)
ppMaxPasswordAge f x =
    f (_ppMaxPasswordAge x)
        <&> \y -> x { _ppMaxPasswordAge = y }
{-# INLINE ppMaxPasswordAge #-}

-- | Specifies the number of previous passwords that IAM users are prevented
-- from reusing.
ppPasswordReusePrevention :: Lens' PasswordPolicy (Maybe Integer)
ppPasswordReusePrevention f x =
    f (_ppPasswordReusePrevention x)
        <&> \y -> x { _ppPasswordReusePrevention = y }
{-# INLINE ppPasswordReusePrevention #-}

-- | Specifies whether IAM users are prevented from setting a new password after
-- their password has expired.
ppHardExpiry :: Lens' PasswordPolicy (Maybe Bool)
ppHardExpiry f x =
    f (_ppHardExpiry x)
        <&> \y -> x { _ppHardExpiry = y }
{-# INLINE ppHardExpiry #-}

instance FromXML PasswordPolicy where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PasswordPolicy"

-- | The Role data type contains information about a role. This data type is
-- used as a response element in the following actions: CreateRole GetRole
-- ListRoles.
data Role = Role
    { _rPath :: Text
      -- ^ Path to the role. For more information about paths, see
      -- Identifiers for IAM Entities in the Using IAM guide.
    , _rRoleName :: Text
      -- ^ The name identifying the role.
    , _rRoleId :: Text
      -- ^ The stable and unique string identifying the role. For more
      -- information about IDs, see Identifiers for IAM Entities in the
      -- Using IAM guide.
    , _rArn :: Text
      -- ^ The Amazon Resource Name (ARN) specifying the role. For more
      -- information about ARNs and how to use them in policies, see
      -- Identifiers for IAM Entities in the Using IAM guide.
    , _rCreateDate :: ISO8601
      -- ^ The date when the role was created.
    , _rAssumeRolePolicyDocument :: Maybe Text
      -- ^ The policy that grants an entity permission to assume the role.
      -- The returned policy is URL-encoded according to RFC 3986. For
      -- more information about RFC 3986, go to
      -- http://www.faqs.org/rfcs/rfc3986.html.
    } deriving (Show, Generic)

-- | Path to the role. For more information about paths, see Identifiers for IAM
-- Entities in the Using IAM guide.
rPath :: Lens' Role (Text)
rPath f x =
    f (_rPath x)
        <&> \y -> x { _rPath = y }
{-# INLINE rPath #-}

-- | The name identifying the role.
rRoleName :: Lens' Role (Text)
rRoleName f x =
    f (_rRoleName x)
        <&> \y -> x { _rRoleName = y }
{-# INLINE rRoleName #-}

-- | The stable and unique string identifying the role. For more information
-- about IDs, see Identifiers for IAM Entities in the Using IAM guide.
rRoleId :: Lens' Role (Text)
rRoleId f x =
    f (_rRoleId x)
        <&> \y -> x { _rRoleId = y }
{-# INLINE rRoleId #-}

-- | The Amazon Resource Name (ARN) specifying the role. For more information
-- about ARNs and how to use them in policies, see Identifiers for IAM
-- Entities in the Using IAM guide.
rArn :: Lens' Role (Text)
rArn f x =
    f (_rArn x)
        <&> \y -> x { _rArn = y }
{-# INLINE rArn #-}

-- | The date when the role was created.
rCreateDate :: Lens' Role (ISO8601)
rCreateDate f x =
    f (_rCreateDate x)
        <&> \y -> x { _rCreateDate = y }
{-# INLINE rCreateDate #-}

-- | The policy that grants an entity permission to assume the role. The
-- returned policy is URL-encoded according to RFC 3986. For more information
-- about RFC 3986, go to http://www.faqs.org/rfcs/rfc3986.html.
rAssumeRolePolicyDocument :: Lens' Role (Maybe Text)
rAssumeRolePolicyDocument f x =
    f (_rAssumeRolePolicyDocument x)
        <&> \y -> x { _rAssumeRolePolicyDocument = y }
{-# INLINE rAssumeRolePolicyDocument #-}

instance FromXML Role where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Role"

instance ToQuery Role where
    toQuery = genericQuery def

-- | The list of SAML providers for this account.
data SAMLProviderListEntry = SAMLProviderListEntry
    { _samlpleArn :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the SAML provider.
    , _samlpleValidUntil :: Maybe ISO8601
      -- ^ The expiration date and time for the SAML provider.
    , _samlpleCreateDate :: Maybe ISO8601
      -- ^ The date and time when the SAML provider was created.
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the SAML provider.
samlpleArn :: Lens' SAMLProviderListEntry (Maybe Text)
samlpleArn f x =
    f (_samlpleArn x)
        <&> \y -> x { _samlpleArn = y }
{-# INLINE samlpleArn #-}

-- | The expiration date and time for the SAML provider.
samlpleValidUntil :: Lens' SAMLProviderListEntry (Maybe ISO8601)
samlpleValidUntil f x =
    f (_samlpleValidUntil x)
        <&> \y -> x { _samlpleValidUntil = y }
{-# INLINE samlpleValidUntil #-}

-- | The date and time when the SAML provider was created.
samlpleCreateDate :: Lens' SAMLProviderListEntry (Maybe ISO8601)
samlpleCreateDate f x =
    f (_samlpleCreateDate x)
        <&> \y -> x { _samlpleCreateDate = y }
{-# INLINE samlpleCreateDate #-}

instance FromXML SAMLProviderListEntry where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SAMLProviderListEntry"

-- | Information about the server certificate.
data ServerCertificate = ServerCertificate
    { _scServerCertificateMetadata :: ServerCertificateMetadata
      -- ^ The meta information of the server certificate, such as its name,
      -- path, ID, and ARN.
    , _scCertificateBody :: Text
      -- ^ The contents of the public key certificate.
    , _scCertificateChain :: Maybe Text
      -- ^ The contents of the public key certificate chain.
    } deriving (Show, Generic)

-- | The meta information of the server certificate, such as its name, path, ID,
-- and ARN.
scServerCertificateMetadata :: Lens' ServerCertificate (ServerCertificateMetadata)
scServerCertificateMetadata f x =
    f (_scServerCertificateMetadata x)
        <&> \y -> x { _scServerCertificateMetadata = y }
{-# INLINE scServerCertificateMetadata #-}

-- | The contents of the public key certificate.
scCertificateBody :: Lens' ServerCertificate (Text)
scCertificateBody f x =
    f (_scCertificateBody x)
        <&> \y -> x { _scCertificateBody = y }
{-# INLINE scCertificateBody #-}

-- | The contents of the public key certificate chain.
scCertificateChain :: Lens' ServerCertificate (Maybe Text)
scCertificateChain f x =
    f (_scCertificateChain x)
        <&> \y -> x { _scCertificateChain = y }
{-# INLINE scCertificateChain #-}

instance FromXML ServerCertificate where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ServerCertificate"

-- | The meta information of the server certificate, such as its name, path, ID,
-- and ARN.
data ServerCertificateMetadata = ServerCertificateMetadata
    { _scmPath :: Text
      -- ^ Path to the server certificate. For more information about paths,
      -- see Identifiers for IAM Entities in the Using IAM guide.
    , _scmServerCertificateName :: Text
      -- ^ The name that identifies the server certificate.
    , _scmServerCertificateId :: Text
      -- ^ The stable and unique string identifying the server certificate.
      -- For more information about IDs, see Identifiers for IAM Entities
      -- in the Using IAM guide.
    , _scmArn :: Text
      -- ^ The Amazon Resource Name (ARN) specifying the server certificate.
      -- For more information about ARNs and how to use them in policies,
      -- see Identifiers for IAM Entities in the Using IAM guide.
    , _scmUploadDate :: Maybe ISO8601
      -- ^ The date when the server certificate was uploaded.
    , _scmExpiration :: Maybe ISO8601
      -- ^ The date on which the certificate is set to expire.
    } deriving (Show, Generic)

-- | Path to the server certificate. For more information about paths, see
-- Identifiers for IAM Entities in the Using IAM guide.
scmPath :: Lens' ServerCertificateMetadata (Text)
scmPath f x =
    f (_scmPath x)
        <&> \y -> x { _scmPath = y }
{-# INLINE scmPath #-}

-- | The name that identifies the server certificate.
scmServerCertificateName :: Lens' ServerCertificateMetadata (Text)
scmServerCertificateName f x =
    f (_scmServerCertificateName x)
        <&> \y -> x { _scmServerCertificateName = y }
{-# INLINE scmServerCertificateName #-}

-- | The stable and unique string identifying the server certificate. For more
-- information about IDs, see Identifiers for IAM Entities in the Using IAM
-- guide.
scmServerCertificateId :: Lens' ServerCertificateMetadata (Text)
scmServerCertificateId f x =
    f (_scmServerCertificateId x)
        <&> \y -> x { _scmServerCertificateId = y }
{-# INLINE scmServerCertificateId #-}

-- | The Amazon Resource Name (ARN) specifying the server certificate. For more
-- information about ARNs and how to use them in policies, see Identifiers for
-- IAM Entities in the Using IAM guide.
scmArn :: Lens' ServerCertificateMetadata (Text)
scmArn f x =
    f (_scmArn x)
        <&> \y -> x { _scmArn = y }
{-# INLINE scmArn #-}

-- | The date when the server certificate was uploaded.
scmUploadDate :: Lens' ServerCertificateMetadata (Maybe ISO8601)
scmUploadDate f x =
    f (_scmUploadDate x)
        <&> \y -> x { _scmUploadDate = y }
{-# INLINE scmUploadDate #-}

-- | The date on which the certificate is set to expire.
scmExpiration :: Lens' ServerCertificateMetadata (Maybe ISO8601)
scmExpiration f x =
    f (_scmExpiration x)
        <&> \y -> x { _scmExpiration = y }
{-# INLINE scmExpiration #-}

instance FromXML ServerCertificateMetadata where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ServerCertificateMetadata"

-- | The SigningCertificate data type contains information about an X.509
-- signing certificate. This data type is used as a response element in the
-- actions UploadSigningCertificate and ListSigningCertificates.
data SigningCertificate = SigningCertificate
    { _sdUserName :: Text
      -- ^ Name of the user the signing certificate is associated with.
    , _sdCertificateId :: Text
      -- ^ The ID for the signing certificate.
    , _sdCertificateBody :: Text
      -- ^ The contents of the signing certificate.
    , _sdStatus :: StatusType
      -- ^ The status of the signing certificate. Active means the key is
      -- valid for API calls, while Inactive means it is not.
    , _sdUploadDate :: Maybe ISO8601
      -- ^ The date when the signing certificate was uploaded.
    } deriving (Show, Generic)

-- | Name of the user the signing certificate is associated with.
sdUserName :: Lens' SigningCertificate (Text)
sdUserName f x =
    f (_sdUserName x)
        <&> \y -> x { _sdUserName = y }
{-# INLINE sdUserName #-}

-- | The ID for the signing certificate.
sdCertificateId :: Lens' SigningCertificate (Text)
sdCertificateId f x =
    f (_sdCertificateId x)
        <&> \y -> x { _sdCertificateId = y }
{-# INLINE sdCertificateId #-}

-- | The contents of the signing certificate.
sdCertificateBody :: Lens' SigningCertificate (Text)
sdCertificateBody f x =
    f (_sdCertificateBody x)
        <&> \y -> x { _sdCertificateBody = y }
{-# INLINE sdCertificateBody #-}

-- | The status of the signing certificate. Active means the key is valid for
-- API calls, while Inactive means it is not.
sdStatus :: Lens' SigningCertificate (StatusType)
sdStatus f x =
    f (_sdStatus x)
        <&> \y -> x { _sdStatus = y }
{-# INLINE sdStatus #-}

-- | The date when the signing certificate was uploaded.
sdUploadDate :: Lens' SigningCertificate (Maybe ISO8601)
sdUploadDate f x =
    f (_sdUploadDate x)
        <&> \y -> x { _sdUploadDate = y }
{-# INLINE sdUploadDate #-}

instance FromXML SigningCertificate where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SigningCertificate"

-- | Information about the user.
data User = User
    { _uPath :: Text
      -- ^ Path to the user. For more information about paths, see
      -- Identifiers for IAM Entities in the Using IAM guide.
    , _uUserName :: Text
      -- ^ The name identifying the user.
    , _uUserId :: Text
      -- ^ The stable and unique string identifying the user. For more
      -- information about IDs, see Identifiers for IAM Entities in the
      -- Using IAM guide.
    , _uArn :: Text
      -- ^ The Amazon Resource Name (ARN) specifying the user. For more
      -- information about ARNs and how to use them in policies, see
      -- Identifiers for IAM Entities in the Using IAM guide.
    , _uCreateDate :: ISO8601
      -- ^ The date when the user was created.
    } deriving (Show, Generic)

-- | Path to the user. For more information about paths, see Identifiers for IAM
-- Entities in the Using IAM guide.
uPath :: Lens' User (Text)
uPath f x =
    f (_uPath x)
        <&> \y -> x { _uPath = y }
{-# INLINE uPath #-}

-- | The name identifying the user.
uUserName :: Lens' User (Text)
uUserName f x =
    f (_uUserName x)
        <&> \y -> x { _uUserName = y }
{-# INLINE uUserName #-}

-- | The stable and unique string identifying the user. For more information
-- about IDs, see Identifiers for IAM Entities in the Using IAM guide.
uUserId :: Lens' User (Text)
uUserId f x =
    f (_uUserId x)
        <&> \y -> x { _uUserId = y }
{-# INLINE uUserId #-}

-- | The Amazon Resource Name (ARN) specifying the user. For more information
-- about ARNs and how to use them in policies, see Identifiers for IAM
-- Entities in the Using IAM guide.
uArn :: Lens' User (Text)
uArn f x =
    f (_uArn x)
        <&> \y -> x { _uArn = y }
{-# INLINE uArn #-}

-- | The date when the user was created.
uCreateDate :: Lens' User (ISO8601)
uCreateDate f x =
    f (_uCreateDate x)
        <&> \y -> x { _uCreateDate = y }
{-# INLINE uCreateDate #-}

instance FromXML User where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "User"

instance ToQuery User where
    toQuery = genericQuery def

-- | A newly created virtual MFA device.
data VirtualMFADevice = VirtualMFADevice
    { _vmfadSerialNumber :: Text
      -- ^ The serial number associated with VirtualMFADevice.
    , _vmfadBase32StringSeed :: Maybe ByteString
      -- ^ The Base32 seed defined as specified in RFC3548. The
      -- Base32StringSeed is Base64-encoded.
    , _vmfadQRCodePNG :: Maybe ByteString
      -- ^ A QR code PNG image that encodes
      -- otpauth://totp/$virtualMFADeviceName@$AccountName?
      -- secret=$Base32String where $virtualMFADeviceName is one of the
      -- create call arguments, AccountName is the user name if set
      -- (accountId otherwise), and Base32String is the seed in Base32
      -- format. The Base32String is Base64-encoded.
    , _vmfadUser :: Maybe User
      -- ^ The User data type contains information about a user. This data
      -- type is used as a response element in the following actions:
      -- CreateUser GetUser ListUsers.
    , _vmfadEnableDate :: Maybe ISO8601
    } deriving (Show, Generic)

-- | The serial number associated with VirtualMFADevice.
vmfadSerialNumber :: Lens' VirtualMFADevice (Text)
vmfadSerialNumber f x =
    f (_vmfadSerialNumber x)
        <&> \y -> x { _vmfadSerialNumber = y }
{-# INLINE vmfadSerialNumber #-}

-- | The Base32 seed defined as specified in RFC3548. The Base32StringSeed is
-- Base64-encoded.
vmfadBase32StringSeed :: Lens' VirtualMFADevice (Maybe ByteString)
vmfadBase32StringSeed f x =
    f (_vmfadBase32StringSeed x)
        <&> \y -> x { _vmfadBase32StringSeed = y }
{-# INLINE vmfadBase32StringSeed #-}

-- | A QR code PNG image that encodes
-- otpauth://totp/$virtualMFADeviceName@$AccountName? secret=$Base32String
-- where $virtualMFADeviceName is one of the create call arguments,
-- AccountName is the user name if set (accountId otherwise), and Base32String
-- is the seed in Base32 format. The Base32String is Base64-encoded.
vmfadQRCodePNG :: Lens' VirtualMFADevice (Maybe ByteString)
vmfadQRCodePNG f x =
    f (_vmfadQRCodePNG x)
        <&> \y -> x { _vmfadQRCodePNG = y }
{-# INLINE vmfadQRCodePNG #-}

-- | The User data type contains information about a user. This data type is
-- used as a response element in the following actions: CreateUser GetUser
-- ListUsers.
vmfadUser :: Lens' VirtualMFADevice (Maybe User)
vmfadUser f x =
    f (_vmfadUser x)
        <&> \y -> x { _vmfadUser = y }
{-# INLINE vmfadUser #-}

vmfadEnableDate :: Lens' VirtualMFADevice (Maybe ISO8601)
vmfadEnableDate f x =
    f (_vmfadEnableDate x)
        <&> \y -> x { _vmfadEnableDate = y }
{-# INLINE vmfadEnableDate #-}

instance FromXML VirtualMFADevice where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "VirtualMFADevice"
