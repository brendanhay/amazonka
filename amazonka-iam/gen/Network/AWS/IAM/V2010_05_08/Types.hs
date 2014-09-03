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
akUserName
    :: Functor f
    => (Text
    -> f (Text))
    -> AccessKey
    -> f AccessKey
akUserName f x =
    (\y -> x { _akUserName = y })
       <$> f (_akUserName x)
{-# INLINE akUserName #-}

-- | The ID for this access key.
akAccessKeyId
    :: Functor f
    => (Text
    -> f (Text))
    -> AccessKey
    -> f AccessKey
akAccessKeyId f x =
    (\y -> x { _akAccessKeyId = y })
       <$> f (_akAccessKeyId x)
{-# INLINE akAccessKeyId #-}

-- | The status of the access key. Active means the key is valid for API calls,
-- while Inactive means it is not.
akStatus
    :: Functor f
    => (StatusType
    -> f (StatusType))
    -> AccessKey
    -> f AccessKey
akStatus f x =
    (\y -> x { _akStatus = y })
       <$> f (_akStatus x)
{-# INLINE akStatus #-}

-- | The secret key used to sign requests.
akSecretAccessKey
    :: Functor f
    => (Text
    -> f (Text))
    -> AccessKey
    -> f AccessKey
akSecretAccessKey f x =
    (\y -> x { _akSecretAccessKey = y })
       <$> f (_akSecretAccessKey x)
{-# INLINE akSecretAccessKey #-}

-- | The date when the access key was created.
akCreateDate
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> AccessKey
    -> f AccessKey
akCreateDate f x =
    (\y -> x { _akCreateDate = y })
       <$> f (_akCreateDate x)
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
akmUserName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AccessKeyMetadata
    -> f AccessKeyMetadata
akmUserName f x =
    (\y -> x { _akmUserName = y })
       <$> f (_akmUserName x)
{-# INLINE akmUserName #-}

-- | The ID for this access key.
akmAccessKeyId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AccessKeyMetadata
    -> f AccessKeyMetadata
akmAccessKeyId f x =
    (\y -> x { _akmAccessKeyId = y })
       <$> f (_akmAccessKeyId x)
{-# INLINE akmAccessKeyId #-}

-- | The status of the access key. Active means the key is valid for API calls,
-- while Inactive means it is not.
akmStatus
    :: Functor f
    => (Maybe StatusType
    -> f (Maybe StatusType))
    -> AccessKeyMetadata
    -> f AccessKeyMetadata
akmStatus f x =
    (\y -> x { _akmStatus = y })
       <$> f (_akmStatus x)
{-# INLINE akmStatus #-}

-- | The date when the access key was created.
akmCreateDate
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> AccessKeyMetadata
    -> f AccessKeyMetadata
akmCreateDate f x =
    (\y -> x { _akmCreateDate = y })
       <$> f (_akmCreateDate x)
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
gPath
    :: Functor f
    => (Text
    -> f (Text))
    -> Group
    -> f Group
gPath f x =
    (\y -> x { _gPath = y })
       <$> f (_gPath x)
{-# INLINE gPath #-}

-- | The name that identifies the group.
gGroupName
    :: Functor f
    => (Text
    -> f (Text))
    -> Group
    -> f Group
gGroupName f x =
    (\y -> x { _gGroupName = y })
       <$> f (_gGroupName x)
{-# INLINE gGroupName #-}

-- | The stable and unique string identifying the group. For more information
-- about IDs, see Identifiers for IAM Entities in the Using IAM guide.
gGroupId
    :: Functor f
    => (Text
    -> f (Text))
    -> Group
    -> f Group
gGroupId f x =
    (\y -> x { _gGroupId = y })
       <$> f (_gGroupId x)
{-# INLINE gGroupId #-}

-- | The Amazon Resource Name (ARN) specifying the group. For more information
-- about ARNs and how to use them in policies, see Identifiers for IAM
-- Entities in the Using IAM guide.
gArn
    :: Functor f
    => (Text
    -> f (Text))
    -> Group
    -> f Group
gArn f x =
    (\y -> x { _gArn = y })
       <$> f (_gArn x)
{-# INLINE gArn #-}

-- | The date when the group was created.
gCreateDate
    :: Functor f
    => (ISO8601
    -> f (ISO8601))
    -> Group
    -> f Group
gCreateDate f x =
    (\y -> x { _gCreateDate = y })
       <$> f (_gCreateDate x)
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
ipPath
    :: Functor f
    => (Text
    -> f (Text))
    -> InstanceProfile
    -> f InstanceProfile
ipPath f x =
    (\y -> x { _ipPath = y })
       <$> f (_ipPath x)
{-# INLINE ipPath #-}

-- | The name identifying the instance profile.
ipInstanceProfileName
    :: Functor f
    => (Text
    -> f (Text))
    -> InstanceProfile
    -> f InstanceProfile
ipInstanceProfileName f x =
    (\y -> x { _ipInstanceProfileName = y })
       <$> f (_ipInstanceProfileName x)
{-# INLINE ipInstanceProfileName #-}

-- | The stable and unique string identifying the instance profile. For more
-- information about IDs, see Identifiers for IAM Entities in the Using IAM
-- guide.
ipInstanceProfileId
    :: Functor f
    => (Text
    -> f (Text))
    -> InstanceProfile
    -> f InstanceProfile
ipInstanceProfileId f x =
    (\y -> x { _ipInstanceProfileId = y })
       <$> f (_ipInstanceProfileId x)
{-# INLINE ipInstanceProfileId #-}

-- | The Amazon Resource Name (ARN) specifying the instance profile. For more
-- information about ARNs and how to use them in policies, see Identifiers for
-- IAM Entities in the Using IAM guide.
ipArn
    :: Functor f
    => (Text
    -> f (Text))
    -> InstanceProfile
    -> f InstanceProfile
ipArn f x =
    (\y -> x { _ipArn = y })
       <$> f (_ipArn x)
{-# INLINE ipArn #-}

-- | The date when the instance profile was created.
ipCreateDate
    :: Functor f
    => (ISO8601
    -> f (ISO8601))
    -> InstanceProfile
    -> f InstanceProfile
ipCreateDate f x =
    (\y -> x { _ipCreateDate = y })
       <$> f (_ipCreateDate x)
{-# INLINE ipCreateDate #-}

-- | The role associated with the instance profile.
ipRoles
    :: Functor f
    => ([Role]
    -> f ([Role]))
    -> InstanceProfile
    -> f InstanceProfile
ipRoles f x =
    (\y -> x { _ipRoles = y })
       <$> f (_ipRoles x)
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
lpUserName
    :: Functor f
    => (Text
    -> f (Text))
    -> LoginProfile
    -> f LoginProfile
lpUserName f x =
    (\y -> x { _lpUserName = y })
       <$> f (_lpUserName x)
{-# INLINE lpUserName #-}

-- | The date when the password for the user was created.
lpCreateDate
    :: Functor f
    => (ISO8601
    -> f (ISO8601))
    -> LoginProfile
    -> f LoginProfile
lpCreateDate f x =
    (\y -> x { _lpCreateDate = y })
       <$> f (_lpCreateDate x)
{-# INLINE lpCreateDate #-}

-- | Specifies whether the user is required to set a new password on next
-- sign-in.
lpPasswordResetRequired
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> LoginProfile
    -> f LoginProfile
lpPasswordResetRequired f x =
    (\y -> x { _lpPasswordResetRequired = y })
       <$> f (_lpPasswordResetRequired x)
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
mfadUserName
    :: Functor f
    => (Text
    -> f (Text))
    -> MFADevice
    -> f MFADevice
mfadUserName f x =
    (\y -> x { _mfadUserName = y })
       <$> f (_mfadUserName x)
{-# INLINE mfadUserName #-}

-- | The serial number that uniquely identifies the MFA device. For virtual MFA
-- devices, the serial number is the device ARN.
mfadSerialNumber
    :: Functor f
    => (Text
    -> f (Text))
    -> MFADevice
    -> f MFADevice
mfadSerialNumber f x =
    (\y -> x { _mfadSerialNumber = y })
       <$> f (_mfadSerialNumber x)
{-# INLINE mfadSerialNumber #-}

-- | The date when the MFA device was enabled for the user.
mfadEnableDate
    :: Functor f
    => (ISO8601
    -> f (ISO8601))
    -> MFADevice
    -> f MFADevice
mfadEnableDate f x =
    (\y -> x { _mfadEnableDate = y })
       <$> f (_mfadEnableDate x)
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
ppMinimumPasswordLength
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> PasswordPolicy
    -> f PasswordPolicy
ppMinimumPasswordLength f x =
    (\y -> x { _ppMinimumPasswordLength = y })
       <$> f (_ppMinimumPasswordLength x)
{-# INLINE ppMinimumPasswordLength #-}

-- | Specifies whether to require symbols for IAM user passwords.
ppRequireSymbols
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> PasswordPolicy
    -> f PasswordPolicy
ppRequireSymbols f x =
    (\y -> x { _ppRequireSymbols = y })
       <$> f (_ppRequireSymbols x)
{-# INLINE ppRequireSymbols #-}

-- | Specifies whether to require numbers for IAM user passwords.
ppRequireNumbers
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> PasswordPolicy
    -> f PasswordPolicy
ppRequireNumbers f x =
    (\y -> x { _ppRequireNumbers = y })
       <$> f (_ppRequireNumbers x)
{-# INLINE ppRequireNumbers #-}

-- | Specifies whether to require uppercase characters for IAM user passwords.
ppRequireUppercaseCharacters
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> PasswordPolicy
    -> f PasswordPolicy
ppRequireUppercaseCharacters f x =
    (\y -> x { _ppRequireUppercaseCharacters = y })
       <$> f (_ppRequireUppercaseCharacters x)
{-# INLINE ppRequireUppercaseCharacters #-}

-- | Specifies whether to require lowercase characters for IAM user passwords.
ppRequireLowercaseCharacters
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> PasswordPolicy
    -> f PasswordPolicy
ppRequireLowercaseCharacters f x =
    (\y -> x { _ppRequireLowercaseCharacters = y })
       <$> f (_ppRequireLowercaseCharacters x)
{-# INLINE ppRequireLowercaseCharacters #-}

-- | Specifies whether IAM users are allowed to change their own password.
ppAllowUsersToChangePassword
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> PasswordPolicy
    -> f PasswordPolicy
ppAllowUsersToChangePassword f x =
    (\y -> x { _ppAllowUsersToChangePassword = y })
       <$> f (_ppAllowUsersToChangePassword x)
{-# INLINE ppAllowUsersToChangePassword #-}

-- | Specifies whether IAM users are required to change their password after a
-- specified number of days.
ppExpirePasswords
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> PasswordPolicy
    -> f PasswordPolicy
ppExpirePasswords f x =
    (\y -> x { _ppExpirePasswords = y })
       <$> f (_ppExpirePasswords x)
{-# INLINE ppExpirePasswords #-}

-- | The number of days that an IAM user password is valid.
ppMaxPasswordAge
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> PasswordPolicy
    -> f PasswordPolicy
ppMaxPasswordAge f x =
    (\y -> x { _ppMaxPasswordAge = y })
       <$> f (_ppMaxPasswordAge x)
{-# INLINE ppMaxPasswordAge #-}

-- | Specifies the number of previous passwords that IAM users are prevented
-- from reusing.
ppPasswordReusePrevention
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> PasswordPolicy
    -> f PasswordPolicy
ppPasswordReusePrevention f x =
    (\y -> x { _ppPasswordReusePrevention = y })
       <$> f (_ppPasswordReusePrevention x)
{-# INLINE ppPasswordReusePrevention #-}

-- | Specifies whether IAM users are prevented from setting a new password after
-- their password has expired.
ppHardExpiry
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> PasswordPolicy
    -> f PasswordPolicy
ppHardExpiry f x =
    (\y -> x { _ppHardExpiry = y })
       <$> f (_ppHardExpiry x)
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
rPath
    :: Functor f
    => (Text
    -> f (Text))
    -> Role
    -> f Role
rPath f x =
    (\y -> x { _rPath = y })
       <$> f (_rPath x)
{-# INLINE rPath #-}

-- | The name identifying the role.
rRoleName
    :: Functor f
    => (Text
    -> f (Text))
    -> Role
    -> f Role
rRoleName f x =
    (\y -> x { _rRoleName = y })
       <$> f (_rRoleName x)
{-# INLINE rRoleName #-}

-- | The stable and unique string identifying the role. For more information
-- about IDs, see Identifiers for IAM Entities in the Using IAM guide.
rRoleId
    :: Functor f
    => (Text
    -> f (Text))
    -> Role
    -> f Role
rRoleId f x =
    (\y -> x { _rRoleId = y })
       <$> f (_rRoleId x)
{-# INLINE rRoleId #-}

-- | The Amazon Resource Name (ARN) specifying the role. For more information
-- about ARNs and how to use them in policies, see Identifiers for IAM
-- Entities in the Using IAM guide.
rArn
    :: Functor f
    => (Text
    -> f (Text))
    -> Role
    -> f Role
rArn f x =
    (\y -> x { _rArn = y })
       <$> f (_rArn x)
{-# INLINE rArn #-}

-- | The date when the role was created.
rCreateDate
    :: Functor f
    => (ISO8601
    -> f (ISO8601))
    -> Role
    -> f Role
rCreateDate f x =
    (\y -> x { _rCreateDate = y })
       <$> f (_rCreateDate x)
{-# INLINE rCreateDate #-}

-- | The policy that grants an entity permission to assume the role. The
-- returned policy is URL-encoded according to RFC 3986. For more information
-- about RFC 3986, go to http://www.faqs.org/rfcs/rfc3986.html.
rAssumeRolePolicyDocument
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Role
    -> f Role
rAssumeRolePolicyDocument f x =
    (\y -> x { _rAssumeRolePolicyDocument = y })
       <$> f (_rAssumeRolePolicyDocument x)
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
samlpleArn
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SAMLProviderListEntry
    -> f SAMLProviderListEntry
samlpleArn f x =
    (\y -> x { _samlpleArn = y })
       <$> f (_samlpleArn x)
{-# INLINE samlpleArn #-}

-- | The expiration date and time for the SAML provider.
samlpleValidUntil
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> SAMLProviderListEntry
    -> f SAMLProviderListEntry
samlpleValidUntil f x =
    (\y -> x { _samlpleValidUntil = y })
       <$> f (_samlpleValidUntil x)
{-# INLINE samlpleValidUntil #-}

-- | The date and time when the SAML provider was created.
samlpleCreateDate
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> SAMLProviderListEntry
    -> f SAMLProviderListEntry
samlpleCreateDate f x =
    (\y -> x { _samlpleCreateDate = y })
       <$> f (_samlpleCreateDate x)
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
scServerCertificateMetadata
    :: Functor f
    => (ServerCertificateMetadata
    -> f (ServerCertificateMetadata))
    -> ServerCertificate
    -> f ServerCertificate
scServerCertificateMetadata f x =
    (\y -> x { _scServerCertificateMetadata = y })
       <$> f (_scServerCertificateMetadata x)
{-# INLINE scServerCertificateMetadata #-}

-- | The contents of the public key certificate.
scCertificateBody
    :: Functor f
    => (Text
    -> f (Text))
    -> ServerCertificate
    -> f ServerCertificate
scCertificateBody f x =
    (\y -> x { _scCertificateBody = y })
       <$> f (_scCertificateBody x)
{-# INLINE scCertificateBody #-}

-- | The contents of the public key certificate chain.
scCertificateChain
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ServerCertificate
    -> f ServerCertificate
scCertificateChain f x =
    (\y -> x { _scCertificateChain = y })
       <$> f (_scCertificateChain x)
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
scmPath
    :: Functor f
    => (Text
    -> f (Text))
    -> ServerCertificateMetadata
    -> f ServerCertificateMetadata
scmPath f x =
    (\y -> x { _scmPath = y })
       <$> f (_scmPath x)
{-# INLINE scmPath #-}

-- | The name that identifies the server certificate.
scmServerCertificateName
    :: Functor f
    => (Text
    -> f (Text))
    -> ServerCertificateMetadata
    -> f ServerCertificateMetadata
scmServerCertificateName f x =
    (\y -> x { _scmServerCertificateName = y })
       <$> f (_scmServerCertificateName x)
{-# INLINE scmServerCertificateName #-}

-- | The stable and unique string identifying the server certificate. For more
-- information about IDs, see Identifiers for IAM Entities in the Using IAM
-- guide.
scmServerCertificateId
    :: Functor f
    => (Text
    -> f (Text))
    -> ServerCertificateMetadata
    -> f ServerCertificateMetadata
scmServerCertificateId f x =
    (\y -> x { _scmServerCertificateId = y })
       <$> f (_scmServerCertificateId x)
{-# INLINE scmServerCertificateId #-}

-- | The Amazon Resource Name (ARN) specifying the server certificate. For more
-- information about ARNs and how to use them in policies, see Identifiers for
-- IAM Entities in the Using IAM guide.
scmArn
    :: Functor f
    => (Text
    -> f (Text))
    -> ServerCertificateMetadata
    -> f ServerCertificateMetadata
scmArn f x =
    (\y -> x { _scmArn = y })
       <$> f (_scmArn x)
{-# INLINE scmArn #-}

-- | The date when the server certificate was uploaded.
scmUploadDate
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> ServerCertificateMetadata
    -> f ServerCertificateMetadata
scmUploadDate f x =
    (\y -> x { _scmUploadDate = y })
       <$> f (_scmUploadDate x)
{-# INLINE scmUploadDate #-}

-- | The date on which the certificate is set to expire.
scmExpiration
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> ServerCertificateMetadata
    -> f ServerCertificateMetadata
scmExpiration f x =
    (\y -> x { _scmExpiration = y })
       <$> f (_scmExpiration x)
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
sdUserName
    :: Functor f
    => (Text
    -> f (Text))
    -> SigningCertificate
    -> f SigningCertificate
sdUserName f x =
    (\y -> x { _sdUserName = y })
       <$> f (_sdUserName x)
{-# INLINE sdUserName #-}

-- | The ID for the signing certificate.
sdCertificateId
    :: Functor f
    => (Text
    -> f (Text))
    -> SigningCertificate
    -> f SigningCertificate
sdCertificateId f x =
    (\y -> x { _sdCertificateId = y })
       <$> f (_sdCertificateId x)
{-# INLINE sdCertificateId #-}

-- | The contents of the signing certificate.
sdCertificateBody
    :: Functor f
    => (Text
    -> f (Text))
    -> SigningCertificate
    -> f SigningCertificate
sdCertificateBody f x =
    (\y -> x { _sdCertificateBody = y })
       <$> f (_sdCertificateBody x)
{-# INLINE sdCertificateBody #-}

-- | The status of the signing certificate. Active means the key is valid for
-- API calls, while Inactive means it is not.
sdStatus
    :: Functor f
    => (StatusType
    -> f (StatusType))
    -> SigningCertificate
    -> f SigningCertificate
sdStatus f x =
    (\y -> x { _sdStatus = y })
       <$> f (_sdStatus x)
{-# INLINE sdStatus #-}

-- | The date when the signing certificate was uploaded.
sdUploadDate
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> SigningCertificate
    -> f SigningCertificate
sdUploadDate f x =
    (\y -> x { _sdUploadDate = y })
       <$> f (_sdUploadDate x)
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
uPath
    :: Functor f
    => (Text
    -> f (Text))
    -> User
    -> f User
uPath f x =
    (\y -> x { _uPath = y })
       <$> f (_uPath x)
{-# INLINE uPath #-}

-- | The name identifying the user.
uUserName
    :: Functor f
    => (Text
    -> f (Text))
    -> User
    -> f User
uUserName f x =
    (\y -> x { _uUserName = y })
       <$> f (_uUserName x)
{-# INLINE uUserName #-}

-- | The stable and unique string identifying the user. For more information
-- about IDs, see Identifiers for IAM Entities in the Using IAM guide.
uUserId
    :: Functor f
    => (Text
    -> f (Text))
    -> User
    -> f User
uUserId f x =
    (\y -> x { _uUserId = y })
       <$> f (_uUserId x)
{-# INLINE uUserId #-}

-- | The Amazon Resource Name (ARN) specifying the user. For more information
-- about ARNs and how to use them in policies, see Identifiers for IAM
-- Entities in the Using IAM guide.
uArn
    :: Functor f
    => (Text
    -> f (Text))
    -> User
    -> f User
uArn f x =
    (\y -> x { _uArn = y })
       <$> f (_uArn x)
{-# INLINE uArn #-}

-- | The date when the user was created.
uCreateDate
    :: Functor f
    => (ISO8601
    -> f (ISO8601))
    -> User
    -> f User
uCreateDate f x =
    (\y -> x { _uCreateDate = y })
       <$> f (_uCreateDate x)
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
vmfadSerialNumber
    :: Functor f
    => (Text
    -> f (Text))
    -> VirtualMFADevice
    -> f VirtualMFADevice
vmfadSerialNumber f x =
    (\y -> x { _vmfadSerialNumber = y })
       <$> f (_vmfadSerialNumber x)
{-# INLINE vmfadSerialNumber #-}

-- | The Base32 seed defined as specified in RFC3548. The Base32StringSeed is
-- Base64-encoded.
vmfadBase32StringSeed
    :: Functor f
    => (Maybe ByteString
    -> f (Maybe ByteString))
    -> VirtualMFADevice
    -> f VirtualMFADevice
vmfadBase32StringSeed f x =
    (\y -> x { _vmfadBase32StringSeed = y })
       <$> f (_vmfadBase32StringSeed x)
{-# INLINE vmfadBase32StringSeed #-}

-- | A QR code PNG image that encodes
-- otpauth://totp/$virtualMFADeviceName@$AccountName? secret=$Base32String
-- where $virtualMFADeviceName is one of the create call arguments,
-- AccountName is the user name if set (accountId otherwise), and Base32String
-- is the seed in Base32 format. The Base32String is Base64-encoded.
vmfadQRCodePNG
    :: Functor f
    => (Maybe ByteString
    -> f (Maybe ByteString))
    -> VirtualMFADevice
    -> f VirtualMFADevice
vmfadQRCodePNG f x =
    (\y -> x { _vmfadQRCodePNG = y })
       <$> f (_vmfadQRCodePNG x)
{-# INLINE vmfadQRCodePNG #-}

-- | The User data type contains information about a user. This data type is
-- used as a response element in the following actions: CreateUser GetUser
-- ListUsers.
vmfadUser
    :: Functor f
    => (Maybe User
    -> f (Maybe User))
    -> VirtualMFADevice
    -> f VirtualMFADevice
vmfadUser f x =
    (\y -> x { _vmfadUser = y })
       <$> f (_vmfadUser x)
{-# INLINE vmfadUser #-}

vmfadEnableDate
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> VirtualMFADevice
    -> f VirtualMFADevice
vmfadEnableDate f x =
    (\y -> x { _vmfadEnableDate = y })
       <$> f (_vmfadEnableDate x)
{-# INLINE vmfadEnableDate #-}

instance FromXML VirtualMFADevice where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "VirtualMFADevice"
