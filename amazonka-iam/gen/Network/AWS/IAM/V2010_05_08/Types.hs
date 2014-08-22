{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.IAM.V2010_05_08.Types where

import Control.Lens.TH (makeLenses)
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
    { _akStatus :: StatusType
      -- ^ The status of the access key. Active means the key is valid for
      -- API calls, while Inactive means it is not.
    , _akSecretAccessKey :: Text
      -- ^ The secret key used to sign requests.
    , _akCreateDate :: Maybe ISO8601
      -- ^ The date when the access key was created.
    , _akUserName :: Text
      -- ^ Name of the user the key is associated with.
    , _akAccessKeyId :: Text
      -- ^ The ID for this access key.
    } deriving (Show, Generic)

instance FromXML AccessKey where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AccessKey"

-- | The AccessKey data type contains information about an AWS access key,
-- without its secret key. This data type is used as a response element in the
-- action ListAccessKeys.
data AccessKeyMetadata = AccessKeyMetadata
    { _akmStatus :: Maybe StatusType
      -- ^ The status of the access key. Active means the key is valid for
      -- API calls, while Inactive means it is not.
    , _akmCreateDate :: Maybe ISO8601
      -- ^ The date when the access key was created.
    , _akmUserName :: Maybe Text
      -- ^ Name of the user the key is associated with.
    , _akmAccessKeyId :: Maybe Text
      -- ^ The ID for this access key.
    } deriving (Show, Generic)

instance FromXML AccessKeyMetadata where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AccessKeyMetadata"

-- | The Group data type contains information about a group. This data type is
-- used as a response element in the following actions: CreateGroup GetGroup
-- ListGroups.
data Group = Group
    { _gpArn :: Text
      -- ^ The Amazon Resource Name (ARN) specifying the group. For more
      -- information about ARNs and how to use them in policies, see
      -- Identifiers for IAM Entities in the Using IAM guide.
    , _gpPath :: Text
      -- ^ Path to the group. For more information about paths, see
      -- Identifiers for IAM Entities in the Using IAM guide.
    , _gpCreateDate :: ISO8601
      -- ^ The date when the group was created.
    , _gpGroupId :: Text
      -- ^ The stable and unique string identifying the group. For more
      -- information about IDs, see Identifiers for IAM Entities in the
      -- Using IAM guide.
    , _gpGroupName :: Text
      -- ^ The name that identifies the group.
    } deriving (Show, Generic)

instance FromXML Group where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Group"

-- | The InstanceProfile data type contains information about an instance
-- profile. This data type is used as a response element in the following
-- actions: CreateInstanceProfile GetInstanceProfile ListInstanceProfiles
-- ListInstanceProfilesForRole.
data InstanceProfile = InstanceProfile
    { _ipRoles :: [Role]
      -- ^ The role associated with the instance profile.
    , _ipArn :: Text
      -- ^ The Amazon Resource Name (ARN) specifying the instance profile.
      -- For more information about ARNs and how to use them in policies,
      -- see Identifiers for IAM Entities in the Using IAM guide.
    , _ipPath :: Text
      -- ^ Path to the instance profile. For more information about paths,
      -- see Identifiers for IAM Entities in the Using IAM guide.
    , _ipCreateDate :: ISO8601
      -- ^ The date when the instance profile was created.
    , _ipInstanceProfileId :: Text
      -- ^ The stable and unique string identifying the instance profile.
      -- For more information about IDs, see Identifiers for IAM Entities
      -- in the Using IAM guide.
    , _ipInstanceProfileName :: Text
      -- ^ The name identifying the instance profile.
    } deriving (Show, Generic)

instance FromXML InstanceProfile where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "InstanceProfile"

-- | User name and password create date for the user.
data LoginProfile = LoginProfile
    { _lpCreateDate :: ISO8601
      -- ^ The date when the password for the user was created.
    , _lpUserName :: Text
      -- ^ The name of the user, which can be used for signing in to the AWS
      -- Management Console.
    , _lpPasswordResetRequired :: Maybe Bool
      -- ^ Specifies whether the user is required to set a new password on
      -- next sign-in.
    } deriving (Show, Generic)

instance FromXML LoginProfile where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LoginProfile"

-- | The MFADevice data type contains information about an MFA device. This data
-- type is used as a response element in the action ListMFADevices.
data MFADevice = MFADevice
    { _mfadUserName :: Text
      -- ^ The user with whom the MFA device is associated.
    , _mfadEnableDate :: ISO8601
      -- ^ The date when the MFA device was enabled for the user.
    , _mfadSerialNumber :: Text
      -- ^ The serial number that uniquely identifies the MFA device. For
      -- virtual MFA devices, the serial number is the device ARN.
    } deriving (Show, Generic)

instance FromXML MFADevice where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "MFADevice"

-- | The PasswordPolicy data type contains information about the account
-- password policy. This data type is used as a response element in the action
-- GetAccountPasswordPolicy.
data PasswordPolicy = PasswordPolicy
    { _ppExpirePasswords :: Maybe Bool
      -- ^ Specifies whether IAM users are required to change their password
      -- after a specified number of days.
    , _ppMinimumPasswordLength :: Maybe Integer
      -- ^ Minimum length to require for IAM user passwords.
    , _ppRequireNumbers :: Maybe Bool
      -- ^ Specifies whether to require numbers for IAM user passwords.
    , _ppPasswordReusePrevention :: Maybe Integer
      -- ^ Specifies the number of previous passwords that IAM users are
      -- prevented from reusing.
    , _ppRequireLowercaseCharacters :: Maybe Bool
      -- ^ Specifies whether to require lowercase characters for IAM user
      -- passwords.
    , _ppMaxPasswordAge :: Maybe Integer
      -- ^ The number of days that an IAM user password is valid.
    , _ppHardExpiry :: Maybe Bool
      -- ^ Specifies whether IAM users are prevented from setting a new
      -- password after their password has expired.
    , _ppRequireSymbols :: Maybe Bool
      -- ^ Specifies whether to require symbols for IAM user passwords.
    , _ppRequireUppercaseCharacters :: Maybe Bool
      -- ^ Specifies whether to require uppercase characters for IAM user
      -- passwords.
    , _ppAllowUsersToChangePassword :: Maybe Bool
      -- ^ Specifies whether IAM users are allowed to change their own
      -- password.
    } deriving (Show, Generic)

instance FromXML PasswordPolicy where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PasswordPolicy"

-- | The Role data type contains information about a role. This data type is
-- used as a response element in the following actions: CreateRole GetRole
-- ListRoles.
data Role = Role
    { _rAssumeRolePolicyDocument :: Maybe Text
      -- ^ The policy that grants an entity permission to assume the role.
      -- The returned policy is URL-encoded according to RFC 3986. For
      -- more information about RFC 3986, go to
      -- http://www.faqs.org/rfcs/rfc3986.html.
    , _rArn :: Text
      -- ^ The Amazon Resource Name (ARN) specifying the role. For more
      -- information about ARNs and how to use them in policies, see
      -- Identifiers for IAM Entities in the Using IAM guide.
    , _rPath :: Text
      -- ^ Path to the role. For more information about paths, see
      -- Identifiers for IAM Entities in the Using IAM guide.
    , _rCreateDate :: ISO8601
      -- ^ The date when the role was created.
    , _rRoleName :: Text
      -- ^ The name identifying the role.
    , _rRoleId :: Text
      -- ^ The stable and unique string identifying the role. For more
      -- information about IDs, see Identifiers for IAM Entities in the
      -- Using IAM guide.
    } deriving (Show, Generic)

instance FromXML Role where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Role"

instance ToQuery Role where
    toQuery = genericQuery def

-- | The list of SAML providers for this account.
data SAMLProviderListEntry = SAMLProviderListEntry
    { _samlpleArn :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the SAML provider.
    , _samlpleCreateDate :: Maybe ISO8601
      -- ^ The date and time when the SAML provider was created.
    , _samlpleValidUntil :: Maybe ISO8601
      -- ^ The expiration date and time for the SAML provider.
    } deriving (Show, Generic)

instance FromXML SAMLProviderListEntry where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SAMLProviderListEntry"

-- | Information about the server certificate.
data ServerCertificate = ServerCertificate
    { _seServerCertificateMetadata :: ServerCertificateMetadata
      -- ^ The meta information of the server certificate, such as its name,
      -- path, ID, and ARN.
    , _seCertificateBody :: Text
      -- ^ The contents of the public key certificate.
    , _seCertificateChain :: Maybe Text
      -- ^ The contents of the public key certificate chain.
    } deriving (Show, Generic)

instance FromXML ServerCertificate where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ServerCertificate"

-- | ServerCertificateMetadata contains information about a server certificate
-- without its certificate body, certificate chain, and private key. This data
-- type is used as a response element in the action UploadServerCertificate
-- and ListServerCertificates.
data ServerCertificateMetadata = ServerCertificateMetadata
    { _scmServerCertificateName :: Text
      -- ^ The name that identifies the server certificate.
    , _scmUploadDate :: Maybe ISO8601
      -- ^ The date when the server certificate was uploaded.
    , _scmServerCertificateId :: Text
      -- ^ The stable and unique string identifying the server certificate.
      -- For more information about IDs, see Identifiers for IAM Entities
      -- in the Using IAM guide.
    , _scmArn :: Text
      -- ^ The Amazon Resource Name (ARN) specifying the server certificate.
      -- For more information about ARNs and how to use them in policies,
      -- see Identifiers for IAM Entities in the Using IAM guide.
    , _scmPath :: Text
      -- ^ Path to the server certificate. For more information about paths,
      -- see Identifiers for IAM Entities in the Using IAM guide.
    , _scmExpiration :: Maybe ISO8601
      -- ^ The date on which the certificate is set to expire.
    } deriving (Show, Generic)

instance FromXML ServerCertificateMetadata where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ServerCertificateMetadata"

-- | The SigningCertificate data type contains information about an X.509
-- signing certificate. This data type is used as a response element in the
-- actions UploadSigningCertificate and ListSigningCertificates.
data SigningCertificate = SigningCertificate
    { _scStatus :: StatusType
      -- ^ The status of the signing certificate. Active means the key is
      -- valid for API calls, while Inactive means it is not.
    , _scUploadDate :: Maybe ISO8601
      -- ^ The date when the signing certificate was uploaded.
    , _scCertificateId :: Text
      -- ^ The ID for the signing certificate.
    , _scUserName :: Text
      -- ^ Name of the user the signing certificate is associated with.
    , _scCertificateBody :: Text
      -- ^ The contents of the signing certificate.
    } deriving (Show, Generic)

instance FromXML SigningCertificate where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SigningCertificate"

-- | The User data type contains information about a user. This data type is
-- used as a response element in the following actions: CreateUser GetUser
-- ListUsers.
data User = User
    { _uArn :: Text
      -- ^ The Amazon Resource Name (ARN) specifying the user. For more
      -- information about ARNs and how to use them in policies, see
      -- Identifiers for IAM Entities in the Using IAM guide.
    , _uPath :: Text
      -- ^ Path to the user. For more information about paths, see
      -- Identifiers for IAM Entities in the Using IAM guide.
    , _uCreateDate :: ISO8601
      -- ^ The date when the user was created.
    , _uUserName :: Text
      -- ^ The name identifying the user.
    , _uUserId :: Text
      -- ^ The stable and unique string identifying the user. For more
      -- information about IDs, see Identifiers for IAM Entities in the
      -- Using IAM guide.
    } deriving (Show, Generic)

instance FromXML User where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "User"

instance ToQuery User where
    toQuery = genericQuery def

-- | A newly created virtual MFA device.
data VirtualMFADevice = VirtualMFADevice
    { _vmfadQRCodePNG :: Maybe ByteString
      -- ^ A QR code PNG image that encodes
      -- otpauth://totp/$virtualMFADeviceName@$AccountName?
      -- secret=$Base32String where $virtualMFADeviceName is one of the
      -- create call arguments, AccountName is the user name if set
      -- (accountId otherwise), and Base32String is the seed in Base32
      -- format. The Base32String is Base64-encoded.
    , _vmfadBase32StringSeed :: Maybe ByteString
      -- ^ The Base32 seed defined as specified in RFC3548. The
      -- Base32StringSeed is Base64-encoded.
    , _vmfadUser :: Maybe User
      -- ^ The User data type contains information about a user. This data
      -- type is used as a response element in the following actions:
      -- CreateUser GetUser ListUsers.
    , _vmfadEnableDate :: Maybe ISO8601
    , _vmfadSerialNumber :: Text
      -- ^ The serial number associated with VirtualMFADevice.
    } deriving (Show, Generic)

instance FromXML VirtualMFADevice where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "VirtualMFADevice"

-- Newtypes

-- Products
makeLenses ''AccessKey
makeLenses ''AccessKeyMetadata
makeLenses ''Group
makeLenses ''InstanceProfile
makeLenses ''LoginProfile
makeLenses ''MFADevice
makeLenses ''PasswordPolicy
makeLenses ''Role
makeLenses ''SAMLProviderListEntry
makeLenses ''ServerCertificate
makeLenses ''ServerCertificateMetadata
makeLenses ''SigningCertificate
makeLenses ''User
makeLenses ''VirtualMFADevice
