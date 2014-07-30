{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

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

import Control.Applicative
import Control.Exception      (Exception)
import Data.Default
import Data.Tagged
import Data.Text              (Text)
import Data.Typeable
import GHC.Generics
import Network.AWS.Data
import Network.AWS.Signing.V4
import Network.AWS.Types      hiding (Error, Endpoint, Region)
import Network.HTTP.Client    (HttpException)

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

    service = Service
        { _svcEndpoint = Regional
        , _svcPrefix   = "iam"
        , _svcVersion  = "2010-05-08"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er IAM)
deriving instance Generic (Er IAM)

instance AWSError (Er IAM) where
    awsError = const "IAMError"

instance ServiceError (Er IAM) where
    serviceError    = IAMService
    clientError     = IAMClient
    serializerError = IAMSerializer

instance Exception (Er IAM)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlNamespace = Just "https://iam.amazonaws.com/doc/2010-05-08/"
    }

-- | The format (MIME type) of the credential report.
data ReportFormatType
    = ReportFormatTypeTextCsv -- ^ text/csv
      deriving (Eq, Show, Generic)

instance FromText ReportFormatType where
    parser = match "text/csv" ReportFormatTypeTextCsv

instance ToText ReportFormatType where
    toText ReportFormatTypeTextCsv = "text/csv"

instance ToByteString ReportFormatType where
    toBS ReportFormatTypeTextCsv = "text/csv"

instance FromXML ReportFormatType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReportFormatType"

-- | Information about the state of a credential report.
data ReportStateType
    = ReportStateTypeComplete -- ^ COMPLETE
    | ReportStateTypeInprogress -- ^ INPROGRESS
    | ReportStateTypeStarted -- ^ STARTED
      deriving (Eq, Show, Generic)

instance FromText ReportStateType where
    parser = match "COMPLETE" ReportStateTypeComplete
         <|> match "INPROGRESS" ReportStateTypeInprogress
         <|> match "STARTED" ReportStateTypeStarted

instance ToText ReportStateType where
    toText ReportStateTypeComplete = "COMPLETE"
    toText ReportStateTypeInprogress = "INPROGRESS"
    toText ReportStateTypeStarted = "STARTED"

instance ToByteString ReportStateType where
    toBS ReportStateTypeComplete = "COMPLETE"
    toBS ReportStateTypeInprogress = "INPROGRESS"
    toBS ReportStateTypeStarted = "STARTED"

instance FromXML ReportStateType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReportStateType"

-- | The status (unassigned or assigned) of the devices to list. If you do not
-- specify an AssignmentStatus, the action defaults to Any which lists both
-- assigned and unassigned virtual MFA devices.
data assignmentStatusType
    = assignmentStatusTypeAny -- ^ Any
    | assignmentStatusTypeAssigned -- ^ Assigned
    | assignmentStatusTypeUnassigned -- ^ Unassigned
      deriving (Eq, Show, Generic)

instance FromText assignmentStatusType where
    parser = match "Any" assignmentStatusTypeAny
         <|> match "Assigned" assignmentStatusTypeAssigned
         <|> match "Unassigned" assignmentStatusTypeUnassigned

instance ToText assignmentStatusType where
    toText assignmentStatusTypeAny = "Any"
    toText assignmentStatusTypeAssigned = "Assigned"
    toText assignmentStatusTypeUnassigned = "Unassigned"

instance ToByteString assignmentStatusType where
    toBS assignmentStatusTypeAny = "Any"
    toBS assignmentStatusTypeAssigned = "Assigned"
    toBS assignmentStatusTypeUnassigned = "Unassigned"

instance ToQuery assignmentStatusType where
    toQuery = genericToQuery def

-- | The status of the access key. Active means the key is valid for API calls,
-- while Inactive means it is not.
data statusType
    = statusTypeActive -- ^ Active
    | statusTypeInactive -- ^ Inactive
      deriving (Eq, Show, Generic)

instance FromText statusType where
    parser = match "Active" statusTypeActive
         <|> match "Inactive" statusTypeInactive

instance ToText statusType where
    toText statusTypeActive = "Active"
    toText statusTypeInactive = "Inactive"

instance ToByteString statusType where
    toBS statusTypeActive = "Active"
    toBS statusTypeInactive = "Inactive"

instance FromXML statusType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "statusType"

instance ToQuery statusType where
    toQuery = genericToQuery def

data summaryKeyType
    = summaryKeyTypeAccessKeysPerUserQuota -- ^ AccessKeysPerUserQuota
    | summaryKeyTypeAccountMFAEnabled -- ^ AccountMFAEnabled
    | summaryKeyTypeGroupPolicySizeQuota -- ^ GroupPolicySizeQuota
    | summaryKeyTypeGroups -- ^ Groups
    | summaryKeyTypeGroupsPerUserQuota -- ^ GroupsPerUserQuota
    | summaryKeyTypeGroupsQuota -- ^ GroupsQuota
    | summaryKeyTypeMFADevices -- ^ MFADevices
    | summaryKeyTypeMFADevicesInUse -- ^ MFADevicesInUse
    | summaryKeyTypeServerCertificates -- ^ ServerCertificates
    | summaryKeyTypeServerCertificatesQuota -- ^ ServerCertificatesQuota
    | summaryKeyTypeSigningCertificatesPerUserQuota -- ^ SigningCertificatesPerUserQuota
    | summaryKeyTypeUserPolicySizeQuota -- ^ UserPolicySizeQuota
    | summaryKeyTypeUsers -- ^ Users
    | summaryKeyTypeUsersQuota -- ^ UsersQuota
      deriving (Eq, Show, Generic)

instance FromText summaryKeyType where
    parser = match "AccessKeysPerUserQuota" summaryKeyTypeAccessKeysPerUserQuota
         <|> match "AccountMFAEnabled" summaryKeyTypeAccountMFAEnabled
         <|> match "GroupPolicySizeQuota" summaryKeyTypeGroupPolicySizeQuota
         <|> match "Groups" summaryKeyTypeGroups
         <|> match "GroupsPerUserQuota" summaryKeyTypeGroupsPerUserQuota
         <|> match "GroupsQuota" summaryKeyTypeGroupsQuota
         <|> match "MFADevices" summaryKeyTypeMFADevices
         <|> match "MFADevicesInUse" summaryKeyTypeMFADevicesInUse
         <|> match "ServerCertificates" summaryKeyTypeServerCertificates
         <|> match "ServerCertificatesQuota" summaryKeyTypeServerCertificatesQuota
         <|> match "SigningCertificatesPerUserQuota" summaryKeyTypeSigningCertificatesPerUserQuota
         <|> match "UserPolicySizeQuota" summaryKeyTypeUserPolicySizeQuota
         <|> match "Users" summaryKeyTypeUsers
         <|> match "UsersQuota" summaryKeyTypeUsersQuota

instance ToText summaryKeyType where
    toText summaryKeyTypeAccessKeysPerUserQuota = "AccessKeysPerUserQuota"
    toText summaryKeyTypeAccountMFAEnabled = "AccountMFAEnabled"
    toText summaryKeyTypeGroupPolicySizeQuota = "GroupPolicySizeQuota"
    toText summaryKeyTypeGroups = "Groups"
    toText summaryKeyTypeGroupsPerUserQuota = "GroupsPerUserQuota"
    toText summaryKeyTypeGroupsQuota = "GroupsQuota"
    toText summaryKeyTypeMFADevices = "MFADevices"
    toText summaryKeyTypeMFADevicesInUse = "MFADevicesInUse"
    toText summaryKeyTypeServerCertificates = "ServerCertificates"
    toText summaryKeyTypeServerCertificatesQuota = "ServerCertificatesQuota"
    toText summaryKeyTypeSigningCertificatesPerUserQuota = "SigningCertificatesPerUserQuota"
    toText summaryKeyTypeUserPolicySizeQuota = "UserPolicySizeQuota"
    toText summaryKeyTypeUsers = "Users"
    toText summaryKeyTypeUsersQuota = "UsersQuota"

instance ToByteString summaryKeyType where
    toBS summaryKeyTypeAccessKeysPerUserQuota = "AccessKeysPerUserQuota"
    toBS summaryKeyTypeAccountMFAEnabled = "AccountMFAEnabled"
    toBS summaryKeyTypeGroupPolicySizeQuota = "GroupPolicySizeQuota"
    toBS summaryKeyTypeGroups = "Groups"
    toBS summaryKeyTypeGroupsPerUserQuota = "GroupsPerUserQuota"
    toBS summaryKeyTypeGroupsQuota = "GroupsQuota"
    toBS summaryKeyTypeMFADevices = "MFADevices"
    toBS summaryKeyTypeMFADevicesInUse = "MFADevicesInUse"
    toBS summaryKeyTypeServerCertificates = "ServerCertificates"
    toBS summaryKeyTypeServerCertificatesQuota = "ServerCertificatesQuota"
    toBS summaryKeyTypeSigningCertificatesPerUserQuota = "SigningCertificatesPerUserQuota"
    toBS summaryKeyTypeUserPolicySizeQuota = "UserPolicySizeQuota"
    toBS summaryKeyTypeUsers = "Users"
    toBS summaryKeyTypeUsersQuota = "UsersQuota"

instance FromXML summaryKeyType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "summaryKeyType"

-- | Information about the access key.
data AccessKey = AccessKey
    { _akStatus :: statusType
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
    } deriving (Generic)

instance FromXML AccessKey where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AccessKey"

-- | The AccessKey data type contains information about an AWS access key,
-- without its secret key. This data type is used as a response element in the
-- action ListAccessKeys.
data AccessKeyMetadata = AccessKeyMetadata
    { _akmStatus :: Maybe statusType
      -- ^ The status of the access key. Active means the key is valid for
      -- API calls, while Inactive means it is not.
    , _akmCreateDate :: Maybe ISO8601
      -- ^ The date when the access key was created.
    , _akmUserName :: Maybe Text
      -- ^ Name of the user the key is associated with.
    , _akmAccessKeyId :: Maybe Text
      -- ^ The ID for this access key.
    } deriving (Generic)

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
    } deriving (Generic)

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
    } deriving (Generic)

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
    } deriving (Generic)

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
    } deriving (Generic)

instance FromXML MFADevice where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "MFADevice"

-- | The PasswordPolicy data type contains information about the account
-- password policy. This data type is used as a response element in the action
-- GetAccountPasswordPolicy.
data PasswordPolicy = PasswordPolicy
    { _ppyExpirePasswords :: Maybe Bool
      -- ^ Specifies whether IAM users are required to change their password
      -- after a specified number of days.
    , _ppyMinimumPasswordLength :: Maybe Integer
      -- ^ Minimum length to require for IAM user passwords.
    , _ppyRequireNumbers :: Maybe Bool
      -- ^ Specifies whether to require numbers for IAM user passwords.
    , _ppyPasswordReusePrevention :: Maybe Integer
      -- ^ Specifies the number of previous passwords that IAM users are
      -- prevented from reusing.
    , _ppyRequireLowercaseCharacters :: Maybe Bool
      -- ^ Specifies whether to require lowercase characters for IAM user
      -- passwords.
    , _ppyMaxPasswordAge :: Maybe Integer
      -- ^ The number of days that an IAM user password is valid.
    , _ppyHardExpiry :: Maybe Bool
      -- ^ Specifies whether IAM users are prevented from setting a new
      -- password after their password has expired.
    , _ppyRequireSymbols :: Maybe Bool
      -- ^ Specifies whether to require symbols for IAM user passwords.
    , _ppyRequireUppercaseCharacters :: Maybe Bool
      -- ^ Specifies whether to require uppercase characters for IAM user
      -- passwords.
    , _ppyAllowUsersToChangePassword :: Maybe Bool
      -- ^ Specifies whether IAM users are allowed to change their own
      -- password.
    } deriving (Generic)

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
    } deriving (Generic)

instance FromXML Role where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Role"

instance ToQuery Role where
    toQuery = genericToQuery def

-- | The list of SAML providers for this account.
data SAMLProviderListEntry = SAMLProviderListEntry
    { _samlpleArn :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the SAML provider.
    , _samlpleCreateDate :: Maybe ISO8601
      -- ^ The date and time when the SAML provider was created.
    , _samlpleValidUntil :: Maybe ISO8601
      -- ^ The expiration date and time for the SAML provider.
    } deriving (Generic)

instance FromXML SAMLProviderListEntry where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SAMLProviderListEntry"

-- | Information about the server certificate.
data ServerCertificate = ServerCertificate
    { _skServerCertificateMetadata :: ServerCertificateMetadata
      -- ^ The meta information of the server certificate, such as its name,
      -- path, ID, and ARN.
    , _skCertificateBody :: Text
      -- ^ The contents of the public key certificate.
    , _skCertificateChain :: Maybe Text
      -- ^ The contents of the public key certificate chain.
    } deriving (Generic)

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
    } deriving (Generic)

instance FromXML ServerCertificateMetadata where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ServerCertificateMetadata"

-- | The SigningCertificate data type contains information about an X.509
-- signing certificate. This data type is used as a response element in the
-- actions UploadSigningCertificate and ListSigningCertificates.
data SigningCertificate = SigningCertificate
    { _scStatus :: statusType
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
    } deriving (Generic)

instance FromXML SigningCertificate where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SigningCertificate"

-- | The User data type contains information about a user. This data type is
-- used as a response element in the following actions: CreateUser GetUser
-- ListUsers.
data User = User
    { _urArn :: Text
      -- ^ The Amazon Resource Name (ARN) specifying the user. For more
      -- information about ARNs and how to use them in policies, see
      -- Identifiers for IAM Entities in the Using IAM guide.
    , _urPath :: Text
      -- ^ Path to the user. For more information about paths, see
      -- Identifiers for IAM Entities in the Using IAM guide.
    , _urCreateDate :: ISO8601
      -- ^ The date when the user was created.
    , _urUserName :: Text
      -- ^ The name identifying the user.
    , _urUserId :: Text
      -- ^ The stable and unique string identifying the user. For more
      -- information about IDs, see Identifiers for IAM Entities in the
      -- Using IAM guide.
    } deriving (Generic)

instance FromXML User where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "User"

instance ToQuery User where
    toQuery = genericToQuery def

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
    } deriving (Generic)

instance FromXML VirtualMFADevice where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "VirtualMFADevice"
