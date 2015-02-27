{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.IAM.Types
    (
    -- * Service
      IAM
    -- ** Error
    , RESTError
    -- ** XML
    , ns

    -- * PolicyRole
    , PolicyRole
    , policyRole
    , prRoleName

    -- * AssignmentStatusType
    , AssignmentStatusType (..)

    -- * PasswordPolicy
    , PasswordPolicy
    , passwordPolicy
    , ppAllowUsersToChangePassword
    , ppExpirePasswords
    , ppHardExpiry
    , ppMaxPasswordAge
    , ppMinimumPasswordLength
    , ppPasswordReusePrevention
    , ppRequireLowercaseCharacters
    , ppRequireNumbers
    , ppRequireSymbols
    , ppRequireUppercaseCharacters

    -- * Group
    , Group
    , group
    , gArn
    , gCreateDate
    , gGroupId
    , gGroupName
    , gPath

    -- * AttachedPolicy
    , AttachedPolicy
    , attachedPolicy
    , apPolicyArn
    , apPolicyName

    -- * MFADevice
    , MFADevice
    , mfadevice
    , mfadEnableDate
    , mfadSerialNumber
    , mfadUserName

    -- * PolicyVersion
    , PolicyVersion
    , policyVersion
    , pvCreateDate
    , pvDocument
    , pvIsDefaultVersion
    , pvVersionId

    -- * InstanceProfile
    , InstanceProfile
    , instanceProfile
    , ipArn
    , ipCreateDate
    , ipInstanceProfileId
    , ipInstanceProfileName
    , ipPath
    , ipRoles

    -- * RoleDetail
    , RoleDetail
    , roleDetail
    , rdArn
    , rdAssumeRolePolicyDocument
    , rdCreateDate
    , rdInstanceProfileList
    , rdPath
    , rdRoleId
    , rdRoleName
    , rdRolePolicyList

    -- * ReportFormatType
    , ReportFormatType (..)

    -- * ServerCertificateMetadata
    , ServerCertificateMetadata
    , serverCertificateMetadata
    , scmArn
    , scmExpiration
    , scmPath
    , scmServerCertificateId
    , scmServerCertificateName
    , scmUploadDate

    -- * OpenIDConnectProviderListEntry
    , OpenIDConnectProviderListEntry
    , openIDConnectProviderListEntry
    , oidcpleArn

    -- * LoginProfile
    , LoginProfile
    , loginProfile
    , lpCreateDate
    , lpPasswordResetRequired
    , lpUserName

    -- * EntityType
    , EntityType (..)

    -- * SummaryKeyType
    , SummaryKeyType (..)

    -- * GroupDetail
    , GroupDetail
    , groupDetail
    , gdArn
    , gdCreateDate
    , gdGroupId
    , gdGroupName
    , gdGroupPolicyList
    , gdPath

    -- * ReportStateType
    , ReportStateType (..)

    -- * User
    , User
    , user
    , uArn
    , uCreateDate
    , uPasswordLastUsed
    , uPath
    , uUserId
    , uUserName

    -- * PolicyDetail
    , PolicyDetail
    , policyDetail
    , pdPolicyDocument
    , pdPolicyName

    -- * StatusType
    , StatusType (..)

    -- * SAMLProviderListEntry
    , SAMLProviderListEntry
    , samlproviderListEntry
    , samlpleArn
    , samlpleCreateDate
    , samlpleValidUntil

    -- * Role
    , Role
    , role
    , rArn
    , rAssumeRolePolicyDocument
    , rCreateDate
    , rPath
    , rRoleId
    , rRoleName

    -- * PolicyGroup
    , PolicyGroup
    , policyGroup
    , pgGroupName

    -- * PolicyScopeType
    , PolicyScopeType (..)

    -- * UserDetail
    , UserDetail
    , userDetail
    , udArn
    , udCreateDate
    , udGroupList
    , udPath
    , udUserId
    , udUserName
    , udUserPolicyList

    -- * Policy
    , Policy
    , policy
    , pArn
    , pAttachmentCount
    , pCreateDate
    , pDefaultVersionId
    , pDescription
    , pIsAttachable
    , pPath
    , pPolicyId
    , pPolicyName
    , pUpdateDate

    -- * ServerCertificate
    , ServerCertificate
    , serverCertificate
    , scCertificateBody
    , scCertificateChain
    , scServerCertificateMetadata

    -- * AccessKey
    , AccessKey
    , accessKey
    , akAccessKeyId
    , akCreateDate
    , akSecretAccessKey
    , akStatus
    , akUserName

    -- * VirtualMFADevice
    , VirtualMFADevice
    , virtualMFADevice
    , vmfadBase32StringSeed
    , vmfadEnableDate
    , vmfadQRCodePNG
    , vmfadSerialNumber
    , vmfadUser

    -- * SigningCertificate
    , SigningCertificate
    , signingCertificate
    , sc1CertificateBody
    , sc1CertificateId
    , sc1Status
    , sc1UploadDate
    , sc1UserName

    -- * AccessKeyMetadata
    , AccessKeyMetadata
    , accessKeyMetadata
    , akmAccessKeyId
    , akmCreateDate
    , akmStatus
    , akmUserName

    -- * PolicyUser
    , PolicyUser
    , policyUser
    , puUserName
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing
import qualified GHC.Exts

-- | Version @2010-05-08@ of the Amazon Identity and Access Management service.
data IAM

instance AWSService IAM where
    type Sg IAM = V4
    type Er IAM = RESTError

    service = service'
      where
        service' :: Service IAM
        service' = Service
            { _svcAbbrev       = "IAM"
            , _svcPrefix       = "iam"
            , _svcVersion      = "2010-05-08"
            , _svcTargetPrefix = Nothing
            , _svcJSONVersion  = Nothing
            , _svcHandle       = handle
            , _svcRetry        = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError RESTError)
        handle = restError statusSuccess service'

        retry :: Retry IAM
        retry = Exponential
            { _retryBase     = 0.05
            , _retryGrowth   = 2
            , _retryAttempts = 5
            , _retryCheck    = check
            }

        check :: Status
              -> RESTError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e)
            | s == 400 && "Throttling" == e = True -- Throttling
            | s == 500  = True -- General Server Error
            | s == 509  = True -- Limit Exceeded
            | s == 503  = True -- Service Unavailable
            | otherwise = False

ns :: Text
ns = "https://iam.amazonaws.com/doc/2010-05-08/"
{-# INLINE ns #-}

newtype PolicyRole = PolicyRole
    { _prRoleName :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'PolicyRole' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prRoleName' @::@ 'Maybe' 'Text'
--
policyRole :: PolicyRole
policyRole = PolicyRole
    { _prRoleName = Nothing
    }

-- | The name (friendly name, not ARN) identifying the role.
prRoleName :: Lens' PolicyRole (Maybe Text)
prRoleName = lens _prRoleName (\s a -> s { _prRoleName = a })

instance FromXML PolicyRole where
    parseXML x = PolicyRole
        <$> x .@? "RoleName"

instance ToQuery PolicyRole where
    toQuery PolicyRole{..} = mconcat
        [ "RoleName" =? _prRoleName
        ]

data AssignmentStatusType
    = Any        -- ^ Any
    | Assigned   -- ^ Assigned
    | Unassigned -- ^ Unassigned
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable AssignmentStatusType

instance FromText AssignmentStatusType where
    parser = takeLowerText >>= \case
        "any"        -> pure Any
        "assigned"   -> pure Assigned
        "unassigned" -> pure Unassigned
        e            -> fail $
            "Failure parsing AssignmentStatusType from " ++ show e

instance ToText AssignmentStatusType where
    toText = \case
        Any        -> "Any"
        Assigned   -> "Assigned"
        Unassigned -> "Unassigned"

instance ToByteString AssignmentStatusType
instance ToHeader     AssignmentStatusType
instance ToQuery      AssignmentStatusType

instance FromXML AssignmentStatusType where
    parseXML = parseXMLText "AssignmentStatusType"

data PasswordPolicy = PasswordPolicy
    { _ppAllowUsersToChangePassword :: Maybe Bool
    , _ppExpirePasswords            :: Maybe Bool
    , _ppHardExpiry                 :: Maybe Bool
    , _ppMaxPasswordAge             :: Maybe Nat
    , _ppMinimumPasswordLength      :: Maybe Nat
    , _ppPasswordReusePrevention    :: Maybe Nat
    , _ppRequireLowercaseCharacters :: Maybe Bool
    , _ppRequireNumbers             :: Maybe Bool
    , _ppRequireSymbols             :: Maybe Bool
    , _ppRequireUppercaseCharacters :: Maybe Bool
    } deriving (Eq, Ord, Read, Show)

-- | 'PasswordPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ppAllowUsersToChangePassword' @::@ 'Maybe' 'Bool'
--
-- * 'ppExpirePasswords' @::@ 'Maybe' 'Bool'
--
-- * 'ppHardExpiry' @::@ 'Maybe' 'Bool'
--
-- * 'ppMaxPasswordAge' @::@ 'Maybe' 'Natural'
--
-- * 'ppMinimumPasswordLength' @::@ 'Maybe' 'Natural'
--
-- * 'ppPasswordReusePrevention' @::@ 'Maybe' 'Natural'
--
-- * 'ppRequireLowercaseCharacters' @::@ 'Maybe' 'Bool'
--
-- * 'ppRequireNumbers' @::@ 'Maybe' 'Bool'
--
-- * 'ppRequireSymbols' @::@ 'Maybe' 'Bool'
--
-- * 'ppRequireUppercaseCharacters' @::@ 'Maybe' 'Bool'
--
passwordPolicy :: PasswordPolicy
passwordPolicy = PasswordPolicy
    { _ppMinimumPasswordLength      = Nothing
    , _ppRequireSymbols             = Nothing
    , _ppRequireNumbers             = Nothing
    , _ppRequireUppercaseCharacters = Nothing
    , _ppRequireLowercaseCharacters = Nothing
    , _ppAllowUsersToChangePassword = Nothing
    , _ppExpirePasswords            = Nothing
    , _ppMaxPasswordAge             = Nothing
    , _ppPasswordReusePrevention    = Nothing
    , _ppHardExpiry                 = Nothing
    }

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

-- | Specifies whether IAM users are prevented from setting a new password after
-- their password has expired.
ppHardExpiry :: Lens' PasswordPolicy (Maybe Bool)
ppHardExpiry = lens _ppHardExpiry (\s a -> s { _ppHardExpiry = a })

-- | The number of days that an IAM user password is valid.
ppMaxPasswordAge :: Lens' PasswordPolicy (Maybe Natural)
ppMaxPasswordAge = lens _ppMaxPasswordAge (\s a -> s { _ppMaxPasswordAge = a }) . mapping _Nat

-- | Minimum length to require for IAM user passwords.
ppMinimumPasswordLength :: Lens' PasswordPolicy (Maybe Natural)
ppMinimumPasswordLength =
    lens _ppMinimumPasswordLength (\s a -> s { _ppMinimumPasswordLength = a })
        . mapping _Nat

-- | Specifies the number of previous passwords that IAM users are prevented from
-- reusing.
ppPasswordReusePrevention :: Lens' PasswordPolicy (Maybe Natural)
ppPasswordReusePrevention =
    lens _ppPasswordReusePrevention
        (\s a -> s { _ppPasswordReusePrevention = a })
            . mapping _Nat

-- | Specifies whether to require lowercase characters for IAM user passwords.
ppRequireLowercaseCharacters :: Lens' PasswordPolicy (Maybe Bool)
ppRequireLowercaseCharacters =
    lens _ppRequireLowercaseCharacters
        (\s a -> s { _ppRequireLowercaseCharacters = a })

-- | Specifies whether to require numbers for IAM user passwords.
ppRequireNumbers :: Lens' PasswordPolicy (Maybe Bool)
ppRequireNumbers = lens _ppRequireNumbers (\s a -> s { _ppRequireNumbers = a })

-- | Specifies whether to require symbols for IAM user passwords.
ppRequireSymbols :: Lens' PasswordPolicy (Maybe Bool)
ppRequireSymbols = lens _ppRequireSymbols (\s a -> s { _ppRequireSymbols = a })

-- | Specifies whether to require uppercase characters for IAM user passwords.
ppRequireUppercaseCharacters :: Lens' PasswordPolicy (Maybe Bool)
ppRequireUppercaseCharacters =
    lens _ppRequireUppercaseCharacters
        (\s a -> s { _ppRequireUppercaseCharacters = a })

instance FromXML PasswordPolicy where
    parseXML x = PasswordPolicy
        <$> x .@? "AllowUsersToChangePassword"
        <*> x .@? "ExpirePasswords"
        <*> x .@? "HardExpiry"
        <*> x .@? "MaxPasswordAge"
        <*> x .@? "MinimumPasswordLength"
        <*> x .@? "PasswordReusePrevention"
        <*> x .@? "RequireLowercaseCharacters"
        <*> x .@? "RequireNumbers"
        <*> x .@? "RequireSymbols"
        <*> x .@? "RequireUppercaseCharacters"

instance ToQuery PasswordPolicy where
    toQuery PasswordPolicy{..} = mconcat
        [ "AllowUsersToChangePassword" =? _ppAllowUsersToChangePassword
        , "ExpirePasswords"            =? _ppExpirePasswords
        , "HardExpiry"                 =? _ppHardExpiry
        , "MaxPasswordAge"             =? _ppMaxPasswordAge
        , "MinimumPasswordLength"      =? _ppMinimumPasswordLength
        , "PasswordReusePrevention"    =? _ppPasswordReusePrevention
        , "RequireLowercaseCharacters" =? _ppRequireLowercaseCharacters
        , "RequireNumbers"             =? _ppRequireNumbers
        , "RequireSymbols"             =? _ppRequireSymbols
        , "RequireUppercaseCharacters" =? _ppRequireUppercaseCharacters
        ]

data Group = Group
    { _gArn        :: Text
    , _gCreateDate :: ISO8601
    , _gGroupId    :: Text
    , _gGroupName  :: Text
    , _gPath       :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'Group' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gArn' @::@ 'Text'
--
-- * 'gCreateDate' @::@ 'UTCTime'
--
-- * 'gGroupId' @::@ 'Text'
--
-- * 'gGroupName' @::@ 'Text'
--
-- * 'gPath' @::@ 'Text'
--
group :: Text -- ^ 'gPath'
      -> Text -- ^ 'gGroupName'
      -> Text -- ^ 'gGroupId'
      -> Text -- ^ 'gArn'
      -> UTCTime -- ^ 'gCreateDate'
      -> Group
group p1 p2 p3 p4 p5 = Group
    { _gPath       = p1
    , _gGroupName  = p2
    , _gGroupId    = p3
    , _gArn        = p4
    , _gCreateDate = withIso _Time (const id) p5
    }

-- | The Amazon Resource Name (ARN) specifying the group. For more information
-- about ARNs and how to use them in policies, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /UsingIAM/ guide.
gArn :: Lens' Group Text
gArn = lens _gArn (\s a -> s { _gArn = a })

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the group was created.
gCreateDate :: Lens' Group UTCTime
gCreateDate = lens _gCreateDate (\s a -> s { _gCreateDate = a }) . _Time

-- | The stable and unique string identifying the group. For more information
-- about IDs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
gGroupId :: Lens' Group Text
gGroupId = lens _gGroupId (\s a -> s { _gGroupId = a })

-- | The friendly name that identifies the group.
gGroupName :: Lens' Group Text
gGroupName = lens _gGroupName (\s a -> s { _gGroupName = a })

-- | The path to the group. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
gPath :: Lens' Group Text
gPath = lens _gPath (\s a -> s { _gPath = a })

instance FromXML Group where
    parseXML x = Group
        <$> x .@  "Arn"
        <*> x .@  "CreateDate"
        <*> x .@  "GroupId"
        <*> x .@  "GroupName"
        <*> x .@  "Path"

instance ToQuery Group where
    toQuery Group{..} = mconcat
        [ "Arn"        =? _gArn
        , "CreateDate" =? _gCreateDate
        , "GroupId"    =? _gGroupId
        , "GroupName"  =? _gGroupName
        , "Path"       =? _gPath
        ]

data AttachedPolicy = AttachedPolicy
    { _apPolicyArn  :: Maybe Text
    , _apPolicyName :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'AttachedPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'apPolicyArn' @::@ 'Maybe' 'Text'
--
-- * 'apPolicyName' @::@ 'Maybe' 'Text'
--
attachedPolicy :: AttachedPolicy
attachedPolicy = AttachedPolicy
    { _apPolicyName = Nothing
    , _apPolicyArn  = Nothing
    }

apPolicyArn :: Lens' AttachedPolicy (Maybe Text)
apPolicyArn = lens _apPolicyArn (\s a -> s { _apPolicyArn = a })

-- | The friendly name of the attached policy.
apPolicyName :: Lens' AttachedPolicy (Maybe Text)
apPolicyName = lens _apPolicyName (\s a -> s { _apPolicyName = a })

instance FromXML AttachedPolicy where
    parseXML x = AttachedPolicy
        <$> x .@? "PolicyArn"
        <*> x .@? "PolicyName"

instance ToQuery AttachedPolicy where
    toQuery AttachedPolicy{..} = mconcat
        [ "PolicyArn"  =? _apPolicyArn
        , "PolicyName" =? _apPolicyName
        ]

data MFADevice = MFADevice
    { _mfadEnableDate   :: ISO8601
    , _mfadSerialNumber :: Text
    , _mfadUserName     :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'MFADevice' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mfadEnableDate' @::@ 'UTCTime'
--
-- * 'mfadSerialNumber' @::@ 'Text'
--
-- * 'mfadUserName' @::@ 'Text'
--
mfadevice :: Text -- ^ 'mfadUserName'
          -> Text -- ^ 'mfadSerialNumber'
          -> UTCTime -- ^ 'mfadEnableDate'
          -> MFADevice
mfadevice p1 p2 p3 = MFADevice
    { _mfadUserName     = p1
    , _mfadSerialNumber = p2
    , _mfadEnableDate   = withIso _Time (const id) p3
    }

-- | The date when the MFA device was enabled for the user.
mfadEnableDate :: Lens' MFADevice UTCTime
mfadEnableDate = lens _mfadEnableDate (\s a -> s { _mfadEnableDate = a }) . _Time

-- | The serial number that uniquely identifies the MFA device. For virtual MFA
-- devices, the serial number is the device ARN.
mfadSerialNumber :: Lens' MFADevice Text
mfadSerialNumber = lens _mfadSerialNumber (\s a -> s { _mfadSerialNumber = a })

-- | The user with whom the MFA device is associated.
mfadUserName :: Lens' MFADevice Text
mfadUserName = lens _mfadUserName (\s a -> s { _mfadUserName = a })

instance FromXML MFADevice where
    parseXML x = MFADevice
        <$> x .@  "EnableDate"
        <*> x .@  "SerialNumber"
        <*> x .@  "UserName"

instance ToQuery MFADevice where
    toQuery MFADevice{..} = mconcat
        [ "EnableDate"   =? _mfadEnableDate
        , "SerialNumber" =? _mfadSerialNumber
        , "UserName"     =? _mfadUserName
        ]

data PolicyVersion = PolicyVersion
    { _pvCreateDate       :: Maybe ISO8601
    , _pvDocument         :: Maybe Text
    , _pvIsDefaultVersion :: Maybe Bool
    , _pvVersionId        :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'PolicyVersion' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pvCreateDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'pvDocument' @::@ 'Maybe' 'Text'
--
-- * 'pvIsDefaultVersion' @::@ 'Maybe' 'Bool'
--
-- * 'pvVersionId' @::@ 'Maybe' 'Text'
--
policyVersion :: PolicyVersion
policyVersion = PolicyVersion
    { _pvDocument         = Nothing
    , _pvVersionId        = Nothing
    , _pvIsDefaultVersion = Nothing
    , _pvCreateDate       = Nothing
    }

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the policy version was
-- created.
pvCreateDate :: Lens' PolicyVersion (Maybe UTCTime)
pvCreateDate = lens _pvCreateDate (\s a -> s { _pvCreateDate = a }) . mapping _Time

-- | The policy document.
--
-- The policy document is returned in the response to the 'GetPolicyVersion'
-- operation. It is not included in the response to the 'ListPolicyVersions'
-- operation.
pvDocument :: Lens' PolicyVersion (Maybe Text)
pvDocument = lens _pvDocument (\s a -> s { _pvDocument = a })

-- | Specifies whether the policy version is set as the policy's default version.
pvIsDefaultVersion :: Lens' PolicyVersion (Maybe Bool)
pvIsDefaultVersion =
    lens _pvIsDefaultVersion (\s a -> s { _pvIsDefaultVersion = a })

-- | The identifier for the policy version.
--
-- Policy version identifiers always begin with 'v' (always lowercase). When a
-- policy is created, the first policy version is 'v1'.
pvVersionId :: Lens' PolicyVersion (Maybe Text)
pvVersionId = lens _pvVersionId (\s a -> s { _pvVersionId = a })

instance FromXML PolicyVersion where
    parseXML x = PolicyVersion
        <$> x .@? "CreateDate"
        <*> x .@? "Document"
        <*> x .@? "IsDefaultVersion"
        <*> x .@? "VersionId"

instance ToQuery PolicyVersion where
    toQuery PolicyVersion{..} = mconcat
        [ "CreateDate"       =? _pvCreateDate
        , "Document"         =? _pvDocument
        , "IsDefaultVersion" =? _pvIsDefaultVersion
        , "VersionId"        =? _pvVersionId
        ]

data InstanceProfile = InstanceProfile
    { _ipArn                 :: Text
    , _ipCreateDate          :: ISO8601
    , _ipInstanceProfileId   :: Text
    , _ipInstanceProfileName :: Text
    , _ipPath                :: Text
    , _ipRoles               :: List "member" Role
    } deriving (Eq, Read, Show)

-- | 'InstanceProfile' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ipArn' @::@ 'Text'
--
-- * 'ipCreateDate' @::@ 'UTCTime'
--
-- * 'ipInstanceProfileId' @::@ 'Text'
--
-- * 'ipInstanceProfileName' @::@ 'Text'
--
-- * 'ipPath' @::@ 'Text'
--
-- * 'ipRoles' @::@ ['Role']
--
instanceProfile :: Text -- ^ 'ipPath'
                -> Text -- ^ 'ipInstanceProfileName'
                -> Text -- ^ 'ipInstanceProfileId'
                -> Text -- ^ 'ipArn'
                -> UTCTime -- ^ 'ipCreateDate'
                -> InstanceProfile
instanceProfile p1 p2 p3 p4 p5 = InstanceProfile
    { _ipPath                = p1
    , _ipInstanceProfileName = p2
    , _ipInstanceProfileId   = p3
    , _ipArn                 = p4
    , _ipCreateDate          = withIso _Time (const id) p5
    , _ipRoles               = mempty
    }

-- | The Amazon Resource Name (ARN) specifying the instance profile. For more
-- information about ARNs and how to use them in policies, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
ipArn :: Lens' InstanceProfile Text
ipArn = lens _ipArn (\s a -> s { _ipArn = a })

-- | The date when the instance profile was created.
ipCreateDate :: Lens' InstanceProfile UTCTime
ipCreateDate = lens _ipCreateDate (\s a -> s { _ipCreateDate = a }) . _Time

-- | The stable and unique string identifying the instance profile. For more
-- information about IDs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
ipInstanceProfileId :: Lens' InstanceProfile Text
ipInstanceProfileId =
    lens _ipInstanceProfileId (\s a -> s { _ipInstanceProfileId = a })

-- | The name identifying the instance profile.
ipInstanceProfileName :: Lens' InstanceProfile Text
ipInstanceProfileName =
    lens _ipInstanceProfileName (\s a -> s { _ipInstanceProfileName = a })

-- | The path to the instance profile. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAMIdentifiers> in the /Using IAM/ guide.
ipPath :: Lens' InstanceProfile Text
ipPath = lens _ipPath (\s a -> s { _ipPath = a })

-- | The role associated with the instance profile.
ipRoles :: Lens' InstanceProfile [Role]
ipRoles = lens _ipRoles (\s a -> s { _ipRoles = a }) . _List

instance FromXML InstanceProfile where
    parseXML x = InstanceProfile
        <$> x .@  "Arn"
        <*> x .@  "CreateDate"
        <*> x .@  "InstanceProfileId"
        <*> x .@  "InstanceProfileName"
        <*> x .@  "Path"
        <*> x .@? "Roles" .!@ mempty

instance ToQuery InstanceProfile where
    toQuery InstanceProfile{..} = mconcat
        [ "Arn"                 =? _ipArn
        , "CreateDate"          =? _ipCreateDate
        , "InstanceProfileId"   =? _ipInstanceProfileId
        , "InstanceProfileName" =? _ipInstanceProfileName
        , "Path"                =? _ipPath
        , "Roles"               =? _ipRoles
        ]

data RoleDetail = RoleDetail
    { _rdArn                      :: Maybe Text
    , _rdAssumeRolePolicyDocument :: Maybe Text
    , _rdCreateDate               :: Maybe ISO8601
    , _rdInstanceProfileList      :: List "member" InstanceProfile
    , _rdPath                     :: Maybe Text
    , _rdRoleId                   :: Maybe Text
    , _rdRoleName                 :: Maybe Text
    , _rdRolePolicyList           :: List "member" PolicyDetail
    } deriving (Eq, Read, Show)

-- | 'RoleDetail' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdArn' @::@ 'Maybe' 'Text'
--
-- * 'rdAssumeRolePolicyDocument' @::@ 'Maybe' 'Text'
--
-- * 'rdCreateDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'rdInstanceProfileList' @::@ ['InstanceProfile']
--
-- * 'rdPath' @::@ 'Maybe' 'Text'
--
-- * 'rdRoleId' @::@ 'Maybe' 'Text'
--
-- * 'rdRoleName' @::@ 'Maybe' 'Text'
--
-- * 'rdRolePolicyList' @::@ ['PolicyDetail']
--
roleDetail :: RoleDetail
roleDetail = RoleDetail
    { _rdPath                     = Nothing
    , _rdRoleName                 = Nothing
    , _rdRoleId                   = Nothing
    , _rdArn                      = Nothing
    , _rdCreateDate               = Nothing
    , _rdAssumeRolePolicyDocument = Nothing
    , _rdInstanceProfileList      = mempty
    , _rdRolePolicyList           = mempty
    }

rdArn :: Lens' RoleDetail (Maybe Text)
rdArn = lens _rdArn (\s a -> s { _rdArn = a })

-- | The trust policy that grants permission to assume the role.
--
-- The returned policy is URL-encoded according to <http://www.faqs.org/rfcs/rfc3986.html RFC 3986>.
rdAssumeRolePolicyDocument :: Lens' RoleDetail (Maybe Text)
rdAssumeRolePolicyDocument =
    lens _rdAssumeRolePolicyDocument
        (\s a -> s { _rdAssumeRolePolicyDocument = a })

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the role was created.
rdCreateDate :: Lens' RoleDetail (Maybe UTCTime)
rdCreateDate = lens _rdCreateDate (\s a -> s { _rdCreateDate = a }) . mapping _Time

rdInstanceProfileList :: Lens' RoleDetail [InstanceProfile]
rdInstanceProfileList =
    lens _rdInstanceProfileList (\s a -> s { _rdInstanceProfileList = a })
        . _List

-- | The path to the role. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
rdPath :: Lens' RoleDetail (Maybe Text)
rdPath = lens _rdPath (\s a -> s { _rdPath = a })

-- | The stable and unique string identifying the role. For more information about
-- IDs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
rdRoleId :: Lens' RoleDetail (Maybe Text)
rdRoleId = lens _rdRoleId (\s a -> s { _rdRoleId = a })

-- | The friendly name that identifies the role.
rdRoleName :: Lens' RoleDetail (Maybe Text)
rdRoleName = lens _rdRoleName (\s a -> s { _rdRoleName = a })

-- | A list of the role's access (permissions) policies.
rdRolePolicyList :: Lens' RoleDetail [PolicyDetail]
rdRolePolicyList = lens _rdRolePolicyList (\s a -> s { _rdRolePolicyList = a }) . _List

instance FromXML RoleDetail where
    parseXML x = RoleDetail
        <$> x .@? "Arn"
        <*> x .@? "AssumeRolePolicyDocument"
        <*> x .@? "CreateDate"
        <*> x .@? "InstanceProfileList" .!@ mempty
        <*> x .@? "Path"
        <*> x .@? "RoleId"
        <*> x .@? "RoleName"
        <*> x .@? "RolePolicyList" .!@ mempty

instance ToQuery RoleDetail where
    toQuery RoleDetail{..} = mconcat
        [ "Arn"                      =? _rdArn
        , "AssumeRolePolicyDocument" =? _rdAssumeRolePolicyDocument
        , "CreateDate"               =? _rdCreateDate
        , "InstanceProfileList"      =? _rdInstanceProfileList
        , "Path"                     =? _rdPath
        , "RoleId"                   =? _rdRoleId
        , "RoleName"                 =? _rdRoleName
        , "RolePolicyList"           =? _rdRolePolicyList
        ]

data ReportFormatType
    = TextCsv -- ^ text/csv
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable ReportFormatType

instance FromText ReportFormatType where
    parser = takeLowerText >>= \case
        "text/csv" -> pure TextCsv
        e          -> fail $
            "Failure parsing ReportFormatType from " ++ show e

instance ToText ReportFormatType where
    toText TextCsv = "text/csv"

instance ToByteString ReportFormatType
instance ToHeader     ReportFormatType
instance ToQuery      ReportFormatType

instance FromXML ReportFormatType where
    parseXML = parseXMLText "ReportFormatType"

data ServerCertificateMetadata = ServerCertificateMetadata
    { _scmArn                   :: Text
    , _scmExpiration            :: Maybe ISO8601
    , _scmPath                  :: Text
    , _scmServerCertificateId   :: Text
    , _scmServerCertificateName :: Text
    , _scmUploadDate            :: Maybe ISO8601
    } deriving (Eq, Ord, Read, Show)

-- | 'ServerCertificateMetadata' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'scmArn' @::@ 'Text'
--
-- * 'scmExpiration' @::@ 'Maybe' 'UTCTime'
--
-- * 'scmPath' @::@ 'Text'
--
-- * 'scmServerCertificateId' @::@ 'Text'
--
-- * 'scmServerCertificateName' @::@ 'Text'
--
-- * 'scmUploadDate' @::@ 'Maybe' 'UTCTime'
--
serverCertificateMetadata :: Text -- ^ 'scmPath'
                          -> Text -- ^ 'scmServerCertificateName'
                          -> Text -- ^ 'scmServerCertificateId'
                          -> Text -- ^ 'scmArn'
                          -> ServerCertificateMetadata
serverCertificateMetadata p1 p2 p3 p4 = ServerCertificateMetadata
    { _scmPath                  = p1
    , _scmServerCertificateName = p2
    , _scmServerCertificateId   = p3
    , _scmArn                   = p4
    , _scmUploadDate            = Nothing
    , _scmExpiration            = Nothing
    }

-- | The Amazon Resource Name (ARN) specifying the server certificate. For more
-- information about ARNs and how to use them in policies, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
scmArn :: Lens' ServerCertificateMetadata Text
scmArn = lens _scmArn (\s a -> s { _scmArn = a })

-- | The date on which the certificate is set to expire.
scmExpiration :: Lens' ServerCertificateMetadata (Maybe UTCTime)
scmExpiration = lens _scmExpiration (\s a -> s { _scmExpiration = a }) . mapping _Time

-- | The path to the server certificate. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
scmPath :: Lens' ServerCertificateMetadata Text
scmPath = lens _scmPath (\s a -> s { _scmPath = a })

-- | The stable and unique string identifying the server certificate. For more
-- information about IDs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
scmServerCertificateId :: Lens' ServerCertificateMetadata Text
scmServerCertificateId =
    lens _scmServerCertificateId (\s a -> s { _scmServerCertificateId = a })

-- | The name that identifies the server certificate.
scmServerCertificateName :: Lens' ServerCertificateMetadata Text
scmServerCertificateName =
    lens _scmServerCertificateName
        (\s a -> s { _scmServerCertificateName = a })

-- | The date when the server certificate was uploaded.
scmUploadDate :: Lens' ServerCertificateMetadata (Maybe UTCTime)
scmUploadDate = lens _scmUploadDate (\s a -> s { _scmUploadDate = a }) . mapping _Time

instance FromXML ServerCertificateMetadata where
    parseXML x = ServerCertificateMetadata
        <$> x .@  "Arn"
        <*> x .@? "Expiration"
        <*> x .@  "Path"
        <*> x .@  "ServerCertificateId"
        <*> x .@  "ServerCertificateName"
        <*> x .@? "UploadDate"

instance ToQuery ServerCertificateMetadata where
    toQuery ServerCertificateMetadata{..} = mconcat
        [ "Arn"                   =? _scmArn
        , "Expiration"            =? _scmExpiration
        , "Path"                  =? _scmPath
        , "ServerCertificateId"   =? _scmServerCertificateId
        , "ServerCertificateName" =? _scmServerCertificateName
        , "UploadDate"            =? _scmUploadDate
        ]

newtype OpenIDConnectProviderListEntry = OpenIDConnectProviderListEntry
    { _oidcpleArn :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'OpenIDConnectProviderListEntry' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'oidcpleArn' @::@ 'Maybe' 'Text'
--
openIDConnectProviderListEntry :: OpenIDConnectProviderListEntry
openIDConnectProviderListEntry = OpenIDConnectProviderListEntry
    { _oidcpleArn = Nothing
    }

oidcpleArn :: Lens' OpenIDConnectProviderListEntry (Maybe Text)
oidcpleArn = lens _oidcpleArn (\s a -> s { _oidcpleArn = a })

instance FromXML OpenIDConnectProviderListEntry where
    parseXML x = OpenIDConnectProviderListEntry
        <$> x .@? "Arn"

instance ToQuery OpenIDConnectProviderListEntry where
    toQuery OpenIDConnectProviderListEntry{..} = mconcat
        [ "Arn" =? _oidcpleArn
        ]

data LoginProfile = LoginProfile
    { _lpCreateDate            :: ISO8601
    , _lpPasswordResetRequired :: Maybe Bool
    , _lpUserName              :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'LoginProfile' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lpCreateDate' @::@ 'UTCTime'
--
-- * 'lpPasswordResetRequired' @::@ 'Maybe' 'Bool'
--
-- * 'lpUserName' @::@ 'Text'
--
loginProfile :: Text -- ^ 'lpUserName'
             -> UTCTime -- ^ 'lpCreateDate'
             -> LoginProfile
loginProfile p1 p2 = LoginProfile
    { _lpUserName              = p1
    , _lpCreateDate            = withIso _Time (const id) p2
    , _lpPasswordResetRequired = Nothing
    }

-- | The date when the password for the user was created.
lpCreateDate :: Lens' LoginProfile UTCTime
lpCreateDate = lens _lpCreateDate (\s a -> s { _lpCreateDate = a }) . _Time

-- | Specifies whether the user is required to set a new password on next sign-in.
lpPasswordResetRequired :: Lens' LoginProfile (Maybe Bool)
lpPasswordResetRequired =
    lens _lpPasswordResetRequired (\s a -> s { _lpPasswordResetRequired = a })

-- | The name of the user, which can be used for signing in to the AWS Management
-- Console.
lpUserName :: Lens' LoginProfile Text
lpUserName = lens _lpUserName (\s a -> s { _lpUserName = a })

instance FromXML LoginProfile where
    parseXML x = LoginProfile
        <$> x .@  "CreateDate"
        <*> x .@? "PasswordResetRequired"
        <*> x .@  "UserName"

instance ToQuery LoginProfile where
    toQuery LoginProfile{..} = mconcat
        [ "CreateDate"            =? _lpCreateDate
        , "PasswordResetRequired" =? _lpPasswordResetRequired
        , "UserName"              =? _lpUserName
        ]

data EntityType
    = ETGroup -- ^ Group
    | ETRole  -- ^ Role
    | ETUser  -- ^ User
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable EntityType

instance FromText EntityType where
    parser = takeLowerText >>= \case
        "group" -> pure ETGroup
        "role"  -> pure ETRole
        "user"  -> pure ETUser
        e       -> fail $
            "Failure parsing EntityType from " ++ show e

instance ToText EntityType where
    toText = \case
        ETGroup -> "Group"
        ETRole  -> "Role"
        ETUser  -> "User"

instance ToByteString EntityType
instance ToHeader     EntityType
instance ToQuery      EntityType

instance FromXML EntityType where
    parseXML = parseXMLText "EntityType"

data SummaryKeyType
    = AccessKeysPerUserQuota            -- ^ AccessKeysPerUserQuota
    | AccountAccessKeysPresent          -- ^ AccountAccessKeysPresent
    | AccountMFAEnabled                 -- ^ AccountMFAEnabled
    | AccountSigningCertificatesPresent -- ^ AccountSigningCertificatesPresent
    | AttachedPoliciesPerGroupQuota     -- ^ AttachedPoliciesPerGroupQuota
    | AttachedPoliciesPerRoleQuota      -- ^ AttachedPoliciesPerRoleQuota
    | AttachedPoliciesPerUserQuota      -- ^ AttachedPoliciesPerUserQuota
    | GroupPolicySizeQuota              -- ^ GroupPolicySizeQuota
    | Groups                            -- ^ Groups
    | GroupsPerUserQuota                -- ^ GroupsPerUserQuota
    | GroupsQuota                       -- ^ GroupsQuota
    | MFADevices                        -- ^ MFADevices
    | MFADevicesInUse                   -- ^ MFADevicesInUse
    | Policies                          -- ^ Policies
    | PoliciesQuota                     -- ^ PoliciesQuota
    | PolicySizeQuota                   -- ^ PolicySizeQuota
    | PolicyVersionsInUse               -- ^ PolicyVersionsInUse
    | PolicyVersionsInUseQuota          -- ^ PolicyVersionsInUseQuota
    | ServerCertificates                -- ^ ServerCertificates
    | ServerCertificatesQuota           -- ^ ServerCertificatesQuota
    | SigningCertificatesPerUserQuota   -- ^ SigningCertificatesPerUserQuota
    | UserPolicySizeQuota               -- ^ UserPolicySizeQuota
    | Users                             -- ^ Users
    | UsersQuota                        -- ^ UsersQuota
    | VersionsPerPolicyQuota            -- ^ VersionsPerPolicyQuota
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable SummaryKeyType

instance FromText SummaryKeyType where
    parser = takeLowerText >>= \case
        "accesskeysperuserquota"            -> pure AccessKeysPerUserQuota
        "accountaccesskeyspresent"          -> pure AccountAccessKeysPresent
        "accountmfaenabled"                 -> pure AccountMFAEnabled
        "accountsigningcertificatespresent" -> pure AccountSigningCertificatesPresent
        "attachedpoliciespergroupquota"     -> pure AttachedPoliciesPerGroupQuota
        "attachedpoliciesperrolequota"      -> pure AttachedPoliciesPerRoleQuota
        "attachedpoliciesperuserquota"      -> pure AttachedPoliciesPerUserQuota
        "grouppolicysizequota"              -> pure GroupPolicySizeQuota
        "groups"                            -> pure Groups
        "groupsperuserquota"                -> pure GroupsPerUserQuota
        "groupsquota"                       -> pure GroupsQuota
        "mfadevices"                        -> pure MFADevices
        "mfadevicesinuse"                   -> pure MFADevicesInUse
        "policies"                          -> pure Policies
        "policiesquota"                     -> pure PoliciesQuota
        "policysizequota"                   -> pure PolicySizeQuota
        "policyversionsinuse"               -> pure PolicyVersionsInUse
        "policyversionsinusequota"          -> pure PolicyVersionsInUseQuota
        "servercertificates"                -> pure ServerCertificates
        "servercertificatesquota"           -> pure ServerCertificatesQuota
        "signingcertificatesperuserquota"   -> pure SigningCertificatesPerUserQuota
        "userpolicysizequota"               -> pure UserPolicySizeQuota
        "users"                             -> pure Users
        "usersquota"                        -> pure UsersQuota
        "versionsperpolicyquota"            -> pure VersionsPerPolicyQuota
        e                                   -> fail $
            "Failure parsing SummaryKeyType from " ++ show e

instance ToText SummaryKeyType where
    toText = \case
        AccessKeysPerUserQuota            -> "AccessKeysPerUserQuota"
        AccountAccessKeysPresent          -> "AccountAccessKeysPresent"
        AccountMFAEnabled                 -> "AccountMFAEnabled"
        AccountSigningCertificatesPresent -> "AccountSigningCertificatesPresent"
        AttachedPoliciesPerGroupQuota     -> "AttachedPoliciesPerGroupQuota"
        AttachedPoliciesPerRoleQuota      -> "AttachedPoliciesPerRoleQuota"
        AttachedPoliciesPerUserQuota      -> "AttachedPoliciesPerUserQuota"
        GroupPolicySizeQuota              -> "GroupPolicySizeQuota"
        Groups                            -> "Groups"
        GroupsPerUserQuota                -> "GroupsPerUserQuota"
        GroupsQuota                       -> "GroupsQuota"
        MFADevices                        -> "MFADevices"
        MFADevicesInUse                   -> "MFADevicesInUse"
        Policies                          -> "Policies"
        PoliciesQuota                     -> "PoliciesQuota"
        PolicySizeQuota                   -> "PolicySizeQuota"
        PolicyVersionsInUse               -> "PolicyVersionsInUse"
        PolicyVersionsInUseQuota          -> "PolicyVersionsInUseQuota"
        ServerCertificates                -> "ServerCertificates"
        ServerCertificatesQuota           -> "ServerCertificatesQuota"
        SigningCertificatesPerUserQuota   -> "SigningCertificatesPerUserQuota"
        UserPolicySizeQuota               -> "UserPolicySizeQuota"
        Users                             -> "Users"
        UsersQuota                        -> "UsersQuota"
        VersionsPerPolicyQuota            -> "VersionsPerPolicyQuota"

instance ToByteString SummaryKeyType
instance ToHeader     SummaryKeyType
instance ToQuery      SummaryKeyType

instance FromXML SummaryKeyType where
    parseXML = parseXMLText "SummaryKeyType"

data GroupDetail = GroupDetail
    { _gdArn             :: Maybe Text
    , _gdCreateDate      :: Maybe ISO8601
    , _gdGroupId         :: Maybe Text
    , _gdGroupName       :: Maybe Text
    , _gdGroupPolicyList :: List "member" PolicyDetail
    , _gdPath            :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'GroupDetail' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdArn' @::@ 'Maybe' 'Text'
--
-- * 'gdCreateDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'gdGroupId' @::@ 'Maybe' 'Text'
--
-- * 'gdGroupName' @::@ 'Maybe' 'Text'
--
-- * 'gdGroupPolicyList' @::@ ['PolicyDetail']
--
-- * 'gdPath' @::@ 'Maybe' 'Text'
--
groupDetail :: GroupDetail
groupDetail = GroupDetail
    { _gdPath            = Nothing
    , _gdGroupName       = Nothing
    , _gdGroupId         = Nothing
    , _gdArn             = Nothing
    , _gdCreateDate      = Nothing
    , _gdGroupPolicyList = mempty
    }

gdArn :: Lens' GroupDetail (Maybe Text)
gdArn = lens _gdArn (\s a -> s { _gdArn = a })

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the group was created.
gdCreateDate :: Lens' GroupDetail (Maybe UTCTime)
gdCreateDate = lens _gdCreateDate (\s a -> s { _gdCreateDate = a }) . mapping _Time

-- | The stable and unique string identifying the group. For more information
-- about IDs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
gdGroupId :: Lens' GroupDetail (Maybe Text)
gdGroupId = lens _gdGroupId (\s a -> s { _gdGroupId = a })

-- | The friendly name that identifies the group.
gdGroupName :: Lens' GroupDetail (Maybe Text)
gdGroupName = lens _gdGroupName (\s a -> s { _gdGroupName = a })

-- | A list of the group's policies.
gdGroupPolicyList :: Lens' GroupDetail [PolicyDetail]
gdGroupPolicyList =
    lens _gdGroupPolicyList (\s a -> s { _gdGroupPolicyList = a })
        . _List

-- | The path to the group. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
gdPath :: Lens' GroupDetail (Maybe Text)
gdPath = lens _gdPath (\s a -> s { _gdPath = a })

instance FromXML GroupDetail where
    parseXML x = GroupDetail
        <$> x .@? "Arn"
        <*> x .@? "CreateDate"
        <*> x .@? "GroupId"
        <*> x .@? "GroupName"
        <*> x .@? "GroupPolicyList" .!@ mempty
        <*> x .@? "Path"

instance ToQuery GroupDetail where
    toQuery GroupDetail{..} = mconcat
        [ "Arn"             =? _gdArn
        , "CreateDate"      =? _gdCreateDate
        , "GroupId"         =? _gdGroupId
        , "GroupName"       =? _gdGroupName
        , "GroupPolicyList" =? _gdGroupPolicyList
        , "Path"            =? _gdPath
        ]

data ReportStateType
    = Complete   -- ^ COMPLETE
    | Inprogress -- ^ INPROGRESS
    | Started    -- ^ STARTED
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable ReportStateType

instance FromText ReportStateType where
    parser = takeLowerText >>= \case
        "complete"   -> pure Complete
        "inprogress" -> pure Inprogress
        "started"    -> pure Started
        e            -> fail $
            "Failure parsing ReportStateType from " ++ show e

instance ToText ReportStateType where
    toText = \case
        Complete   -> "COMPLETE"
        Inprogress -> "INPROGRESS"
        Started    -> "STARTED"

instance ToByteString ReportStateType
instance ToHeader     ReportStateType
instance ToQuery      ReportStateType

instance FromXML ReportStateType where
    parseXML = parseXMLText "ReportStateType"

data User = User
    { _uArn              :: Text
    , _uCreateDate       :: ISO8601
    , _uPasswordLastUsed :: Maybe ISO8601
    , _uPath             :: Text
    , _uUserId           :: Text
    , _uUserName         :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'User' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uArn' @::@ 'Text'
--
-- * 'uCreateDate' @::@ 'UTCTime'
--
-- * 'uPasswordLastUsed' @::@ 'Maybe' 'UTCTime'
--
-- * 'uPath' @::@ 'Text'
--
-- * 'uUserId' @::@ 'Text'
--
-- * 'uUserName' @::@ 'Text'
--
user :: Text -- ^ 'uPath'
     -> Text -- ^ 'uUserName'
     -> Text -- ^ 'uUserId'
     -> Text -- ^ 'uArn'
     -> UTCTime -- ^ 'uCreateDate'
     -> User
user p1 p2 p3 p4 p5 = User
    { _uPath             = p1
    , _uUserName         = p2
    , _uUserId           = p3
    , _uArn              = p4
    , _uCreateDate       = withIso _Time (const id) p5
    , _uPasswordLastUsed = Nothing
    }

-- | The Amazon Resource Name (ARN) that identifies the user. For more information
-- about ARNs and how to use ARNs in policies, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /UsingIAM/ guide.
uArn :: Lens' User Text
uArn = lens _uArn (\s a -> s { _uArn = a })

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the user was created.
uCreateDate :: Lens' User UTCTime
uCreateDate = lens _uCreateDate (\s a -> s { _uCreateDate = a }) . _Time

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the user's password was
-- last used to sign in to an AWS website. For a list of AWS websites that
-- capture a user's last sign-in time, see the <http://docs.aws.amazon.com/IAM/latest/UserGuide/credential-reports.html Credential Reports> topic in the /Using IAM/ guide. If a password is used more than once in a five-minute span, only the
-- first use is returned in this field. When the user does not have a password,
-- this field is null (not present). When a user's password exists but has never
-- been used, or when there is no sign-in data associated with the user, this
-- field is null (not present).
--
-- This value is returned only in the 'GetUser' and 'ListUsers' actions.
uPasswordLastUsed :: Lens' User (Maybe UTCTime)
uPasswordLastUsed =
    lens _uPasswordLastUsed (\s a -> s { _uPasswordLastUsed = a })
        . mapping _Time

-- | The path to the user. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
uPath :: Lens' User Text
uPath = lens _uPath (\s a -> s { _uPath = a })

-- | The stable and unique string identifying the user. For more information about
-- IDs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
uUserId :: Lens' User Text
uUserId = lens _uUserId (\s a -> s { _uUserId = a })

-- | The friendly name identifying the user.
uUserName :: Lens' User Text
uUserName = lens _uUserName (\s a -> s { _uUserName = a })

instance FromXML User where
    parseXML x = User
        <$> x .@  "Arn"
        <*> x .@  "CreateDate"
        <*> x .@? "PasswordLastUsed"
        <*> x .@  "Path"
        <*> x .@  "UserId"
        <*> x .@  "UserName"

instance ToQuery User where
    toQuery User{..} = mconcat
        [ "Arn"              =? _uArn
        , "CreateDate"       =? _uCreateDate
        , "PasswordLastUsed" =? _uPasswordLastUsed
        , "Path"             =? _uPath
        , "UserId"           =? _uUserId
        , "UserName"         =? _uUserName
        ]

data PolicyDetail = PolicyDetail
    { _pdPolicyDocument :: Maybe Text
    , _pdPolicyName     :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'PolicyDetail' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pdPolicyDocument' @::@ 'Maybe' 'Text'
--
-- * 'pdPolicyName' @::@ 'Maybe' 'Text'
--
policyDetail :: PolicyDetail
policyDetail = PolicyDetail
    { _pdPolicyName     = Nothing
    , _pdPolicyDocument = Nothing
    }

-- | The policy document.
--
-- The returned policy is URL-encoded according to <http://www.faqs.org/rfcs/rfc3986.html RFC 3986>.
pdPolicyDocument :: Lens' PolicyDetail (Maybe Text)
pdPolicyDocument = lens _pdPolicyDocument (\s a -> s { _pdPolicyDocument = a })

-- | The name of the policy.
pdPolicyName :: Lens' PolicyDetail (Maybe Text)
pdPolicyName = lens _pdPolicyName (\s a -> s { _pdPolicyName = a })

instance FromXML PolicyDetail where
    parseXML x = PolicyDetail
        <$> x .@? "PolicyDocument"
        <*> x .@? "PolicyName"

instance ToQuery PolicyDetail where
    toQuery PolicyDetail{..} = mconcat
        [ "PolicyDocument" =? _pdPolicyDocument
        , "PolicyName"     =? _pdPolicyName
        ]

data StatusType
    = Active   -- ^ Active
    | Inactive -- ^ Inactive
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable StatusType

instance FromText StatusType where
    parser = takeLowerText >>= \case
        "active"   -> pure Active
        "inactive" -> pure Inactive
        e          -> fail $
            "Failure parsing StatusType from " ++ show e

instance ToText StatusType where
    toText = \case
        Active   -> "Active"
        Inactive -> "Inactive"

instance ToByteString StatusType
instance ToHeader     StatusType
instance ToQuery      StatusType

instance FromXML StatusType where
    parseXML = parseXMLText "StatusType"

data SAMLProviderListEntry = SAMLProviderListEntry
    { _samlpleArn        :: Maybe Text
    , _samlpleCreateDate :: Maybe ISO8601
    , _samlpleValidUntil :: Maybe ISO8601
    } deriving (Eq, Ord, Read, Show)

-- | 'SAMLProviderListEntry' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'samlpleArn' @::@ 'Maybe' 'Text'
--
-- * 'samlpleCreateDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'samlpleValidUntil' @::@ 'Maybe' 'UTCTime'
--
samlproviderListEntry :: SAMLProviderListEntry
samlproviderListEntry = SAMLProviderListEntry
    { _samlpleArn        = Nothing
    , _samlpleValidUntil = Nothing
    , _samlpleCreateDate = Nothing
    }

-- | The Amazon Resource Name (ARN) of the SAML provider.
samlpleArn :: Lens' SAMLProviderListEntry (Maybe Text)
samlpleArn = lens _samlpleArn (\s a -> s { _samlpleArn = a })

-- | The date and time when the SAML provider was created.
samlpleCreateDate :: Lens' SAMLProviderListEntry (Maybe UTCTime)
samlpleCreateDate =
    lens _samlpleCreateDate (\s a -> s { _samlpleCreateDate = a })
        . mapping _Time

-- | The expiration date and time for the SAML provider.
samlpleValidUntil :: Lens' SAMLProviderListEntry (Maybe UTCTime)
samlpleValidUntil =
    lens _samlpleValidUntil (\s a -> s { _samlpleValidUntil = a })
        . mapping _Time

instance FromXML SAMLProviderListEntry where
    parseXML x = SAMLProviderListEntry
        <$> x .@? "Arn"
        <*> x .@? "CreateDate"
        <*> x .@? "ValidUntil"

instance ToQuery SAMLProviderListEntry where
    toQuery SAMLProviderListEntry{..} = mconcat
        [ "Arn"        =? _samlpleArn
        , "CreateDate" =? _samlpleCreateDate
        , "ValidUntil" =? _samlpleValidUntil
        ]

data Role = Role
    { _rArn                      :: Text
    , _rAssumeRolePolicyDocument :: Maybe Text
    , _rCreateDate               :: ISO8601
    , _rPath                     :: Text
    , _rRoleId                   :: Text
    , _rRoleName                 :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'Role' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rArn' @::@ 'Text'
--
-- * 'rAssumeRolePolicyDocument' @::@ 'Maybe' 'Text'
--
-- * 'rCreateDate' @::@ 'UTCTime'
--
-- * 'rPath' @::@ 'Text'
--
-- * 'rRoleId' @::@ 'Text'
--
-- * 'rRoleName' @::@ 'Text'
--
role :: Text -- ^ 'rPath'
     -> Text -- ^ 'rRoleName'
     -> Text -- ^ 'rRoleId'
     -> Text -- ^ 'rArn'
     -> UTCTime -- ^ 'rCreateDate'
     -> Role
role p1 p2 p3 p4 p5 = Role
    { _rPath                     = p1
    , _rRoleName                 = p2
    , _rRoleId                   = p3
    , _rArn                      = p4
    , _rCreateDate               = withIso _Time (const id) p5
    , _rAssumeRolePolicyDocument = Nothing
    }

-- | The Amazon Resource Name (ARN) specifying the role. For more information
-- about ARNs and how to use them in policies, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /UsingIAM/ guide.
rArn :: Lens' Role Text
rArn = lens _rArn (\s a -> s { _rArn = a })

-- | The policy that grants an entity permission to assume the role.
--
-- The returned policy is URL-encoded according to <http://www.faqs.org/rfcs/rfc3986.html RFC 3986>.
rAssumeRolePolicyDocument :: Lens' Role (Maybe Text)
rAssumeRolePolicyDocument =
    lens _rAssumeRolePolicyDocument
        (\s a -> s { _rAssumeRolePolicyDocument = a })

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the role was created.
rCreateDate :: Lens' Role UTCTime
rCreateDate = lens _rCreateDate (\s a -> s { _rCreateDate = a }) . _Time

-- | The path to the role. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
rPath :: Lens' Role Text
rPath = lens _rPath (\s a -> s { _rPath = a })

-- | The stable and unique string identifying the role. For more information
-- about IDs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
rRoleId :: Lens' Role Text
rRoleId = lens _rRoleId (\s a -> s { _rRoleId = a })

-- | The friendly name that identifies the role.
rRoleName :: Lens' Role Text
rRoleName = lens _rRoleName (\s a -> s { _rRoleName = a })

instance FromXML Role where
    parseXML x = Role
        <$> x .@  "Arn"
        <*> x .@? "AssumeRolePolicyDocument"
        <*> x .@  "CreateDate"
        <*> x .@  "Path"
        <*> x .@  "RoleId"
        <*> x .@  "RoleName"

instance ToQuery Role where
    toQuery Role{..} = mconcat
        [ "Arn"                      =? _rArn
        , "AssumeRolePolicyDocument" =? _rAssumeRolePolicyDocument
        , "CreateDate"               =? _rCreateDate
        , "Path"                     =? _rPath
        , "RoleId"                   =? _rRoleId
        , "RoleName"                 =? _rRoleName
        ]

newtype PolicyGroup = PolicyGroup
    { _pgGroupName :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'PolicyGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pgGroupName' @::@ 'Maybe' 'Text'
--
policyGroup :: PolicyGroup
policyGroup = PolicyGroup
    { _pgGroupName = Nothing
    }

-- | The name (friendly name, not ARN) identifying the group.
pgGroupName :: Lens' PolicyGroup (Maybe Text)
pgGroupName = lens _pgGroupName (\s a -> s { _pgGroupName = a })

instance FromXML PolicyGroup where
    parseXML x = PolicyGroup
        <$> x .@? "GroupName"

instance ToQuery PolicyGroup where
    toQuery PolicyGroup{..} = mconcat
        [ "GroupName" =? _pgGroupName
        ]

data PolicyScopeType
    = All   -- ^ All
    | Aws   -- ^ AWS
    | Local -- ^ Local
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable PolicyScopeType

instance FromText PolicyScopeType where
    parser = takeLowerText >>= \case
        "all"   -> pure All
        "aws"   -> pure Aws
        "local" -> pure Local
        e       -> fail $
            "Failure parsing PolicyScopeType from " ++ show e

instance ToText PolicyScopeType where
    toText = \case
        All   -> "All"
        Aws   -> "AWS"
        Local -> "Local"

instance ToByteString PolicyScopeType
instance ToHeader     PolicyScopeType
instance ToQuery      PolicyScopeType

instance FromXML PolicyScopeType where
    parseXML = parseXMLText "PolicyScopeType"

data UserDetail = UserDetail
    { _udArn            :: Maybe Text
    , _udCreateDate     :: Maybe ISO8601
    , _udGroupList      :: List "member" Text
    , _udPath           :: Maybe Text
    , _udUserId         :: Maybe Text
    , _udUserName       :: Maybe Text
    , _udUserPolicyList :: List "member" PolicyDetail
    } deriving (Eq, Read, Show)

-- | 'UserDetail' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udArn' @::@ 'Maybe' 'Text'
--
-- * 'udCreateDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'udGroupList' @::@ ['Text']
--
-- * 'udPath' @::@ 'Maybe' 'Text'
--
-- * 'udUserId' @::@ 'Maybe' 'Text'
--
-- * 'udUserName' @::@ 'Maybe' 'Text'
--
-- * 'udUserPolicyList' @::@ ['PolicyDetail']
--
userDetail :: UserDetail
userDetail = UserDetail
    { _udPath           = Nothing
    , _udUserName       = Nothing
    , _udUserId         = Nothing
    , _udArn            = Nothing
    , _udCreateDate     = Nothing
    , _udUserPolicyList = mempty
    , _udGroupList      = mempty
    }

udArn :: Lens' UserDetail (Maybe Text)
udArn = lens _udArn (\s a -> s { _udArn = a })

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the user was created.
udCreateDate :: Lens' UserDetail (Maybe UTCTime)
udCreateDate = lens _udCreateDate (\s a -> s { _udCreateDate = a }) . mapping _Time

-- | A list of IAM groups that the user is in.
udGroupList :: Lens' UserDetail [Text]
udGroupList = lens _udGroupList (\s a -> s { _udGroupList = a }) . _List

-- | The path to the user. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
udPath :: Lens' UserDetail (Maybe Text)
udPath = lens _udPath (\s a -> s { _udPath = a })

-- | The stable and unique string identifying the user. For more information about
-- IDs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
udUserId :: Lens' UserDetail (Maybe Text)
udUserId = lens _udUserId (\s a -> s { _udUserId = a })

-- | The friendly name identifying the user.
udUserName :: Lens' UserDetail (Maybe Text)
udUserName = lens _udUserName (\s a -> s { _udUserName = a })

-- | A list of the user's policies.
udUserPolicyList :: Lens' UserDetail [PolicyDetail]
udUserPolicyList = lens _udUserPolicyList (\s a -> s { _udUserPolicyList = a }) . _List

instance FromXML UserDetail where
    parseXML x = UserDetail
        <$> x .@? "Arn"
        <*> x .@? "CreateDate"
        <*> x .@? "GroupList" .!@ mempty
        <*> x .@? "Path"
        <*> x .@? "UserId"
        <*> x .@? "UserName"
        <*> x .@? "UserPolicyList" .!@ mempty

instance ToQuery UserDetail where
    toQuery UserDetail{..} = mconcat
        [ "Arn"            =? _udArn
        , "CreateDate"     =? _udCreateDate
        , "GroupList"      =? _udGroupList
        , "Path"           =? _udPath
        , "UserId"         =? _udUserId
        , "UserName"       =? _udUserName
        , "UserPolicyList" =? _udUserPolicyList
        ]

data Policy = Policy
    { _pArn              :: Maybe Text
    , _pAttachmentCount  :: Maybe Int
    , _pCreateDate       :: Maybe ISO8601
    , _pDefaultVersionId :: Maybe Text
    , _pDescription      :: Maybe Text
    , _pIsAttachable     :: Maybe Bool
    , _pPath             :: Maybe Text
    , _pPolicyId         :: Maybe Text
    , _pPolicyName       :: Maybe Text
    , _pUpdateDate       :: Maybe ISO8601
    } deriving (Eq, Ord, Read, Show)

-- | 'Policy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pArn' @::@ 'Maybe' 'Text'
--
-- * 'pAttachmentCount' @::@ 'Maybe' 'Int'
--
-- * 'pCreateDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'pDefaultVersionId' @::@ 'Maybe' 'Text'
--
-- * 'pDescription' @::@ 'Maybe' 'Text'
--
-- * 'pIsAttachable' @::@ 'Maybe' 'Bool'
--
-- * 'pPath' @::@ 'Maybe' 'Text'
--
-- * 'pPolicyId' @::@ 'Maybe' 'Text'
--
-- * 'pPolicyName' @::@ 'Maybe' 'Text'
--
-- * 'pUpdateDate' @::@ 'Maybe' 'UTCTime'
--
policy :: Policy
policy = Policy
    { _pPolicyName       = Nothing
    , _pPolicyId         = Nothing
    , _pArn              = Nothing
    , _pPath             = Nothing
    , _pDefaultVersionId = Nothing
    , _pAttachmentCount  = Nothing
    , _pIsAttachable     = Nothing
    , _pDescription      = Nothing
    , _pCreateDate       = Nothing
    , _pUpdateDate       = Nothing
    }

pArn :: Lens' Policy (Maybe Text)
pArn = lens _pArn (\s a -> s { _pArn = a })

-- | The number of entities (users, groups, and roles) that the policy is attached
-- to.
pAttachmentCount :: Lens' Policy (Maybe Int)
pAttachmentCount = lens _pAttachmentCount (\s a -> s { _pAttachmentCount = a })

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the policy was created.
pCreateDate :: Lens' Policy (Maybe UTCTime)
pCreateDate = lens _pCreateDate (\s a -> s { _pCreateDate = a }) . mapping _Time

-- | The identifier for the version of the policy that is set as the default
-- version.
pDefaultVersionId :: Lens' Policy (Maybe Text)
pDefaultVersionId =
    lens _pDefaultVersionId (\s a -> s { _pDefaultVersionId = a })

-- | A friendly description of the policy.
--
-- This element is included in the response to the 'GetPolicy' operation. It is
-- not included in the response to the 'ListPolicies' operation.
pDescription :: Lens' Policy (Maybe Text)
pDescription = lens _pDescription (\s a -> s { _pDescription = a })

-- | Specifies whether the policy can be attached to an IAM user, group, or role.
pIsAttachable :: Lens' Policy (Maybe Bool)
pIsAttachable = lens _pIsAttachable (\s a -> s { _pIsAttachable = a })

-- | The path to the policy.
--
-- For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
pPath :: Lens' Policy (Maybe Text)
pPath = lens _pPath (\s a -> s { _pPath = a })

-- | The stable and unique string identifying the policy.
--
-- For more information about IDs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
pPolicyId :: Lens' Policy (Maybe Text)
pPolicyId = lens _pPolicyId (\s a -> s { _pPolicyId = a })

-- | The friendly name (not ARN) identifying the policy.
pPolicyName :: Lens' Policy (Maybe Text)
pPolicyName = lens _pPolicyName (\s a -> s { _pPolicyName = a })

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the policy was last
-- updated.
--
-- When a policy has only one version, this field contains the date and time
-- when the policy was created. When a policy has more than one version, this
-- field contains the date and time when the most recent policy version was
-- created.
pUpdateDate :: Lens' Policy (Maybe UTCTime)
pUpdateDate = lens _pUpdateDate (\s a -> s { _pUpdateDate = a }) . mapping _Time

instance FromXML Policy where
    parseXML x = Policy
        <$> x .@? "Arn"
        <*> x .@? "AttachmentCount"
        <*> x .@? "CreateDate"
        <*> x .@? "DefaultVersionId"
        <*> x .@? "Description"
        <*> x .@? "IsAttachable"
        <*> x .@? "Path"
        <*> x .@? "PolicyId"
        <*> x .@? "PolicyName"
        <*> x .@? "UpdateDate"

instance ToQuery Policy where
    toQuery Policy{..} = mconcat
        [ "Arn"              =? _pArn
        , "AttachmentCount"  =? _pAttachmentCount
        , "CreateDate"       =? _pCreateDate
        , "DefaultVersionId" =? _pDefaultVersionId
        , "Description"      =? _pDescription
        , "IsAttachable"     =? _pIsAttachable
        , "Path"             =? _pPath
        , "PolicyId"         =? _pPolicyId
        , "PolicyName"       =? _pPolicyName
        , "UpdateDate"       =? _pUpdateDate
        ]

data ServerCertificate = ServerCertificate
    { _scCertificateBody           :: Text
    , _scCertificateChain          :: Maybe Text
    , _scServerCertificateMetadata :: ServerCertificateMetadata
    } deriving (Eq, Read, Show)

-- | 'ServerCertificate' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'scCertificateBody' @::@ 'Text'
--
-- * 'scCertificateChain' @::@ 'Maybe' 'Text'
--
-- * 'scServerCertificateMetadata' @::@ 'ServerCertificateMetadata'
--
serverCertificate :: ServerCertificateMetadata -- ^ 'scServerCertificateMetadata'
                  -> Text -- ^ 'scCertificateBody'
                  -> ServerCertificate
serverCertificate p1 p2 = ServerCertificate
    { _scServerCertificateMetadata = p1
    , _scCertificateBody           = p2
    , _scCertificateChain          = Nothing
    }

-- | The contents of the public key certificate.
scCertificateBody :: Lens' ServerCertificate Text
scCertificateBody =
    lens _scCertificateBody (\s a -> s { _scCertificateBody = a })

-- | The contents of the public key certificate chain.
scCertificateChain :: Lens' ServerCertificate (Maybe Text)
scCertificateChain =
    lens _scCertificateChain (\s a -> s { _scCertificateChain = a })

-- | The meta information of the server certificate, such as its name, path, ID,
-- and ARN.
scServerCertificateMetadata :: Lens' ServerCertificate ServerCertificateMetadata
scServerCertificateMetadata =
    lens _scServerCertificateMetadata
        (\s a -> s { _scServerCertificateMetadata = a })

instance FromXML ServerCertificate where
    parseXML x = ServerCertificate
        <$> x .@  "CertificateBody"
        <*> x .@? "CertificateChain"
        <*> x .@  "ServerCertificateMetadata"

instance ToQuery ServerCertificate where
    toQuery ServerCertificate{..} = mconcat
        [ "CertificateBody"           =? _scCertificateBody
        , "CertificateChain"          =? _scCertificateChain
        , "ServerCertificateMetadata" =? _scServerCertificateMetadata
        ]

data AccessKey = AccessKey
    { _akAccessKeyId     :: Text
    , _akCreateDate      :: Maybe ISO8601
    , _akSecretAccessKey :: Sensitive Text
    , _akStatus          :: StatusType
    , _akUserName        :: Text
    } deriving (Eq, Read, Show)

-- | 'AccessKey' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'akAccessKeyId' @::@ 'Text'
--
-- * 'akCreateDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'akSecretAccessKey' @::@ 'Text'
--
-- * 'akStatus' @::@ 'StatusType'
--
-- * 'akUserName' @::@ 'Text'
--
accessKey :: Text -- ^ 'akUserName'
          -> Text -- ^ 'akAccessKeyId'
          -> StatusType -- ^ 'akStatus'
          -> Text -- ^ 'akSecretAccessKey'
          -> AccessKey
accessKey p1 p2 p3 p4 = AccessKey
    { _akUserName        = p1
    , _akAccessKeyId     = p2
    , _akStatus          = p3
    , _akSecretAccessKey = withIso _Sensitive (const id) p4
    , _akCreateDate      = Nothing
    }

-- | The ID for this access key.
akAccessKeyId :: Lens' AccessKey Text
akAccessKeyId = lens _akAccessKeyId (\s a -> s { _akAccessKeyId = a })

-- | The date when the access key was created.
akCreateDate :: Lens' AccessKey (Maybe UTCTime)
akCreateDate = lens _akCreateDate (\s a -> s { _akCreateDate = a }) . mapping _Time

-- | The secret key used to sign requests.
akSecretAccessKey :: Lens' AccessKey Text
akSecretAccessKey =
    lens _akSecretAccessKey (\s a -> s { _akSecretAccessKey = a })
        . _Sensitive

-- | The status of the access key. 'Active' means the key is valid for API calls,
-- while 'Inactive' means it is not.
akStatus :: Lens' AccessKey StatusType
akStatus = lens _akStatus (\s a -> s { _akStatus = a })

-- | The name of the IAM user that the access key is associated with.
akUserName :: Lens' AccessKey Text
akUserName = lens _akUserName (\s a -> s { _akUserName = a })

instance FromXML AccessKey where
    parseXML x = AccessKey
        <$> x .@  "AccessKeyId"
        <*> x .@? "CreateDate"
        <*> x .@  "SecretAccessKey"
        <*> x .@  "Status"
        <*> x .@  "UserName"

instance ToQuery AccessKey where
    toQuery AccessKey{..} = mconcat
        [ "AccessKeyId"     =? _akAccessKeyId
        , "CreateDate"      =? _akCreateDate
        , "SecretAccessKey" =? _akSecretAccessKey
        , "Status"          =? _akStatus
        , "UserName"        =? _akUserName
        ]

data VirtualMFADevice = VirtualMFADevice
    { _vmfadBase32StringSeed :: Maybe Base64
    , _vmfadEnableDate       :: Maybe ISO8601
    , _vmfadQRCodePNG        :: Maybe Base64
    , _vmfadSerialNumber     :: Text
    , _vmfadUser             :: Maybe User
    } deriving (Eq, Read, Show)

-- | 'VirtualMFADevice' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vmfadBase32StringSeed' @::@ 'Maybe' 'Base64'
--
-- * 'vmfadEnableDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'vmfadQRCodePNG' @::@ 'Maybe' 'Base64'
--
-- * 'vmfadSerialNumber' @::@ 'Text'
--
-- * 'vmfadUser' @::@ 'Maybe' 'User'
--
virtualMFADevice :: Text -- ^ 'vmfadSerialNumber'
                 -> VirtualMFADevice
virtualMFADevice p1 = VirtualMFADevice
    { _vmfadSerialNumber     = p1
    , _vmfadBase32StringSeed = Nothing
    , _vmfadQRCodePNG        = Nothing
    , _vmfadUser             = Nothing
    , _vmfadEnableDate       = Nothing
    }

-- | The Base32 seed defined as specified in <http://www.ietf.org/rfc/rfc3548.txt RFC3548>. The 'Base32StringSeed' is
-- Base64-encoded.
vmfadBase32StringSeed :: Lens' VirtualMFADevice (Maybe Base64)
vmfadBase32StringSeed =
    lens _vmfadBase32StringSeed (\s a -> s { _vmfadBase32StringSeed = a })

-- | The date and time on which the virtual MFA device was enabled.
vmfadEnableDate :: Lens' VirtualMFADevice (Maybe UTCTime)
vmfadEnableDate = lens _vmfadEnableDate (\s a -> s { _vmfadEnableDate = a }) . mapping _Time

-- | A QR code PNG image that encodes 'otpauth://totp/$virtualMFADeviceName@$AccountName?secret=$Base32String' where '$virtualMFADeviceName' is one of the create call arguments, 'AccountName'
-- is the user name if set (otherwise, the account ID otherwise), and 'Base32String' is the seed in Base32 format. The 'Base32String' value is Base64-encoded.
vmfadQRCodePNG :: Lens' VirtualMFADevice (Maybe Base64)
vmfadQRCodePNG = lens _vmfadQRCodePNG (\s a -> s { _vmfadQRCodePNG = a })

-- | The serial number associated with 'VirtualMFADevice'.
vmfadSerialNumber :: Lens' VirtualMFADevice Text
vmfadSerialNumber =
    lens _vmfadSerialNumber (\s a -> s { _vmfadSerialNumber = a })

vmfadUser :: Lens' VirtualMFADevice (Maybe User)
vmfadUser = lens _vmfadUser (\s a -> s { _vmfadUser = a })

instance FromXML VirtualMFADevice where
    parseXML x = VirtualMFADevice
        <$> x .@? "Base32StringSeed"
        <*> x .@? "EnableDate"
        <*> x .@? "QRCodePNG"
        <*> x .@  "SerialNumber"
        <*> x .@? "User"

instance ToQuery VirtualMFADevice where
    toQuery VirtualMFADevice{..} = mconcat
        [ "Base32StringSeed" =? _vmfadBase32StringSeed
        , "EnableDate"       =? _vmfadEnableDate
        , "QRCodePNG"        =? _vmfadQRCodePNG
        , "SerialNumber"     =? _vmfadSerialNumber
        , "User"             =? _vmfadUser
        ]

data SigningCertificate = SigningCertificate
    { _sc1CertificateBody :: Text
    , _sc1CertificateId   :: Text
    , _sc1Status          :: StatusType
    , _sc1UploadDate      :: Maybe ISO8601
    , _sc1UserName        :: Text
    } deriving (Eq, Read, Show)

-- | 'SigningCertificate' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sc1CertificateBody' @::@ 'Text'
--
-- * 'sc1CertificateId' @::@ 'Text'
--
-- * 'sc1Status' @::@ 'StatusType'
--
-- * 'sc1UploadDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'sc1UserName' @::@ 'Text'
--
signingCertificate :: Text -- ^ 'sc1UserName'
                   -> Text -- ^ 'sc1CertificateId'
                   -> Text -- ^ 'sc1CertificateBody'
                   -> StatusType -- ^ 'sc1Status'
                   -> SigningCertificate
signingCertificate p1 p2 p3 p4 = SigningCertificate
    { _sc1UserName        = p1
    , _sc1CertificateId   = p2
    , _sc1CertificateBody = p3
    , _sc1Status          = p4
    , _sc1UploadDate      = Nothing
    }

-- | The contents of the signing certificate.
sc1CertificateBody :: Lens' SigningCertificate Text
sc1CertificateBody =
    lens _sc1CertificateBody (\s a -> s { _sc1CertificateBody = a })

-- | The ID for the signing certificate.
sc1CertificateId :: Lens' SigningCertificate Text
sc1CertificateId = lens _sc1CertificateId (\s a -> s { _sc1CertificateId = a })

-- | The status of the signing certificate. 'Active' means the key is valid for API
-- calls, while 'Inactive' means it is not.
sc1Status :: Lens' SigningCertificate StatusType
sc1Status = lens _sc1Status (\s a -> s { _sc1Status = a })

-- | The date when the signing certificate was uploaded.
sc1UploadDate :: Lens' SigningCertificate (Maybe UTCTime)
sc1UploadDate = lens _sc1UploadDate (\s a -> s { _sc1UploadDate = a }) . mapping _Time

-- | The name of the user the signing certificate is associated with.
sc1UserName :: Lens' SigningCertificate Text
sc1UserName = lens _sc1UserName (\s a -> s { _sc1UserName = a })

instance FromXML SigningCertificate where
    parseXML x = SigningCertificate
        <$> x .@  "CertificateBody"
        <*> x .@  "CertificateId"
        <*> x .@  "Status"
        <*> x .@? "UploadDate"
        <*> x .@  "UserName"

instance ToQuery SigningCertificate where
    toQuery SigningCertificate{..} = mconcat
        [ "CertificateBody" =? _sc1CertificateBody
        , "CertificateId"   =? _sc1CertificateId
        , "Status"          =? _sc1Status
        , "UploadDate"      =? _sc1UploadDate
        , "UserName"        =? _sc1UserName
        ]

data AccessKeyMetadata = AccessKeyMetadata
    { _akmAccessKeyId :: Maybe Text
    , _akmCreateDate  :: Maybe ISO8601
    , _akmStatus      :: Maybe StatusType
    , _akmUserName    :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'AccessKeyMetadata' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'akmAccessKeyId' @::@ 'Maybe' 'Text'
--
-- * 'akmCreateDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'akmStatus' @::@ 'Maybe' 'StatusType'
--
-- * 'akmUserName' @::@ 'Maybe' 'Text'
--
accessKeyMetadata :: AccessKeyMetadata
accessKeyMetadata = AccessKeyMetadata
    { _akmUserName    = Nothing
    , _akmAccessKeyId = Nothing
    , _akmStatus      = Nothing
    , _akmCreateDate  = Nothing
    }

-- | The ID for this access key.
akmAccessKeyId :: Lens' AccessKeyMetadata (Maybe Text)
akmAccessKeyId = lens _akmAccessKeyId (\s a -> s { _akmAccessKeyId = a })

-- | The date when the access key was created.
akmCreateDate :: Lens' AccessKeyMetadata (Maybe UTCTime)
akmCreateDate = lens _akmCreateDate (\s a -> s { _akmCreateDate = a }) . mapping _Time

-- | The status of the access key. 'Active' means the key is valid for API calls; 'Inactive' means it is not.
akmStatus :: Lens' AccessKeyMetadata (Maybe StatusType)
akmStatus = lens _akmStatus (\s a -> s { _akmStatus = a })

-- | The name of the IAM user that the key is associated with.
akmUserName :: Lens' AccessKeyMetadata (Maybe Text)
akmUserName = lens _akmUserName (\s a -> s { _akmUserName = a })

instance FromXML AccessKeyMetadata where
    parseXML x = AccessKeyMetadata
        <$> x .@? "AccessKeyId"
        <*> x .@? "CreateDate"
        <*> x .@? "Status"
        <*> x .@? "UserName"

instance ToQuery AccessKeyMetadata where
    toQuery AccessKeyMetadata{..} = mconcat
        [ "AccessKeyId" =? _akmAccessKeyId
        , "CreateDate"  =? _akmCreateDate
        , "Status"      =? _akmStatus
        , "UserName"    =? _akmUserName
        ]

newtype PolicyUser = PolicyUser
    { _puUserName :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'PolicyUser' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'puUserName' @::@ 'Maybe' 'Text'
--
policyUser :: PolicyUser
policyUser = PolicyUser
    { _puUserName = Nothing
    }

-- | The name (friendly name, not ARN) identifying the user.
puUserName :: Lens' PolicyUser (Maybe Text)
puUserName = lens _puUserName (\s a -> s { _puUserName = a })

instance FromXML PolicyUser where
    parseXML x = PolicyUser
        <$> x .@? "UserName"

instance ToQuery PolicyUser where
    toQuery PolicyUser{..} = mconcat
        [ "UserName" =? _puUserName
        ]
