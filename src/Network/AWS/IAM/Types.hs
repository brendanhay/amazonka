{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.Types
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.IAM.Types where

import Data.ByteString      (ByteString)
import Data.Monoid
import Data.Text            (Text)
import Data.Time
import Network.AWS.Internal

iamService :: Service
iamService = Service "iam" iamVersion SigningVersion4 $
    Global "iam.amazonaws.com"

-- | Currently supported version of the IAM service.
iamVersion :: ServiceVersion
iamVersion = "2010-05-08"

-- | XML namespace to annotate IAM elements with.
iamNS :: ByteString
iamNS = "https://iam.amazonaws.com/doc/" <> sPack iamVersion <> "/"

-- | Helper to define IAM namespaced XML elements.
iamElem :: ByteString -> NName ByteString
iamElem = mkNName iamNS

data ErrorType = ErrorType
    { etType    :: !Text
    , etCode    :: !Text
    , etMessage :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML ErrorType where
    xmlPickler = withNS iamNS

data IAMError = IAMError
    { erError     :: !ErrorType
    , erRequestId :: !Text
    } deriving (Eq, Show, Generic)

instance ToError IAMError where
    toError = Error . show

instance IsXML IAMError where
    xmlPickler = withNS iamNS

-- | The AccessKey data type contains information about an AWS access key. This
-- data type is used as a response element in the actions CreateAccessKey and
-- ListAccessKeys.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_AccessKey.html>
data AccessKey = AccessKey
    { akAccessKeyId     :: !Text
      -- ^ The ID for this access key.
    , akCreateDate      :: Maybe UTCTime
      -- ^ The date when the access key was created.
    , akSecretAccessKey :: !Text
      -- ^ The secret key used to sign requests.
    , akStatus          :: !Text
      -- ^ The status of the access key. Active means the key is valid for
      -- API calls, while Inactive means it is not.
    , akUserName        :: !Text
      -- ^ Name of the user the key is associated with.
    } deriving (Eq, Show, Generic)

instance IsQuery AccessKey

instance IsXML AccessKey where
    xmlPickler = withNS iamNS

-- | The AccessKey data type contains information about an AWS access key,
-- without its secret key. This data type is used as a response element in the
-- action ListAccessKeys.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_AccessKeyMetadata.html>
data AccessKeyMetadata = AccessKeyMetadata
    { akmAccessKeyId :: Maybe Text
      -- ^ The ID for this access key.
    , akmCreateDate  :: Maybe UTCTime
      -- ^ The date when the access key was created.
    , akmStatus      :: Maybe Text
      -- ^ The status of the access key. Active means the key is valid for
      -- API calls, while Inactive means it is not.
    , akmUserName    :: Maybe Text
      -- ^ Name of the user the key is associated with.
    } deriving (Eq, Show, Generic)

instance IsQuery AccessKeyMetadata

instance IsXML AccessKeyMetadata where
    xmlPickler = withNS iamNS

-- | Contains the result of a successful invocation of the CreateAccessKey
-- action.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateAccessKeyResult.html>
data CreateAccessKeyResult = CreateAccessKeyResult
    { cakrAccessKey :: !AccessKey
      -- ^ Information about the access key.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateAccessKeyResult

instance IsXML CreateAccessKeyResult where
    xmlPickler = withNS iamNS

-- | Contains the result of a successful invocation of the CreateGroup action.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateGroupResult.html>
data CreateGroupResult = CreateGroupResult
    { cgrGroup :: !Group
      -- ^ Information about the group.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateGroupResult

instance IsXML CreateGroupResult where
    xmlPickler = withNS iamNS

-- | Contains the result of a successful invocation of the CreateInstanceProfile
-- action.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateInstanceProfileResult.html>
data CreateInstanceProfileResult = CreateInstanceProfileResult
    { ciprInstanceProfile :: !InstanceProfile
      -- ^ Information about the instance profile.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateInstanceProfileResult

instance IsXML CreateInstanceProfileResult where
    xmlPickler = withNS iamNS

-- | Contains the result of a successful invocation of the CreateLoginProfile
-- action.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateLoginProfileResult.html>
data CreateLoginProfileResult = CreateLoginProfileResult
    { clprLoginProfile :: !LoginProfile
      -- ^ The user name and password create date.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateLoginProfileResult

instance IsXML CreateLoginProfileResult where
    xmlPickler = withNS iamNS

-- | Contains the result of a successful invocation of the CreateRole action.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRoleResult.html>
data CreateRoleResult = CreateRoleResult
    { crrRole :: !Role
      -- ^ Information about the role.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateRoleResult

instance IsXML CreateRoleResult where
    xmlPickler = withNS iamNS

-- | Contains the result of a successful invocation of the CreateUser action.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateUserResult.html>
data CreateUserResult = CreateUserResult
    { curUser :: Maybe User
      -- ^ Information about the user.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateUserResult

instance IsXML CreateUserResult where
    xmlPickler = withNS iamNS

-- | Contains the result of a successful invocation of the
-- CreateVirtualMFADevice action.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateVirtualMFADeviceResult.html>
data CreateVirtualMFADeviceResult = CreateVirtualMFADeviceResult
    { cvmfadrVirtualMFADevice :: !VirtualMFADevice
      -- ^ A newly created virtual MFA device.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateVirtualMFADeviceResult

instance IsXML CreateVirtualMFADeviceResult where
    xmlPickler = withNS iamNS

-- | Contains the result of a successful invocation of the
-- GetAccountPasswordPolicy action.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetAccountPasswordPolicyResult.html>
data GetAccountPasswordPolicyResult = GetAccountPasswordPolicyResult
    { gapprPasswordPolicy :: !PasswordPolicy
      -- ^ The PasswordPolicy data type contains information about the
      -- account password policy.
    } deriving (Eq, Show, Generic)

instance IsQuery GetAccountPasswordPolicyResult

instance IsXML GetAccountPasswordPolicyResult where
    xmlPickler = withNS iamNS

-- | Contains the result of a successful invocation of the GetAccountSummary
-- action.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetAccountSummaryResult.html>
data GetAccountSummaryResult = GetAccountSummaryResult
    { gasrSummaryMap :: Maybe Text
      -- ^ A set of key value pairs containing account-level information.
    } deriving (Eq, Show, Generic)

instance IsQuery GetAccountSummaryResult

instance IsXML GetAccountSummaryResult where
    xmlPickler = withNS iamNS

-- | Contains the result of a successful invocation of the GetGroupPolicy
-- action.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetGroupPolicyResult.html>
data GetGroupPolicyResult = GetGroupPolicyResult
    { ggprGroupName      :: !Text
      -- ^ The group the policy is associated with.
    , ggprPolicyDocument :: !Text
      -- ^ The policy document.
    , ggprPolicyName     :: !Text
      -- ^ The name of the policy.
    } deriving (Eq, Show, Generic)

instance IsQuery GetGroupPolicyResult

instance IsXML GetGroupPolicyResult where
    xmlPickler = withNS iamNS

-- | Contains the result of a successful invocation of the GetGroup action.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetGroupResult.html>
data GetGroupResult = GetGroupResult
    { ggrGroup       :: !Group
      -- ^ Information about the group.
    , ggrIsTruncated :: Maybe Bool
      -- ^ A flag that indicates whether there are more user names to list.
      -- If your results were truncated, you can make a subsequent
      -- pagination request using the Marker request parameter to retrieve
      -- more user names in the list.
    , ggrMarker      :: Maybe Text
      -- ^ If IsTruncated is true, then this element is present and contains
      -- the value to use for the Marker parameter in a subsequent
      -- pagination request.
    , ggrUsers       :: !User
      -- ^ A list of users in the group.
    } deriving (Eq, Show, Generic)

instance IsQuery GetGroupResult

instance IsXML GetGroupResult where
    xmlPickler = withNS iamNS

-- | Contains the result of a successful invocation of the GetInstanceProfile
-- action.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetInstanceProfileResult.html>
data GetInstanceProfileResult = GetInstanceProfileResult
    { giprInstanceProfile :: !InstanceProfile
      -- ^ Information about the instance profile.
    } deriving (Eq, Show, Generic)

instance IsQuery GetInstanceProfileResult

instance IsXML GetInstanceProfileResult where
    xmlPickler = withNS iamNS

-- | Contains the result of a successful invocation of the GetLoginProfile
-- action.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetLoginProfileResult.html>
data GetLoginProfileResult = GetLoginProfileResult
    { glprLoginProfile :: !LoginProfile
      -- ^ User name and password create date for the user.
    } deriving (Eq, Show, Generic)

instance IsQuery GetLoginProfileResult

instance IsXML GetLoginProfileResult where
    xmlPickler = withNS iamNS

-- | Contains the result of a successful invocation of the GetRolePolicy action.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetRolePolicyResult.html>
data GetRolePolicyResult = GetRolePolicyResult
    { grprPolicyDocument :: !Text
      -- ^ The policy document.
    , grprPolicyName     :: !Text
      -- ^ The name of the policy.
    , grprRoleName       :: !Text
      -- ^ The role the policy is associated with.
    } deriving (Eq, Show, Generic)

instance IsQuery GetRolePolicyResult

instance IsXML GetRolePolicyResult where
    xmlPickler = withNS iamNS

-- | Contains the result of a successful invocation of the GetRole action.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetRoleResult.html>
data GetRoleResult = GetRoleResult
    { grrRole :: !Role
      -- ^ Information about the role.
    } deriving (Eq, Show, Generic)

instance IsQuery GetRoleResult

instance IsXML GetRoleResult where
    xmlPickler = withNS iamNS

-- | Contains the result of a successful invocation of the GetServerCertificate
-- action.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetServerCertificateResult.html>
data GetServerCertificateResult = GetServerCertificateResult
    { gscrServerCertificate :: !ServerCertificate
      -- ^ Information about the server certificate.
    } deriving (Eq, Show, Generic)

instance IsQuery GetServerCertificateResult

instance IsXML GetServerCertificateResult where
    xmlPickler = withNS iamNS

-- | Contains the result of a successful invocation of the GetUserPolicy action.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetUserPolicyResult.html>
data GetUserPolicyResult = GetUserPolicyResult
    { guprPolicyDocument :: !Text
      -- ^ The policy document.
    , guprPolicyName     :: !Text
      -- ^ The name of the policy.
    , guprUserName       :: !Text
      -- ^ The user the policy is associated with.
    } deriving (Eq, Show, Generic)

instance IsQuery GetUserPolicyResult

instance IsXML GetUserPolicyResult where
    xmlPickler = withNS iamNS

-- | Contains the result of a successful invocation of the GetUser action.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetUserResult.html>
data GetUserResult = GetUserResult
    { gurUser :: !User
      -- ^ Information about the user.
    } deriving (Eq, Show, Generic)

instance IsQuery GetUserResult

instance IsXML GetUserResult where
    xmlPickler = withNS iamNS

-- | The Group data type contains information about a group. This data type is
-- used as a response element in the following actions:
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_Group.html>
data Group = Group
    { gArn        :: !Text
      -- ^ The Amazon Resource Name (ARN) specifying the group. For more
      -- information about ARNs and how to use them in policies, see
      -- Identifiers for IAM Entities in Using AWS Identity and Access
      -- Management.
    , gCreateDate :: !UTCTime
      -- ^ The date when the group was created.
    , gGroupId    :: !Text
      -- ^ The stable and unique string identifying the group. For more
      -- information about IDs, see Identifiers for IAM Entities in Using
      -- AWS Identity and Access Management.
    , gGroupName  :: !Text
      -- ^ The name that identifies the group.
    , gPath       :: !Text
      -- ^ Path to the group. For more information about paths, see
      -- Identifiers for IAM Entities in Using AWS Identity and Access
      -- Management.
    } deriving (Eq, Show, Generic)

instance IsQuery Group

instance IsXML Group where
    xmlPickler = withNS iamNS

-- | The InstanceProfile data type contains information about an instance
-- profile. This data type is used as a response element in the following
-- actions: CreateInstanceProfile GetInstanceProfile ListInstanceProfiles
-- ListInstanceProfilesForRole
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_InstanceProfile.html>
data InstanceProfile = InstanceProfile
    { ipArn                 :: !Text
      -- ^ The Amazon Resource Name (ARN) specifying the instance profile.
      -- For more information about ARNs and how to use them in policies,
      -- see Identifiers for IAM Entities in Using AWS Identity and Access
      -- Management.
    , ipCreateDate          :: !UTCTime
      -- ^ The date when the instance profile was created.
    , ipInstanceProfileId   :: !Text
      -- ^ The stable and unique string identifying the instance profile.
      -- For more information about IDs, see Identifiers for IAM Entities
      -- in Using AWS Identity and Access Management.
    , ipInstanceProfileName :: !Text
      -- ^ The name identifying the instance profile.
    , ipPath                :: !Text
      -- ^ Path to the instance profile. For more information about paths,
      -- see Identifiers for IAM Entities in Using AWS Identity and Access
      -- Management.
    , ipRoles               :: [Role]
      -- ^ The roles associated with the instance profile.
    } deriving (Eq, Show, Generic)

instance IsQuery InstanceProfile

instance IsXML InstanceProfile where
    xmlPickler = withNS iamNS

-- | Contains the result of a successful invocation of the ListAccessKeys
-- action.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListAccessKeysResult.html>
data ListAccessKeysResult = ListAccessKeysResult
    { lakrAccessKeyMetadata :: !AccessKeyMetadata
      -- ^ A list of access key metadata.
    , lakrIsTruncated       :: Maybe Bool
      -- ^ A flag that indicates whether there are more keys to list. If
      -- your results were truncated, you can make a subsequent pagination
      -- request using the Marker request parameter to retrieve more keys
      -- in the list.
    , lakrMarker            :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value to use for the Marker parameter in a subsequent pagination
      -- request.
    } deriving (Eq, Show, Generic)

instance IsQuery ListAccessKeysResult

instance IsXML ListAccessKeysResult where
    xmlPickler = withNS iamNS

-- | Contains the result of a successful invocation of the ListAccountAliases
-- action.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListAccountAliasesResult.html>
data ListAccountAliasesResult = ListAccountAliasesResult
    { laarAccountAliases :: !Text
      -- ^ A list of aliases associated with the account.
    , laarIsTruncated    :: Maybe Bool
      -- ^ A flag that indicates whether there are more account aliases to
      -- list. If your results were truncated, you can make a subsequent
      -- pagination request using the Marker request parameter to retrieve
      -- more account aliases in the list.
    , laarMarker         :: Maybe Text
      -- ^ Use this only when paginating results, and only in a subsequent
      -- request after you've received a response where the results are
      -- truncated. Set it to the value of the Marker element in the
      -- response you just received.
    } deriving (Eq, Show, Generic)

instance IsQuery ListAccountAliasesResult

instance IsXML ListAccountAliasesResult where
    xmlPickler = withNS iamNS

-- | Contains the result of a successful invocation of the ListGroupPolicies
-- action.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListGroupPoliciesResult.html>
data ListGroupPoliciesResult = ListGroupPoliciesResult
    { lgprIsTruncated :: Maybe Bool
      -- ^ A flag that indicates whether there are more policy names to
      -- list. If your results were truncated, you can make a subsequent
      -- pagination request using the Marker request parameter to retrieve
      -- more policy names in the list.
    , lgprMarker      :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value to use for the Marker parameter in a subsequent pagination
      -- request.
    , lgprPolicyNames :: !Text
      -- ^ A list of policy names.
    } deriving (Eq, Show, Generic)

instance IsQuery ListGroupPoliciesResult

instance IsXML ListGroupPoliciesResult where
    xmlPickler = withNS iamNS

-- | Contains the result of a successful invocation of the ListGroupsForUser
-- action.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListGroupsForUserResult.html>
data ListGroupsForUserResult = ListGroupsForUserResult
    { lgfurGroups      :: !Group
      -- ^ A list of groups.
    , lgfurIsTruncated :: Maybe Bool
      -- ^ A flag that indicates whether there are more groups to list. If
      -- your results were truncated, you can make a subsequent pagination
      -- request using the Marker request parameter to retrieve more
      -- groups in the list.
    , lgfurMarker      :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value to use for the Marker parameter in a subsequent pagination
      -- request.
    } deriving (Eq, Show, Generic)

instance IsQuery ListGroupsForUserResult

instance IsXML ListGroupsForUserResult where
    xmlPickler = withNS iamNS

-- | Contains the result of a successful invocation of the ListGroups action.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListGroupsResult.html>
data ListGroupsResult = ListGroupsResult
    { lgrGroups      :: !Group
      -- ^ A list of groups.
    , lgrIsTruncated :: Maybe Bool
      -- ^ A flag that indicates whether there are more groups to list. If
      -- your results were truncated, you can make a subsequent pagination
      -- request using the Marker request parameter to retrieve more
      -- groups in the list.
    , lgrMarker      :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value to use for the Marker parameter in a subsequent pagination
      -- request.
    } deriving (Eq, Show, Generic)

instance IsQuery ListGroupsResult

instance IsXML ListGroupsResult where
    xmlPickler = withNS iamNS

-- | Contains the result of a successful invocation of the
-- ListInstanceProfilesForRole action.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListInstanceProfilesForRoleResult.html>
data ListInstanceProfilesForRoleResult = ListInstanceProfilesForRoleResult
    { lipfrrInstanceProfiles :: !InstanceProfile
      -- ^ A list of instance profiles.
    , lipfrrIsTruncated      :: Maybe Bool
      -- ^ A flag that indicates whether there are more instance profiles to
      -- list. If your results were truncated, you can make a subsequent
      -- pagination request using the Marker request parameter to retrieve
      -- more instance profiles in the list.
    , lipfrrMarker           :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value to use for the Marker parameter in a subsequent pagination
      -- request.
    } deriving (Eq, Show, Generic)

instance IsQuery ListInstanceProfilesForRoleResult

instance IsXML ListInstanceProfilesForRoleResult where
    xmlPickler = withNS iamNS

-- | Contains the result of a successful invocation of the ListInstanceProfiles
-- action.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListInstanceProfilesResult.html>
data ListInstanceProfilesResult = ListInstanceProfilesResult
    { liprInstanceProfiles :: !InstanceProfile
      -- ^ A list of instance profiles.
    , liprIsTruncated      :: Maybe Bool
      -- ^ A flag that indicates whether there are more instance profiles to
      -- list. If your results were truncated, you can make a subsequent
      -- pagination request using the Marker request parameter to retrieve
      -- more instance profiles in the list.
    , liprMarker           :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value to use for the Marker parameter in a subsequent pagination
      -- request.
    } deriving (Eq, Show, Generic)

instance IsQuery ListInstanceProfilesResult

instance IsXML ListInstanceProfilesResult where
    xmlPickler = withNS iamNS

-- | Contains the result of a successful invocation of the ListMFADevices
-- action.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListMFADevicesResult.html>
data ListMFADevicesResult = ListMFADevicesResult
    { lmfadrIsTruncated :: Maybe Bool
      -- ^ A flag that indicates whether there are more MFA devices to list.
      -- If your results were truncated, you can make a subsequent
      -- pagination request using the Marker request parameter to retrieve
      -- more MFA devices in the list.
    , lmfadrMFADevices  :: !MFADevice
      -- ^ A list of MFA devices.
    , lmfadrMarker      :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value to use for the Marker parameter in a subsequent pagination
      -- request.
    } deriving (Eq, Show, Generic)

instance IsQuery ListMFADevicesResult

instance IsXML ListMFADevicesResult where
    xmlPickler = withNS iamNS

-- | Contains the result of a successful invocation of the ListRolePolicies
-- action.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListRolePoliciesResult.html>
data ListRolePoliciesResult = ListRolePoliciesResult
    { lrprIsTruncated :: Maybe Bool
      -- ^ A flag that indicates whether there are more policy names to
      -- list. If your results were truncated, you can make a subsequent
      -- pagination request using the Marker request parameter to retrieve
      -- more policy names in the list.
    , lrprMarker      :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value to use for the Marker parameter in a subsequent pagination
      -- request.
    , lrprPolicyNames :: !Text
      -- ^ A list of policy names.
    } deriving (Eq, Show, Generic)

instance IsQuery ListRolePoliciesResult

instance IsXML ListRolePoliciesResult where
    xmlPickler = withNS iamNS

-- | Contains the result of a successful invocation of the ListRoles action.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListRolesResult.html>
data ListRolesResult = ListRolesResult
    { lrrIsTruncated :: Maybe Bool
      -- ^ A flag that indicates whether there are more roles to list. If
      -- your results were truncated, you can make a subsequent pagination
      -- request using the Marker request parameter to retrieve more roles
      -- in the list.
    , lrrMarker      :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value to use for the Marker parameter in a subsequent pagination
      -- request.
    , lrrRoles       :: !Role
      -- ^ A list of roles.
    } deriving (Eq, Show, Generic)

instance IsQuery ListRolesResult

instance IsXML ListRolesResult where
    xmlPickler = withNS iamNS

-- | Contains the result of a successful invocation of the
-- ListServerCertificates action.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListServerCertificatesResult.html>
data ListServerCertificatesResult = ListServerCertificatesResult
    { lscrIsTruncated                   :: Maybe Bool
      -- ^ A flag that indicates whether there are more server certificates
      -- to list. If your results were truncated, you can make a
      -- subsequent pagination request using the Marker request parameter
      -- to retrieve more server certificates in the list.
    , lscrMarker                        :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value to use for the Marker parameter in a subsequent pagination
      -- request.
    , lscrServerCertificateMetadataList :: !ServerCertificateMetadata
      -- ^ A list of server certificates.
    } deriving (Eq, Show, Generic)

instance IsQuery ListServerCertificatesResult

instance IsXML ListServerCertificatesResult where
    xmlPickler = withNS iamNS

-- | Contains the result of a successful invocation of the
-- ListSigningCertificates action.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListSigningCertificatesResult.html>
data ListSigningCertificatesResult = ListSigningCertificatesResult
    { lscrCertificates :: !SigningCertificate
      -- ^ A list of the user's signing certificate information.
    , lscsIsTruncated  :: Maybe Bool
      -- ^ A flag that indicates whether there are more certificate IDs to
      -- list. If your results were truncated, you can make a subsequent
      -- pagination request using the Marker request parameter to retrieve
      -- more certificates in the list.
    , lscsMarker       :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value to use for the Marker parameter in a subsequent pagination
      -- request.
    } deriving (Eq, Show, Generic)

instance IsQuery ListSigningCertificatesResult

instance IsXML ListSigningCertificatesResult where
    xmlPickler = withNS iamNS

-- | Contains the result of a successful invocation of the ListUserPolicies
-- action.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListUserPoliciesResult.html>
data ListUserPoliciesResult = ListUserPoliciesResult
    { luprIsTruncated :: Maybe Bool
      -- ^ A flag that indicates whether there are more policy names to
      -- list. If your results were truncated, you can make a subsequent
      -- pagination request using the Marker request parameter to retrieve
      -- more policy names in the list.
    , luprMarker      :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value to use for the Marker parameter in a subsequent pagination
      -- request.
    , luprPolicyNames :: !Text
      -- ^ A list of policy names.
    } deriving (Eq, Show, Generic)

instance IsQuery ListUserPoliciesResult

instance IsXML ListUserPoliciesResult where
    xmlPickler = withNS iamNS

-- | Contains the result of a successful invocation of the ListUsers action.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListUsersResult.html>
data ListUsersResult = ListUsersResult
    { lurIsTruncated :: Maybe Bool
      -- ^ A flag that indicates whether there are more user names to list.
      -- If your results were truncated, you can make a subsequent
      -- pagination request using the Marker request parameter to retrieve
      -- more users in the list.
    , lurMarker      :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value to use for the Marker parameter in a subsequent pagination
      -- request.
    , lurUsers       :: !User
      -- ^ A list of users.
    } deriving (Eq, Show, Generic)

instance IsQuery ListUsersResult

instance IsXML ListUsersResult where
    xmlPickler = withNS iamNS

-- | Contains the result of a successful invocation of the ListVirtualMFADevices
-- action.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListVirtualMFADevicesResult.html>
data ListVirtualMFADevicesResult = ListVirtualMFADevicesResult
    { lvmfadrIsTruncated       :: Maybe Bool
      -- ^ A flag that indicates whether there are more items to list. If
      -- your results were truncated, you can make a subsequent pagination
      -- request using the Marker request parameter to retrieve more items
      -- the list.
    , lvmfadrMarker            :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value to use for the Marker parameter in a subsequent pagination
      -- request.
    , lvmfadrVirtualMFADevices :: !VirtualMFADevice
      -- ^ Type: VirtualMFADevice list
    } deriving (Eq, Show, Generic)

instance IsQuery ListVirtualMFADevicesResult

instance IsXML ListVirtualMFADevicesResult where
    xmlPickler = withNS iamNS

-- | The LoginProfile data type contains the user name and password create date
-- for a user. This data type is used as a response element in the actions
-- CreateLoginProfile and GetLoginProfile.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_LoginProfile.html>
data LoginProfile = LoginProfile
    { lpCreateDate :: !UTCTime
      -- ^ The date when the password for the user was created.
    , lpUserName   :: !Text
      -- ^ The name of the user, which can be used for signing into the AWS
      -- Management Console.
    } deriving (Eq, Show, Generic)

instance IsQuery LoginProfile

instance IsXML LoginProfile where
    xmlPickler = withNS iamNS

-- | The MFADevice data type contains information about an MFA device. This data
-- type is used as a response element in the action ListMFADevices.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_MFADevice.html>
data MFADevice = MFADevice
    { mfadEnableDate   :: !UTCTime
      -- ^ The date when the MFA device was enabled for the user.
    , mfadSerialNumber :: !Text
      -- ^ The serial number that uniquely identifies the MFA device. For
      -- virtual MFA devices, the serial number is the device ARN.
    , mfadUserName     :: !Text
      -- ^ The user with whom the MFA device is associated.
    } deriving (Eq, Show, Generic)

instance IsQuery MFADevice

instance IsXML MFADevice where
    xmlPickler = withNS iamNS

-- | The PasswordPolicy data type contains information about the account
-- password policy. This data type is used as a response element in the action
-- GetAccountPasswordPolicy.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_PasswordPolicy.html>
data PasswordPolicy = PasswordPolicy
    { ppAllowUsersToChangePassword :: Maybe Bool
      -- ^ Specifies whether to allow IAM users to change their own
      -- password.
    , ppMinimumPasswordLength      :: Maybe Integer
      -- ^ Minimum length to require for IAM user passwords.
    , ppRequireLowercaseCharacters :: Maybe Bool
      -- ^ Specifies whether to require lowercase characters for IAM user
      -- passwords.
    , ppRequireNumbers             :: Maybe Bool
      -- ^ Specifies whether to require numbers for IAM user passwords.
    , ppRequireSymbols             :: Maybe Bool
      -- ^ Specifies whether to require symbols for IAM user passwords.
    , ppRequireUppercaseCharacters :: Maybe Bool
      -- ^ Specifies whether to require uppercase characters for IAM user
      -- passwords.
    } deriving (Eq, Show, Generic)

instance IsQuery PasswordPolicy

instance IsXML PasswordPolicy where
    xmlPickler = withNS iamNS

-- | The Role data type contains information about a role. This data type is
-- used as a response element in the following actions: CreateRole GetRole
-- ListRoles
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_Role.html>
data Role = Role
    { rArn                      :: !Text
      -- ^ The Amazon Resource Name (ARN) specifying the role. For more
      -- information about ARNs and how to use them in policies, see
      -- Identifiers for IAM Entities in Using AWS Identity and Access
      -- Management.
    , rAssumeRolePolicyDocument :: Maybe Text
      -- ^ The policy that grants an entity permission to assume the role.
    , rCreateDate               :: !UTCTime
      -- ^ The date when the role was created.
    , rPath                     :: !Text
      -- ^ Path to the role. For more information about paths, see
      -- Identifiers for IAM Entities in Using AWS Identity and Access
      -- Management.
    , rRoleId                   :: !Text
      -- ^ The stable and unique string identifying the role. For more
      -- information about IDs, see Identifiers for IAM Entities in Using
      -- AWS Identity and Access Management.
    , rRoleName                 :: !Text
      -- ^ The name identifying the role.
    } deriving (Eq, Show, Generic)

instance IsQuery Role

instance IsXML Role where
    xmlPickler = withNS iamNS

-- | The ServerCertificate data type contains information about a server
-- certificate. This data type is used as a response element in the action
-- GetServerCertificate.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ServerCertificate.html>
data ServerCertificate = ServerCertificate
    { scCertificateBody           :: !Text
      -- ^ The contents of the public key certificate.
    , scCertificateChain          :: Maybe Text
      -- ^ The contents of the public key certificate chain.
    , scServerCertificateMetadata :: !ServerCertificateMetadata
      -- ^ The meta information of the server certificate, such as its name,
      -- path, ID, and ARN.
    } deriving (Eq, Show, Generic)

instance IsQuery ServerCertificate

instance IsXML ServerCertificate where
    xmlPickler = withNS iamNS

-- | ServerCertificateMetadata contains information about a server certificate
-- without its certificate body, certificate chain, and private key. This data
-- type is used as a response element in the action UploadServerCertificate
-- and ListServerCertificates.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ServerCertificateMetadata.html>
data ServerCertificateMetadata = ServerCertificateMetadata
    { scmArn                   :: !Text
      -- ^ The Amazon Resource Name (ARN) specifying the server certificate.
      -- For more information about ARNs and how to use them in policies,
      -- see Identifiers for IAM Entities in Using AWS Identity and Access
      -- Management.
    , scmPath                  :: !Text
      -- ^ Path to the server certificate. For more information about paths,
      -- see Identifiers for IAM Entities in Using AWS Identity and Access
      -- Management.
    , scmServerCertificateId   :: !Text
      -- ^ The stable and unique string identifying the server certificate.
      -- For more information about IDs, see Identifiers for IAM Entities
      -- in Using AWS Identity and Access Management.
    , scmServerCertificateName :: !Text
      -- ^ The name that identifies the server certificate.
    , scmUploadDate            :: Maybe UTCTime
      -- ^ The date when the server certificate was uploaded.
    } deriving (Eq, Show, Generic)

instance IsQuery ServerCertificateMetadata

instance IsXML ServerCertificateMetadata where
    xmlPickler = withNS iamNS

-- | The SigningCertificate data type contains information about an X.509
-- signing certificate. This data type is used as a response element in the
-- actions UploadSigningCertificate and ListSigningCertificates.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_SigningCertificate.html>
data SigningCertificate = SigningCertificate
    { sdCertificateBody :: !Text
      -- ^ The contents of the signing certificate.
    , sdCertificateId   :: !Text
      -- ^ The ID for the signing certificate.
    , sdStatus          :: !Text
      -- ^ The status of the signing certificate. Active means the key is
      -- valid for API calls, while Inactive means it is not.
    , sdUploadDate      :: Maybe UTCTime
      -- ^ The date when the signing certificate was uploaded.
    , sdUserName        :: !Text
      -- ^ Name of the user the signing certificate is associated with.
    } deriving (Eq, Show, Generic)

instance IsQuery SigningCertificate

instance IsXML SigningCertificate where
    xmlPickler = withNS iamNS

-- | Contains the result of a successful invocation of the
-- UploadServerCertificate action.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UploadServerCertificateResult.html>
data UploadServerCertificateResult = UploadServerCertificateResult
    { uscrServerCertificateMetadata :: Maybe ServerCertificateMetadata
      -- ^ The meta information of the uploaded server certificate without
      -- its certificate body, certificate chain, and private key.
    } deriving (Eq, Show, Generic)

instance IsQuery UploadServerCertificateResult

instance IsXML UploadServerCertificateResult where
    xmlPickler = withNS iamNS

-- | Contains the result of a successful invocation of the
-- UploadSigningCertificate action.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UploadSigningCertificateResult.html>
data UploadSigningCertificateResult = UploadSigningCertificateResult
    { uscrCertificate :: !SigningCertificate
      -- ^ Information about the certificate.
    } deriving (Eq, Show, Generic)

instance IsQuery UploadSigningCertificateResult

instance IsXML UploadSigningCertificateResult where
    xmlPickler = withNS iamNS

-- | The User data type contains information about a user. This data type is
-- used as a response element in the following actions: CreateUser GetUser
-- ListUsers
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_User.html>
data User = User
    { uArn        :: !Text
      -- ^ The Amazon Resource Name (ARN) specifying the user. For more
      -- information about ARNs and how to use them in policies, see
      -- Identifiers for IAM Entities in Using AWS Identity and Access
      -- Management.
    , uCreateDate :: !UTCTime
      -- ^ The date when the user was created.
    , uPath       :: !Text
      -- ^ Path to the user. For more information about paths, see
      -- Identifiers for IAM Entities in Using AWS Identity and Access
      -- Management.
    , uUserId     :: !Text
      -- ^ The stable and unique string identifying the user. For more
      -- information about IDs, see Identifiers for IAM Entities in Using
      -- AWS Identity and Access Management.
    , uUserName   :: !Text
      -- ^ The name identifying the user.
    } deriving (Eq, Show, Generic)

instance IsQuery User

instance IsXML User where
    xmlPickler = withNS iamNS

-- | The VirtualMFADevice data type contains information about a virtual MFA
-- device.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_VirtualMFADevice.html>
data VirtualMFADevice = VirtualMFADevice
    { vmfadBase32StringSeed :: Maybe Text
      -- ^ The Base32 seed defined as specified in RFC3548. The
      -- Base32StringSeed is Base64-encoded.
    , vmfadEnableDate       :: Maybe UTCTime
      -- ^ Type: DateTime
    , vmfadQRCodePNG        :: Maybe Text
      -- ^ A QR code PNG image that encodes
      -- otpauth://totp/$virtualMFADeviceName@$AccountName?
      -- secret=$Base32String where $virtualMFADeviceName is one of the
      -- create call arguments, AccountName is the user name if set
      -- (accountId otherwise), and Base32String is the seed in Base32
      -- format. The Base32String is Base64-encoded.
    , vmfadSerialNumber     :: !Text
      -- ^ The serial number associated with VirtualMFADevice.
    , vmfadUser             :: Maybe User
      -- ^ The User data type contains information about a user.
    } deriving (Eq, Show, Generic)

instance IsQuery VirtualMFADevice

instance IsXML VirtualMFADevice where
    xmlPickler = withNS iamNS
