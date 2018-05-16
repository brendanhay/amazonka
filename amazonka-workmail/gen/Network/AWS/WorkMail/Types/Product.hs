{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkMail.Types.Product where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkMail.Types.Sum

-- | At least one delegate must be associated to the resource to disable automatic replies from the resource.
--
--
--
-- /See:/ 'bookingOptions' smart constructor.
data BookingOptions = BookingOptions'
  { _boAutoDeclineConflictingRequests :: !(Maybe Bool)
  , _boAutoDeclineRecurringRequests   :: !(Maybe Bool)
  , _boAutoAcceptRequests             :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BookingOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'boAutoDeclineConflictingRequests' - The resource's ability to automatically decline any conflicting requests.
--
-- * 'boAutoDeclineRecurringRequests' - The resource's ability to automatically decline any recurring requests.
--
-- * 'boAutoAcceptRequests' - The resource's ability to automatically reply to requests. If disabled, delegates must be associated to the resource.
bookingOptions
    :: BookingOptions
bookingOptions =
  BookingOptions'
    { _boAutoDeclineConflictingRequests = Nothing
    , _boAutoDeclineRecurringRequests = Nothing
    , _boAutoAcceptRequests = Nothing
    }


-- | The resource's ability to automatically decline any conflicting requests.
boAutoDeclineConflictingRequests :: Lens' BookingOptions (Maybe Bool)
boAutoDeclineConflictingRequests = lens _boAutoDeclineConflictingRequests (\ s a -> s{_boAutoDeclineConflictingRequests = a})

-- | The resource's ability to automatically decline any recurring requests.
boAutoDeclineRecurringRequests :: Lens' BookingOptions (Maybe Bool)
boAutoDeclineRecurringRequests = lens _boAutoDeclineRecurringRequests (\ s a -> s{_boAutoDeclineRecurringRequests = a})

-- | The resource's ability to automatically reply to requests. If disabled, delegates must be associated to the resource.
boAutoAcceptRequests :: Lens' BookingOptions (Maybe Bool)
boAutoAcceptRequests = lens _boAutoAcceptRequests (\ s a -> s{_boAutoAcceptRequests = a})

instance FromJSON BookingOptions where
        parseJSON
          = withObject "BookingOptions"
              (\ x ->
                 BookingOptions' <$>
                   (x .:? "AutoDeclineConflictingRequests") <*>
                     (x .:? "AutoDeclineRecurringRequests")
                     <*> (x .:? "AutoAcceptRequests"))

instance Hashable BookingOptions where

instance NFData BookingOptions where

instance ToJSON BookingOptions where
        toJSON BookingOptions'{..}
          = object
              (catMaybes
                 [("AutoDeclineConflictingRequests" .=) <$>
                    _boAutoDeclineConflictingRequests,
                  ("AutoDeclineRecurringRequests" .=) <$>
                    _boAutoDeclineRecurringRequests,
                  ("AutoAcceptRequests" .=) <$> _boAutoAcceptRequests])

-- | The name of the attribute, which is one of the values defined in the UserAttribute enumeration.
--
--
--
-- /See:/ 'delegate' smart constructor.
data Delegate = Delegate'
  { _dId   :: !Text
  , _dType :: !MemberType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Delegate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dId' - The identifier for the user or group is associated as the resource's delegate.
--
-- * 'dType' - The type of the delegate: user or group.
delegate
    :: Text -- ^ 'dId'
    -> MemberType -- ^ 'dType'
    -> Delegate
delegate pId_ pType_ = Delegate' {_dId = pId_, _dType = pType_}


-- | The identifier for the user or group is associated as the resource's delegate.
dId :: Lens' Delegate Text
dId = lens _dId (\ s a -> s{_dId = a})

-- | The type of the delegate: user or group.
dType :: Lens' Delegate MemberType
dType = lens _dType (\ s a -> s{_dType = a})

instance FromJSON Delegate where
        parseJSON
          = withObject "Delegate"
              (\ x -> Delegate' <$> (x .: "Id") <*> (x .: "Type"))

instance Hashable Delegate where

instance NFData Delegate where

-- | The representation of an Amazon WorkMail group.
--
--
--
-- /See:/ 'group'' smart constructor.
data Group = Group'
  { _gEmail        :: !(Maybe Text)
  , _gState        :: !(Maybe EntityState)
  , _gDisabledDate :: !(Maybe POSIX)
  , _gName         :: !(Maybe Text)
  , _gId           :: !(Maybe Text)
  , _gEnabledDate  :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Group' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gEmail' - The email of the group.
--
-- * 'gState' - The state of the group, which can be ENABLED, DISABLED, or DELETED.
--
-- * 'gDisabledDate' - The date indicating when the group was disabled from Amazon WorkMail use.
--
-- * 'gName' - The name of the group.
--
-- * 'gId' - The identifier of the group.
--
-- * 'gEnabledDate' - The date indicating when the group was enabled for Amazon WorkMail use.
group'
    :: Group
group' =
  Group'
    { _gEmail = Nothing
    , _gState = Nothing
    , _gDisabledDate = Nothing
    , _gName = Nothing
    , _gId = Nothing
    , _gEnabledDate = Nothing
    }


-- | The email of the group.
gEmail :: Lens' Group (Maybe Text)
gEmail = lens _gEmail (\ s a -> s{_gEmail = a})

-- | The state of the group, which can be ENABLED, DISABLED, or DELETED.
gState :: Lens' Group (Maybe EntityState)
gState = lens _gState (\ s a -> s{_gState = a})

-- | The date indicating when the group was disabled from Amazon WorkMail use.
gDisabledDate :: Lens' Group (Maybe UTCTime)
gDisabledDate = lens _gDisabledDate (\ s a -> s{_gDisabledDate = a}) . mapping _Time

-- | The name of the group.
gName :: Lens' Group (Maybe Text)
gName = lens _gName (\ s a -> s{_gName = a})

-- | The identifier of the group.
gId :: Lens' Group (Maybe Text)
gId = lens _gId (\ s a -> s{_gId = a})

-- | The date indicating when the group was enabled for Amazon WorkMail use.
gEnabledDate :: Lens' Group (Maybe UTCTime)
gEnabledDate = lens _gEnabledDate (\ s a -> s{_gEnabledDate = a}) . mapping _Time

instance FromJSON Group where
        parseJSON
          = withObject "Group"
              (\ x ->
                 Group' <$>
                   (x .:? "Email") <*> (x .:? "State") <*>
                     (x .:? "DisabledDate")
                     <*> (x .:? "Name")
                     <*> (x .:? "Id")
                     <*> (x .:? "EnabledDate"))

instance Hashable Group where

instance NFData Group where

-- | The representation of a group member (user or group).
--
--
--
-- /See:/ 'member' smart constructor.
data Member = Member'
  { _mState        :: !(Maybe EntityState)
  , _mDisabledDate :: !(Maybe POSIX)
  , _mName         :: !(Maybe Text)
  , _mId           :: !(Maybe Text)
  , _mType         :: !(Maybe MemberType)
  , _mEnabledDate  :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Member' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mState' - The state of the member, which can be ENABLED, DISABLED, or DELETED.
--
-- * 'mDisabledDate' - The date indicating when the member was disabled from Amazon WorkMail use.
--
-- * 'mName' - The name of the member.
--
-- * 'mId' - The identifier of the member.
--
-- * 'mType' - A member can be a user or group.
--
-- * 'mEnabledDate' - The date indicating when the member was enabled for Amazon WorkMail use.
member
    :: Member
member =
  Member'
    { _mState = Nothing
    , _mDisabledDate = Nothing
    , _mName = Nothing
    , _mId = Nothing
    , _mType = Nothing
    , _mEnabledDate = Nothing
    }


-- | The state of the member, which can be ENABLED, DISABLED, or DELETED.
mState :: Lens' Member (Maybe EntityState)
mState = lens _mState (\ s a -> s{_mState = a})

-- | The date indicating when the member was disabled from Amazon WorkMail use.
mDisabledDate :: Lens' Member (Maybe UTCTime)
mDisabledDate = lens _mDisabledDate (\ s a -> s{_mDisabledDate = a}) . mapping _Time

-- | The name of the member.
mName :: Lens' Member (Maybe Text)
mName = lens _mName (\ s a -> s{_mName = a})

-- | The identifier of the member.
mId :: Lens' Member (Maybe Text)
mId = lens _mId (\ s a -> s{_mId = a})

-- | A member can be a user or group.
mType :: Lens' Member (Maybe MemberType)
mType = lens _mType (\ s a -> s{_mType = a})

-- | The date indicating when the member was enabled for Amazon WorkMail use.
mEnabledDate :: Lens' Member (Maybe UTCTime)
mEnabledDate = lens _mEnabledDate (\ s a -> s{_mEnabledDate = a}) . mapping _Time

instance FromJSON Member where
        parseJSON
          = withObject "Member"
              (\ x ->
                 Member' <$>
                   (x .:? "State") <*> (x .:? "DisabledDate") <*>
                     (x .:? "Name")
                     <*> (x .:? "Id")
                     <*> (x .:? "Type")
                     <*> (x .:? "EnabledDate"))

instance Hashable Member where

instance NFData Member where

-- | The brief overview associated with an organization.
--
--
--
-- /See:/ 'organizationSummary' smart constructor.
data OrganizationSummary = OrganizationSummary'
  { _osState          :: !(Maybe Text)
  , _osAlias          :: !(Maybe Text)
  , _osErrorMessage   :: !(Maybe Text)
  , _osOrganizationId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OrganizationSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osState' - The state associated with the organization.
--
-- * 'osAlias' - The alias associated with the organization.
--
-- * 'osErrorMessage' - The error message associated with the organization. It is only present if unexpected behavior has occurred with regards to the organization. It provides insight or solutions regarding unexpected behavior.
--
-- * 'osOrganizationId' - The identifier associated with the organization.
organizationSummary
    :: OrganizationSummary
organizationSummary =
  OrganizationSummary'
    { _osState = Nothing
    , _osAlias = Nothing
    , _osErrorMessage = Nothing
    , _osOrganizationId = Nothing
    }


-- | The state associated with the organization.
osState :: Lens' OrganizationSummary (Maybe Text)
osState = lens _osState (\ s a -> s{_osState = a})

-- | The alias associated with the organization.
osAlias :: Lens' OrganizationSummary (Maybe Text)
osAlias = lens _osAlias (\ s a -> s{_osAlias = a})

-- | The error message associated with the organization. It is only present if unexpected behavior has occurred with regards to the organization. It provides insight or solutions regarding unexpected behavior.
osErrorMessage :: Lens' OrganizationSummary (Maybe Text)
osErrorMessage = lens _osErrorMessage (\ s a -> s{_osErrorMessage = a})

-- | The identifier associated with the organization.
osOrganizationId :: Lens' OrganizationSummary (Maybe Text)
osOrganizationId = lens _osOrganizationId (\ s a -> s{_osOrganizationId = a})

instance FromJSON OrganizationSummary where
        parseJSON
          = withObject "OrganizationSummary"
              (\ x ->
                 OrganizationSummary' <$>
                   (x .:? "State") <*> (x .:? "Alias") <*>
                     (x .:? "ErrorMessage")
                     <*> (x .:? "OrganizationId"))

instance Hashable OrganizationSummary where

instance NFData OrganizationSummary where

-- | Permission granted to an entity (user, group) to access a certain aspect of another entity's mailbox.
--
--
--
-- /See:/ 'permission' smart constructor.
data Permission = Permission'
  { _pGranteeId        :: !Text
  , _pGranteeType      :: !MemberType
  , _pPermissionValues :: ![PermissionType]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Permission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pGranteeId' - The identifier of the entity (user or group) to which the permissions are granted.
--
-- * 'pGranteeType' - The type of entity (user, group) of the entity referred to in GranteeId.
--
-- * 'pPermissionValues' - The permissions granted to the grantee. SEND_AS allows the grantee to send email as the owner of the mailbox (the grantee is not mentioned on these emails). SEND_ON_BEHALF allows the grantee to send email on behalf of the owner of the mailbox (the grantee is not mentioned as the physical sender of these emails). FULL_ACCESS allows the grantee full access to the mailbox, irrespective of other folder-level permissions set on the mailbox.
permission
    :: Text -- ^ 'pGranteeId'
    -> MemberType -- ^ 'pGranteeType'
    -> Permission
permission pGranteeId_ pGranteeType_ =
  Permission'
    { _pGranteeId = pGranteeId_
    , _pGranteeType = pGranteeType_
    , _pPermissionValues = mempty
    }


-- | The identifier of the entity (user or group) to which the permissions are granted.
pGranteeId :: Lens' Permission Text
pGranteeId = lens _pGranteeId (\ s a -> s{_pGranteeId = a})

-- | The type of entity (user, group) of the entity referred to in GranteeId.
pGranteeType :: Lens' Permission MemberType
pGranteeType = lens _pGranteeType (\ s a -> s{_pGranteeType = a})

-- | The permissions granted to the grantee. SEND_AS allows the grantee to send email as the owner of the mailbox (the grantee is not mentioned on these emails). SEND_ON_BEHALF allows the grantee to send email on behalf of the owner of the mailbox (the grantee is not mentioned as the physical sender of these emails). FULL_ACCESS allows the grantee full access to the mailbox, irrespective of other folder-level permissions set on the mailbox.
pPermissionValues :: Lens' Permission [PermissionType]
pPermissionValues = lens _pPermissionValues (\ s a -> s{_pPermissionValues = a}) . _Coerce

instance FromJSON Permission where
        parseJSON
          = withObject "Permission"
              (\ x ->
                 Permission' <$>
                   (x .: "GranteeId") <*> (x .: "GranteeType") <*>
                     (x .:? "PermissionValues" .!= mempty))

instance Hashable Permission where

instance NFData Permission where

-- | The overview for a resource containing relevant data regarding it.
--
--
--
-- /See:/ 'resource' smart constructor.
data Resource = Resource'
  { _rEmail        :: !(Maybe Text)
  , _rState        :: !(Maybe EntityState)
  , _rDisabledDate :: !(Maybe POSIX)
  , _rName         :: !(Maybe Text)
  , _rId           :: !(Maybe Text)
  , _rType         :: !(Maybe ResourceType)
  , _rEnabledDate  :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Resource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rEmail' - The email of the resource.
--
-- * 'rState' - The state of the resource, which can be ENABLED, DISABLED, or DELETED.
--
-- * 'rDisabledDate' - The date indicating when the resource was disabled from Amazon WorkMail use.
--
-- * 'rName' - The name of the resource.
--
-- * 'rId' - The identifier of the resource.
--
-- * 'rType' - The type of the resource: equipment or room.
--
-- * 'rEnabledDate' - The date indicating when the resource was enabled for Amazon WorkMail use.
resource
    :: Resource
resource =
  Resource'
    { _rEmail = Nothing
    , _rState = Nothing
    , _rDisabledDate = Nothing
    , _rName = Nothing
    , _rId = Nothing
    , _rType = Nothing
    , _rEnabledDate = Nothing
    }


-- | The email of the resource.
rEmail :: Lens' Resource (Maybe Text)
rEmail = lens _rEmail (\ s a -> s{_rEmail = a})

-- | The state of the resource, which can be ENABLED, DISABLED, or DELETED.
rState :: Lens' Resource (Maybe EntityState)
rState = lens _rState (\ s a -> s{_rState = a})

-- | The date indicating when the resource was disabled from Amazon WorkMail use.
rDisabledDate :: Lens' Resource (Maybe UTCTime)
rDisabledDate = lens _rDisabledDate (\ s a -> s{_rDisabledDate = a}) . mapping _Time

-- | The name of the resource.
rName :: Lens' Resource (Maybe Text)
rName = lens _rName (\ s a -> s{_rName = a})

-- | The identifier of the resource.
rId :: Lens' Resource (Maybe Text)
rId = lens _rId (\ s a -> s{_rId = a})

-- | The type of the resource: equipment or room.
rType :: Lens' Resource (Maybe ResourceType)
rType = lens _rType (\ s a -> s{_rType = a})

-- | The date indicating when the resource was enabled for Amazon WorkMail use.
rEnabledDate :: Lens' Resource (Maybe UTCTime)
rEnabledDate = lens _rEnabledDate (\ s a -> s{_rEnabledDate = a}) . mapping _Time

instance FromJSON Resource where
        parseJSON
          = withObject "Resource"
              (\ x ->
                 Resource' <$>
                   (x .:? "Email") <*> (x .:? "State") <*>
                     (x .:? "DisabledDate")
                     <*> (x .:? "Name")
                     <*> (x .:? "Id")
                     <*> (x .:? "Type")
                     <*> (x .:? "EnabledDate"))

instance Hashable Resource where

instance NFData Resource where

-- | The representation of an Amazon WorkMail user.
--
--
--
-- /See:/ 'user' smart constructor.
data User = User'
  { _uEmail        :: !(Maybe Text)
  , _uState        :: !(Maybe EntityState)
  , _uDisabledDate :: !(Maybe POSIX)
  , _uName         :: !(Maybe Text)
  , _uId           :: !(Maybe Text)
  , _uDisplayName  :: !(Maybe Text)
  , _uUserRole     :: !(Maybe UserRole)
  , _uEnabledDate  :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'User' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uEmail' - The email of the user.
--
-- * 'uState' - The state of the user, which can be ENABLED, DISABLED, or DELETED.
--
-- * 'uDisabledDate' - The date indicating when the user was disabled from Amazon WorkMail use.
--
-- * 'uName' - The name of the user.
--
-- * 'uId' - The identifier of the user.
--
-- * 'uDisplayName' - The display name of the user.
--
-- * 'uUserRole' - The role of the user.
--
-- * 'uEnabledDate' - The date indicating when the user was enabled for Amazon WorkMail use.
user
    :: User
user =
  User'
    { _uEmail = Nothing
    , _uState = Nothing
    , _uDisabledDate = Nothing
    , _uName = Nothing
    , _uId = Nothing
    , _uDisplayName = Nothing
    , _uUserRole = Nothing
    , _uEnabledDate = Nothing
    }


-- | The email of the user.
uEmail :: Lens' User (Maybe Text)
uEmail = lens _uEmail (\ s a -> s{_uEmail = a})

-- | The state of the user, which can be ENABLED, DISABLED, or DELETED.
uState :: Lens' User (Maybe EntityState)
uState = lens _uState (\ s a -> s{_uState = a})

-- | The date indicating when the user was disabled from Amazon WorkMail use.
uDisabledDate :: Lens' User (Maybe UTCTime)
uDisabledDate = lens _uDisabledDate (\ s a -> s{_uDisabledDate = a}) . mapping _Time

-- | The name of the user.
uName :: Lens' User (Maybe Text)
uName = lens _uName (\ s a -> s{_uName = a})

-- | The identifier of the user.
uId :: Lens' User (Maybe Text)
uId = lens _uId (\ s a -> s{_uId = a})

-- | The display name of the user.
uDisplayName :: Lens' User (Maybe Text)
uDisplayName = lens _uDisplayName (\ s a -> s{_uDisplayName = a})

-- | The role of the user.
uUserRole :: Lens' User (Maybe UserRole)
uUserRole = lens _uUserRole (\ s a -> s{_uUserRole = a})

-- | The date indicating when the user was enabled for Amazon WorkMail use.
uEnabledDate :: Lens' User (Maybe UTCTime)
uEnabledDate = lens _uEnabledDate (\ s a -> s{_uEnabledDate = a}) . mapping _Time

instance FromJSON User where
        parseJSON
          = withObject "User"
              (\ x ->
                 User' <$>
                   (x .:? "Email") <*> (x .:? "State") <*>
                     (x .:? "DisabledDate")
                     <*> (x .:? "Name")
                     <*> (x .:? "Id")
                     <*> (x .:? "DisplayName")
                     <*> (x .:? "UserRole")
                     <*> (x .:? "EnabledDate"))

instance Hashable User where

instance NFData User where
