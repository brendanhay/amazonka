{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.Sum
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkDocs.Types.Sum where

import           Network.AWS.Prelude

data DocumentSourceType
    = Original
    | WithComments
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText DocumentSourceType where
    parser = takeLowerText >>= \case
        "original" -> pure Original
        "with_comments" -> pure WithComments
        e -> fromTextError $ "Failure parsing DocumentSourceType from value: '" <> e
           <> "'. Accepted values: original, with_comments"

instance ToText DocumentSourceType where
    toText = \case
        Original -> "ORIGINAL"
        WithComments -> "WITH_COMMENTS"

instance Hashable     DocumentSourceType
instance NFData       DocumentSourceType
instance ToByteString DocumentSourceType
instance ToQuery      DocumentSourceType
instance ToHeader     DocumentSourceType

instance FromJSON DocumentSourceType where
    parseJSON = parseJSONText "DocumentSourceType"

data DocumentStatusType
    = DSTActive
    | DSTInitialized
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText DocumentStatusType where
    parser = takeLowerText >>= \case
        "active" -> pure DSTActive
        "initialized" -> pure DSTInitialized
        e -> fromTextError $ "Failure parsing DocumentStatusType from value: '" <> e
           <> "'. Accepted values: active, initialized"

instance ToText DocumentStatusType where
    toText = \case
        DSTActive -> "ACTIVE"
        DSTInitialized -> "INITIALIZED"

instance Hashable     DocumentStatusType
instance NFData       DocumentStatusType
instance ToByteString DocumentStatusType
instance ToQuery      DocumentStatusType
instance ToHeader     DocumentStatusType

instance FromJSON DocumentStatusType where
    parseJSON = parseJSONText "DocumentStatusType"

data DocumentThumbnailType
    = Large
    | Small
    | SmallHq
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText DocumentThumbnailType where
    parser = takeLowerText >>= \case
        "large" -> pure Large
        "small" -> pure Small
        "small_hq" -> pure SmallHq
        e -> fromTextError $ "Failure parsing DocumentThumbnailType from value: '" <> e
           <> "'. Accepted values: large, small, small_hq"

instance ToText DocumentThumbnailType where
    toText = \case
        Large -> "LARGE"
        Small -> "SMALL"
        SmallHq -> "SMALL_HQ"

instance Hashable     DocumentThumbnailType
instance NFData       DocumentThumbnailType
instance ToByteString DocumentThumbnailType
instance ToQuery      DocumentThumbnailType
instance ToHeader     DocumentThumbnailType

instance FromJSON DocumentThumbnailType where
    parseJSON = parseJSONText "DocumentThumbnailType"

data DocumentVersionStatus =
    DVSActive
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText DocumentVersionStatus where
    parser = takeLowerText >>= \case
        "active" -> pure DVSActive
        e -> fromTextError $ "Failure parsing DocumentVersionStatus from value: '" <> e
           <> "'. Accepted values: active"

instance ToText DocumentVersionStatus where
    toText = \case
        DVSActive -> "ACTIVE"

instance Hashable     DocumentVersionStatus
instance NFData       DocumentVersionStatus
instance ToByteString DocumentVersionStatus
instance ToQuery      DocumentVersionStatus
instance ToHeader     DocumentVersionStatus

instance ToJSON DocumentVersionStatus where
    toJSON = toJSONText

data FolderContentType
    = FCTAll
    | FCTDocument
    | FCTFolder
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText FolderContentType where
    parser = takeLowerText >>= \case
        "all" -> pure FCTAll
        "document" -> pure FCTDocument
        "folder" -> pure FCTFolder
        e -> fromTextError $ "Failure parsing FolderContentType from value: '" <> e
           <> "'. Accepted values: all, document, folder"

instance ToText FolderContentType where
    toText = \case
        FCTAll -> "ALL"
        FCTDocument -> "DOCUMENT"
        FCTFolder -> "FOLDER"

instance Hashable     FolderContentType
instance NFData       FolderContentType
instance ToByteString FolderContentType
instance ToQuery      FolderContentType
instance ToHeader     FolderContentType

instance ToJSON FolderContentType where
    toJSON = toJSONText

data LocaleType
    = DE
    | Default
    | EN
    | ES
    | FR
    | JA
    | KO
    | PtBr
    | RU
    | ZhCn
    | ZhTw
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText LocaleType where
    parser = takeLowerText >>= \case
        "de" -> pure DE
        "default" -> pure Default
        "en" -> pure EN
        "es" -> pure ES
        "fr" -> pure FR
        "ja" -> pure JA
        "ko" -> pure KO
        "pt_br" -> pure PtBr
        "ru" -> pure RU
        "zh_cn" -> pure ZhCn
        "zh_tw" -> pure ZhTw
        e -> fromTextError $ "Failure parsing LocaleType from value: '" <> e
           <> "'. Accepted values: de, default, en, es, fr, ja, ko, pt_br, ru, zh_cn, zh_tw"

instance ToText LocaleType where
    toText = \case
        DE -> "de"
        Default -> "default"
        EN -> "en"
        ES -> "es"
        FR -> "fr"
        JA -> "ja"
        KO -> "ko"
        PtBr -> "pt_BR"
        RU -> "ru"
        ZhCn -> "zh_CN"
        ZhTw -> "zh_TW"

instance Hashable     LocaleType
instance NFData       LocaleType
instance ToByteString LocaleType
instance ToQuery      LocaleType
instance ToHeader     LocaleType

instance ToJSON LocaleType where
    toJSON = toJSONText

instance FromJSON LocaleType where
    parseJSON = parseJSONText "LocaleType"

data OrderType
    = Ascending
    | Descending
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText OrderType where
    parser = takeLowerText >>= \case
        "ascending" -> pure Ascending
        "descending" -> pure Descending
        e -> fromTextError $ "Failure parsing OrderType from value: '" <> e
           <> "'. Accepted values: ascending, descending"

instance ToText OrderType where
    toText = \case
        Ascending -> "ASCENDING"
        Descending -> "DESCENDING"

instance Hashable     OrderType
instance NFData       OrderType
instance ToByteString OrderType
instance ToQuery      OrderType
instance ToHeader     OrderType

instance ToJSON OrderType where
    toJSON = toJSONText

data PrincipalType
    = Anonymous
    | Group
    | Invite
    | Organization
    | User
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText PrincipalType where
    parser = takeLowerText >>= \case
        "anonymous" -> pure Anonymous
        "group" -> pure Group
        "invite" -> pure Invite
        "organization" -> pure Organization
        "user" -> pure User
        e -> fromTextError $ "Failure parsing PrincipalType from value: '" <> e
           <> "'. Accepted values: anonymous, group, invite, organization, user"

instance ToText PrincipalType where
    toText = \case
        Anonymous -> "ANONYMOUS"
        Group -> "GROUP"
        Invite -> "INVITE"
        Organization -> "ORGANIZATION"
        User -> "USER"

instance Hashable     PrincipalType
instance NFData       PrincipalType
instance ToByteString PrincipalType
instance ToQuery      PrincipalType
instance ToHeader     PrincipalType

instance ToJSON PrincipalType where
    toJSON = toJSONText

instance FromJSON PrincipalType where
    parseJSON = parseJSONText "PrincipalType"

data ResourceSortType
    = Date
    | Name
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ResourceSortType where
    parser = takeLowerText >>= \case
        "date" -> pure Date
        "name" -> pure Name
        e -> fromTextError $ "Failure parsing ResourceSortType from value: '" <> e
           <> "'. Accepted values: date, name"

instance ToText ResourceSortType where
    toText = \case
        Date -> "DATE"
        Name -> "NAME"

instance Hashable     ResourceSortType
instance NFData       ResourceSortType
instance ToByteString ResourceSortType
instance ToQuery      ResourceSortType
instance ToHeader     ResourceSortType

instance ToJSON ResourceSortType where
    toJSON = toJSONText

data ResourceStateType
    = RSTActive
    | RSTRecycled
    | RSTRecycling
    | RSTRestoring
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ResourceStateType where
    parser = takeLowerText >>= \case
        "active" -> pure RSTActive
        "recycled" -> pure RSTRecycled
        "recycling" -> pure RSTRecycling
        "restoring" -> pure RSTRestoring
        e -> fromTextError $ "Failure parsing ResourceStateType from value: '" <> e
           <> "'. Accepted values: active, recycled, recycling, restoring"

instance ToText ResourceStateType where
    toText = \case
        RSTActive -> "ACTIVE"
        RSTRecycled -> "RECYCLED"
        RSTRecycling -> "RECYCLING"
        RSTRestoring -> "RESTORING"

instance Hashable     ResourceStateType
instance NFData       ResourceStateType
instance ToByteString ResourceStateType
instance ToQuery      ResourceStateType
instance ToHeader     ResourceStateType

instance ToJSON ResourceStateType where
    toJSON = toJSONText

instance FromJSON ResourceStateType where
    parseJSON = parseJSONText "ResourceStateType"

data RolePermissionType
    = Direct
    | Inherited
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText RolePermissionType where
    parser = takeLowerText >>= \case
        "direct" -> pure Direct
        "inherited" -> pure Inherited
        e -> fromTextError $ "Failure parsing RolePermissionType from value: '" <> e
           <> "'. Accepted values: direct, inherited"

instance ToText RolePermissionType where
    toText = \case
        Direct -> "DIRECT"
        Inherited -> "INHERITED"

instance Hashable     RolePermissionType
instance NFData       RolePermissionType
instance ToByteString RolePermissionType
instance ToQuery      RolePermissionType
instance ToHeader     RolePermissionType

instance FromJSON RolePermissionType where
    parseJSON = parseJSONText "RolePermissionType"

data RoleType
    = Contributor
    | Coowner
    | Owner
    | Viewer
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText RoleType where
    parser = takeLowerText >>= \case
        "contributor" -> pure Contributor
        "coowner" -> pure Coowner
        "owner" -> pure Owner
        "viewer" -> pure Viewer
        e -> fromTextError $ "Failure parsing RoleType from value: '" <> e
           <> "'. Accepted values: contributor, coowner, owner, viewer"

instance ToText RoleType where
    toText = \case
        Contributor -> "CONTRIBUTOR"
        Coowner -> "COOWNER"
        Owner -> "OWNER"
        Viewer -> "VIEWER"

instance Hashable     RoleType
instance NFData       RoleType
instance ToByteString RoleType
instance ToQuery      RoleType
instance ToHeader     RoleType

instance ToJSON RoleType where
    toJSON = toJSONText

instance FromJSON RoleType where
    parseJSON = parseJSONText "RoleType"

data ShareStatusType
    = Failure
    | Success
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ShareStatusType where
    parser = takeLowerText >>= \case
        "failure" -> pure Failure
        "success" -> pure Success
        e -> fromTextError $ "Failure parsing ShareStatusType from value: '" <> e
           <> "'. Accepted values: failure, success"

instance ToText ShareStatusType where
    toText = \case
        Failure -> "FAILURE"
        Success -> "SUCCESS"

instance Hashable     ShareStatusType
instance NFData       ShareStatusType
instance ToByteString ShareStatusType
instance ToQuery      ShareStatusType
instance ToHeader     ShareStatusType

instance FromJSON ShareStatusType where
    parseJSON = parseJSONText "ShareStatusType"

data StorageType
    = Quota
    | Unlimited
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText StorageType where
    parser = takeLowerText >>= \case
        "quota" -> pure Quota
        "unlimited" -> pure Unlimited
        e -> fromTextError $ "Failure parsing StorageType from value: '" <> e
           <> "'. Accepted values: quota, unlimited"

instance ToText StorageType where
    toText = \case
        Quota -> "QUOTA"
        Unlimited -> "UNLIMITED"

instance Hashable     StorageType
instance NFData       StorageType
instance ToByteString StorageType
instance ToQuery      StorageType
instance ToHeader     StorageType

instance ToJSON StorageType where
    toJSON = toJSONText

instance FromJSON StorageType where
    parseJSON = parseJSONText "StorageType"

data SubscriptionProtocolType =
    HTTPS
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText SubscriptionProtocolType where
    parser = takeLowerText >>= \case
        "https" -> pure HTTPS
        e -> fromTextError $ "Failure parsing SubscriptionProtocolType from value: '" <> e
           <> "'. Accepted values: https"

instance ToText SubscriptionProtocolType where
    toText = \case
        HTTPS -> "HTTPS"

instance Hashable     SubscriptionProtocolType
instance NFData       SubscriptionProtocolType
instance ToByteString SubscriptionProtocolType
instance ToQuery      SubscriptionProtocolType
instance ToHeader     SubscriptionProtocolType

instance ToJSON SubscriptionProtocolType where
    toJSON = toJSONText

instance FromJSON SubscriptionProtocolType where
    parseJSON = parseJSONText "SubscriptionProtocolType"

data SubscriptionType =
    STAll
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText SubscriptionType where
    parser = takeLowerText >>= \case
        "all" -> pure STAll
        e -> fromTextError $ "Failure parsing SubscriptionType from value: '" <> e
           <> "'. Accepted values: all"

instance ToText SubscriptionType where
    toText = \case
        STAll -> "ALL"

instance Hashable     SubscriptionType
instance NFData       SubscriptionType
instance ToByteString SubscriptionType
instance ToQuery      SubscriptionType
instance ToHeader     SubscriptionType

instance ToJSON SubscriptionType where
    toJSON = toJSONText

data UserFilterType
    = ActivePending
    | All
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText UserFilterType where
    parser = takeLowerText >>= \case
        "active_pending" -> pure ActivePending
        "all" -> pure All
        e -> fromTextError $ "Failure parsing UserFilterType from value: '" <> e
           <> "'. Accepted values: active_pending, all"

instance ToText UserFilterType where
    toText = \case
        ActivePending -> "ACTIVE_PENDING"
        All -> "ALL"

instance Hashable     UserFilterType
instance NFData       UserFilterType
instance ToByteString UserFilterType
instance ToQuery      UserFilterType
instance ToHeader     UserFilterType

instance ToJSON UserFilterType where
    toJSON = toJSONText

data UserSortType
    = FullName
    | StorageLimit
    | StorageUsed
    | UserName
    | UserStatus
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText UserSortType where
    parser = takeLowerText >>= \case
        "full_name" -> pure FullName
        "storage_limit" -> pure StorageLimit
        "storage_used" -> pure StorageUsed
        "user_name" -> pure UserName
        "user_status" -> pure UserStatus
        e -> fromTextError $ "Failure parsing UserSortType from value: '" <> e
           <> "'. Accepted values: full_name, storage_limit, storage_used, user_name, user_status"

instance ToText UserSortType where
    toText = \case
        FullName -> "FULL_NAME"
        StorageLimit -> "STORAGE_LIMIT"
        StorageUsed -> "STORAGE_USED"
        UserName -> "USER_NAME"
        UserStatus -> "USER_STATUS"

instance Hashable     UserSortType
instance NFData       UserSortType
instance ToByteString UserSortType
instance ToQuery      UserSortType
instance ToHeader     UserSortType

instance ToJSON UserSortType where
    toJSON = toJSONText

data UserStatusType
    = Active
    | Inactive
    | Pending
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText UserStatusType where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "inactive" -> pure Inactive
        "pending" -> pure Pending
        e -> fromTextError $ "Failure parsing UserStatusType from value: '" <> e
           <> "'. Accepted values: active, inactive, pending"

instance ToText UserStatusType where
    toText = \case
        Active -> "ACTIVE"
        Inactive -> "INACTIVE"
        Pending -> "PENDING"

instance Hashable     UserStatusType
instance NFData       UserStatusType
instance ToByteString UserStatusType
instance ToQuery      UserStatusType
instance ToHeader     UserStatusType

instance FromJSON UserStatusType where
    parseJSON = parseJSONText "UserStatusType"

data UserType
    = UTAdmin
    | UTUser
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText UserType where
    parser = takeLowerText >>= \case
        "admin" -> pure UTAdmin
        "user" -> pure UTUser
        e -> fromTextError $ "Failure parsing UserType from value: '" <> e
           <> "'. Accepted values: admin, user"

instance ToText UserType where
    toText = \case
        UTAdmin -> "ADMIN"
        UTUser -> "USER"

instance Hashable     UserType
instance NFData       UserType
instance ToByteString UserType
instance ToQuery      UserType
instance ToHeader     UserType

instance ToJSON UserType where
    toJSON = toJSONText

instance FromJSON UserType where
    parseJSON = parseJSONText "UserType"
