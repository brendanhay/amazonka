{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.ActivityType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.ActivityType
  ( ActivityType
      ( ActivityType',
        ActivityTypeDocumentCheckedIn,
        ActivityTypeDocumentCheckedOut,
        ActivityTypeDocumentRenamed,
        ActivityTypeDocumentVersionUploaded,
        ActivityTypeDocumentVersionDeleted,
        ActivityTypeDocumentVersionViewed,
        ActivityTypeDocumentVersionDownloaded,
        ActivityTypeDocumentRecycled,
        ActivityTypeDocumentRestored,
        ActivityTypeDocumentReverted,
        ActivityTypeDocumentShared,
        ActivityTypeDocumentUnshared,
        ActivityTypeDocumentSharePermissionChanged,
        ActivityTypeDocumentShareableLinkCreated,
        ActivityTypeDocumentShareableLinkRemoved,
        ActivityTypeDocumentShareableLinkPermissionChanged,
        ActivityTypeDocumentMoved,
        ActivityTypeDocumentCommentAdded,
        ActivityTypeDocumentCommentDeleted,
        ActivityTypeDocumentAnnotationAdded,
        ActivityTypeDocumentAnnotationDeleted,
        ActivityTypeFolderCreated,
        ActivityTypeFolderDeleted,
        ActivityTypeFolderRenamed,
        ActivityTypeFolderRecycled,
        ActivityTypeFolderRestored,
        ActivityTypeFolderShared,
        ActivityTypeFolderUnshared,
        ActivityTypeFolderSharePermissionChanged,
        ActivityTypeFolderShareableLinkCreated,
        ActivityTypeFolderShareableLinkRemoved,
        ActivityTypeFolderShareableLinkPermissionChanged,
        ActivityTypeFolderMoved,
        fromActivityType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ActivityType = ActivityType' {fromActivityType :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ActivityTypeDocumentCheckedIn :: ActivityType
pattern ActivityTypeDocumentCheckedIn = ActivityType' "DOCUMENT_CHECKED_IN"

pattern ActivityTypeDocumentCheckedOut :: ActivityType
pattern ActivityTypeDocumentCheckedOut = ActivityType' "DOCUMENT_CHECKED_OUT"

pattern ActivityTypeDocumentRenamed :: ActivityType
pattern ActivityTypeDocumentRenamed = ActivityType' "DOCUMENT_RENAMED"

pattern ActivityTypeDocumentVersionUploaded :: ActivityType
pattern ActivityTypeDocumentVersionUploaded = ActivityType' "DOCUMENT_VERSION_UPLOADED"

pattern ActivityTypeDocumentVersionDeleted :: ActivityType
pattern ActivityTypeDocumentVersionDeleted = ActivityType' "DOCUMENT_VERSION_DELETED"

pattern ActivityTypeDocumentVersionViewed :: ActivityType
pattern ActivityTypeDocumentVersionViewed = ActivityType' "DOCUMENT_VERSION_VIEWED"

pattern ActivityTypeDocumentVersionDownloaded :: ActivityType
pattern ActivityTypeDocumentVersionDownloaded = ActivityType' "DOCUMENT_VERSION_DOWNLOADED"

pattern ActivityTypeDocumentRecycled :: ActivityType
pattern ActivityTypeDocumentRecycled = ActivityType' "DOCUMENT_RECYCLED"

pattern ActivityTypeDocumentRestored :: ActivityType
pattern ActivityTypeDocumentRestored = ActivityType' "DOCUMENT_RESTORED"

pattern ActivityTypeDocumentReverted :: ActivityType
pattern ActivityTypeDocumentReverted = ActivityType' "DOCUMENT_REVERTED"

pattern ActivityTypeDocumentShared :: ActivityType
pattern ActivityTypeDocumentShared = ActivityType' "DOCUMENT_SHARED"

pattern ActivityTypeDocumentUnshared :: ActivityType
pattern ActivityTypeDocumentUnshared = ActivityType' "DOCUMENT_UNSHARED"

pattern ActivityTypeDocumentSharePermissionChanged :: ActivityType
pattern ActivityTypeDocumentSharePermissionChanged = ActivityType' "DOCUMENT_SHARE_PERMISSION_CHANGED"

pattern ActivityTypeDocumentShareableLinkCreated :: ActivityType
pattern ActivityTypeDocumentShareableLinkCreated = ActivityType' "DOCUMENT_SHAREABLE_LINK_CREATED"

pattern ActivityTypeDocumentShareableLinkRemoved :: ActivityType
pattern ActivityTypeDocumentShareableLinkRemoved = ActivityType' "DOCUMENT_SHAREABLE_LINK_REMOVED"

pattern ActivityTypeDocumentShareableLinkPermissionChanged :: ActivityType
pattern ActivityTypeDocumentShareableLinkPermissionChanged = ActivityType' "DOCUMENT_SHAREABLE_LINK_PERMISSION_CHANGED"

pattern ActivityTypeDocumentMoved :: ActivityType
pattern ActivityTypeDocumentMoved = ActivityType' "DOCUMENT_MOVED"

pattern ActivityTypeDocumentCommentAdded :: ActivityType
pattern ActivityTypeDocumentCommentAdded = ActivityType' "DOCUMENT_COMMENT_ADDED"

pattern ActivityTypeDocumentCommentDeleted :: ActivityType
pattern ActivityTypeDocumentCommentDeleted = ActivityType' "DOCUMENT_COMMENT_DELETED"

pattern ActivityTypeDocumentAnnotationAdded :: ActivityType
pattern ActivityTypeDocumentAnnotationAdded = ActivityType' "DOCUMENT_ANNOTATION_ADDED"

pattern ActivityTypeDocumentAnnotationDeleted :: ActivityType
pattern ActivityTypeDocumentAnnotationDeleted = ActivityType' "DOCUMENT_ANNOTATION_DELETED"

pattern ActivityTypeFolderCreated :: ActivityType
pattern ActivityTypeFolderCreated = ActivityType' "FOLDER_CREATED"

pattern ActivityTypeFolderDeleted :: ActivityType
pattern ActivityTypeFolderDeleted = ActivityType' "FOLDER_DELETED"

pattern ActivityTypeFolderRenamed :: ActivityType
pattern ActivityTypeFolderRenamed = ActivityType' "FOLDER_RENAMED"

pattern ActivityTypeFolderRecycled :: ActivityType
pattern ActivityTypeFolderRecycled = ActivityType' "FOLDER_RECYCLED"

pattern ActivityTypeFolderRestored :: ActivityType
pattern ActivityTypeFolderRestored = ActivityType' "FOLDER_RESTORED"

pattern ActivityTypeFolderShared :: ActivityType
pattern ActivityTypeFolderShared = ActivityType' "FOLDER_SHARED"

pattern ActivityTypeFolderUnshared :: ActivityType
pattern ActivityTypeFolderUnshared = ActivityType' "FOLDER_UNSHARED"

pattern ActivityTypeFolderSharePermissionChanged :: ActivityType
pattern ActivityTypeFolderSharePermissionChanged = ActivityType' "FOLDER_SHARE_PERMISSION_CHANGED"

pattern ActivityTypeFolderShareableLinkCreated :: ActivityType
pattern ActivityTypeFolderShareableLinkCreated = ActivityType' "FOLDER_SHAREABLE_LINK_CREATED"

pattern ActivityTypeFolderShareableLinkRemoved :: ActivityType
pattern ActivityTypeFolderShareableLinkRemoved = ActivityType' "FOLDER_SHAREABLE_LINK_REMOVED"

pattern ActivityTypeFolderShareableLinkPermissionChanged :: ActivityType
pattern ActivityTypeFolderShareableLinkPermissionChanged = ActivityType' "FOLDER_SHAREABLE_LINK_PERMISSION_CHANGED"

pattern ActivityTypeFolderMoved :: ActivityType
pattern ActivityTypeFolderMoved = ActivityType' "FOLDER_MOVED"

{-# COMPLETE
  ActivityTypeDocumentCheckedIn,
  ActivityTypeDocumentCheckedOut,
  ActivityTypeDocumentRenamed,
  ActivityTypeDocumentVersionUploaded,
  ActivityTypeDocumentVersionDeleted,
  ActivityTypeDocumentVersionViewed,
  ActivityTypeDocumentVersionDownloaded,
  ActivityTypeDocumentRecycled,
  ActivityTypeDocumentRestored,
  ActivityTypeDocumentReverted,
  ActivityTypeDocumentShared,
  ActivityTypeDocumentUnshared,
  ActivityTypeDocumentSharePermissionChanged,
  ActivityTypeDocumentShareableLinkCreated,
  ActivityTypeDocumentShareableLinkRemoved,
  ActivityTypeDocumentShareableLinkPermissionChanged,
  ActivityTypeDocumentMoved,
  ActivityTypeDocumentCommentAdded,
  ActivityTypeDocumentCommentDeleted,
  ActivityTypeDocumentAnnotationAdded,
  ActivityTypeDocumentAnnotationDeleted,
  ActivityTypeFolderCreated,
  ActivityTypeFolderDeleted,
  ActivityTypeFolderRenamed,
  ActivityTypeFolderRecycled,
  ActivityTypeFolderRestored,
  ActivityTypeFolderShared,
  ActivityTypeFolderUnshared,
  ActivityTypeFolderSharePermissionChanged,
  ActivityTypeFolderShareableLinkCreated,
  ActivityTypeFolderShareableLinkRemoved,
  ActivityTypeFolderShareableLinkPermissionChanged,
  ActivityTypeFolderMoved,
  ActivityType'
  #-}
