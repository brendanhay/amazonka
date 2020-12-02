{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.ActivityType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.ActivityType where

import Network.AWS.Prelude

data ActivityType
  = DocumentAnnotationAdded
  | DocumentAnnotationDeleted
  | DocumentCheckedIn
  | DocumentCheckedOut
  | DocumentCommentAdded
  | DocumentCommentDeleted
  | DocumentMoved
  | DocumentRecycled
  | DocumentRenamed
  | DocumentRestored
  | DocumentReverted
  | DocumentSharePermissionChanged
  | DocumentShareableLinkCreated
  | DocumentShareableLinkPermissionChanged
  | DocumentShareableLinkRemoved
  | DocumentShared
  | DocumentUnshared
  | DocumentVersionDeleted
  | DocumentVersionDownloaded
  | DocumentVersionUploaded
  | DocumentVersionViewed
  | FolderCreated
  | FolderDeleted
  | FolderMoved
  | FolderRecycled
  | FolderRenamed
  | FolderRestored
  | FolderSharePermissionChanged
  | FolderShareableLinkCreated
  | FolderShareableLinkPermissionChanged
  | FolderShareableLinkRemoved
  | FolderShared
  | FolderUnshared
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText ActivityType where
  parser =
    takeLowerText >>= \case
      "document_annotation_added" -> pure DocumentAnnotationAdded
      "document_annotation_deleted" -> pure DocumentAnnotationDeleted
      "document_checked_in" -> pure DocumentCheckedIn
      "document_checked_out" -> pure DocumentCheckedOut
      "document_comment_added" -> pure DocumentCommentAdded
      "document_comment_deleted" -> pure DocumentCommentDeleted
      "document_moved" -> pure DocumentMoved
      "document_recycled" -> pure DocumentRecycled
      "document_renamed" -> pure DocumentRenamed
      "document_restored" -> pure DocumentRestored
      "document_reverted" -> pure DocumentReverted
      "document_share_permission_changed" -> pure DocumentSharePermissionChanged
      "document_shareable_link_created" -> pure DocumentShareableLinkCreated
      "document_shareable_link_permission_changed" -> pure DocumentShareableLinkPermissionChanged
      "document_shareable_link_removed" -> pure DocumentShareableLinkRemoved
      "document_shared" -> pure DocumentShared
      "document_unshared" -> pure DocumentUnshared
      "document_version_deleted" -> pure DocumentVersionDeleted
      "document_version_downloaded" -> pure DocumentVersionDownloaded
      "document_version_uploaded" -> pure DocumentVersionUploaded
      "document_version_viewed" -> pure DocumentVersionViewed
      "folder_created" -> pure FolderCreated
      "folder_deleted" -> pure FolderDeleted
      "folder_moved" -> pure FolderMoved
      "folder_recycled" -> pure FolderRecycled
      "folder_renamed" -> pure FolderRenamed
      "folder_restored" -> pure FolderRestored
      "folder_share_permission_changed" -> pure FolderSharePermissionChanged
      "folder_shareable_link_created" -> pure FolderShareableLinkCreated
      "folder_shareable_link_permission_changed" -> pure FolderShareableLinkPermissionChanged
      "folder_shareable_link_removed" -> pure FolderShareableLinkRemoved
      "folder_shared" -> pure FolderShared
      "folder_unshared" -> pure FolderUnshared
      e ->
        fromTextError $
          "Failure parsing ActivityType from value: '" <> e
            <> "'. Accepted values: document_annotation_added, document_annotation_deleted, document_checked_in, document_checked_out, document_comment_added, document_comment_deleted, document_moved, document_recycled, document_renamed, document_restored, document_reverted, document_share_permission_changed, document_shareable_link_created, document_shareable_link_permission_changed, document_shareable_link_removed, document_shared, document_unshared, document_version_deleted, document_version_downloaded, document_version_uploaded, document_version_viewed, folder_created, folder_deleted, folder_moved, folder_recycled, folder_renamed, folder_restored, folder_share_permission_changed, folder_shareable_link_created, folder_shareable_link_permission_changed, folder_shareable_link_removed, folder_shared, folder_unshared"

instance ToText ActivityType where
  toText = \case
    DocumentAnnotationAdded -> "DOCUMENT_ANNOTATION_ADDED"
    DocumentAnnotationDeleted -> "DOCUMENT_ANNOTATION_DELETED"
    DocumentCheckedIn -> "DOCUMENT_CHECKED_IN"
    DocumentCheckedOut -> "DOCUMENT_CHECKED_OUT"
    DocumentCommentAdded -> "DOCUMENT_COMMENT_ADDED"
    DocumentCommentDeleted -> "DOCUMENT_COMMENT_DELETED"
    DocumentMoved -> "DOCUMENT_MOVED"
    DocumentRecycled -> "DOCUMENT_RECYCLED"
    DocumentRenamed -> "DOCUMENT_RENAMED"
    DocumentRestored -> "DOCUMENT_RESTORED"
    DocumentReverted -> "DOCUMENT_REVERTED"
    DocumentSharePermissionChanged -> "DOCUMENT_SHARE_PERMISSION_CHANGED"
    DocumentShareableLinkCreated -> "DOCUMENT_SHAREABLE_LINK_CREATED"
    DocumentShareableLinkPermissionChanged -> "DOCUMENT_SHAREABLE_LINK_PERMISSION_CHANGED"
    DocumentShareableLinkRemoved -> "DOCUMENT_SHAREABLE_LINK_REMOVED"
    DocumentShared -> "DOCUMENT_SHARED"
    DocumentUnshared -> "DOCUMENT_UNSHARED"
    DocumentVersionDeleted -> "DOCUMENT_VERSION_DELETED"
    DocumentVersionDownloaded -> "DOCUMENT_VERSION_DOWNLOADED"
    DocumentVersionUploaded -> "DOCUMENT_VERSION_UPLOADED"
    DocumentVersionViewed -> "DOCUMENT_VERSION_VIEWED"
    FolderCreated -> "FOLDER_CREATED"
    FolderDeleted -> "FOLDER_DELETED"
    FolderMoved -> "FOLDER_MOVED"
    FolderRecycled -> "FOLDER_RECYCLED"
    FolderRenamed -> "FOLDER_RENAMED"
    FolderRestored -> "FOLDER_RESTORED"
    FolderSharePermissionChanged -> "FOLDER_SHARE_PERMISSION_CHANGED"
    FolderShareableLinkCreated -> "FOLDER_SHAREABLE_LINK_CREATED"
    FolderShareableLinkPermissionChanged -> "FOLDER_SHAREABLE_LINK_PERMISSION_CHANGED"
    FolderShareableLinkRemoved -> "FOLDER_SHAREABLE_LINK_REMOVED"
    FolderShared -> "FOLDER_SHARED"
    FolderUnshared -> "FOLDER_UNSHARED"

instance Hashable ActivityType

instance NFData ActivityType

instance ToByteString ActivityType

instance ToQuery ActivityType

instance ToHeader ActivityType

instance FromJSON ActivityType where
  parseJSON = parseJSONText "ActivityType"
