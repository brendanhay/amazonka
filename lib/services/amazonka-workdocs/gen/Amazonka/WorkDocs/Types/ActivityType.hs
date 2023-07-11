{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WorkDocs.Types.ActivityType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.ActivityType
  ( ActivityType
      ( ..,
        ActivityType_DOCUMENT_ANNOTATION_ADDED,
        ActivityType_DOCUMENT_ANNOTATION_DELETED,
        ActivityType_DOCUMENT_CHECKED_IN,
        ActivityType_DOCUMENT_CHECKED_OUT,
        ActivityType_DOCUMENT_COMMENT_ADDED,
        ActivityType_DOCUMENT_COMMENT_DELETED,
        ActivityType_DOCUMENT_MOVED,
        ActivityType_DOCUMENT_RECYCLED,
        ActivityType_DOCUMENT_RENAMED,
        ActivityType_DOCUMENT_RESTORED,
        ActivityType_DOCUMENT_REVERTED,
        ActivityType_DOCUMENT_SHAREABLE_LINK_CREATED,
        ActivityType_DOCUMENT_SHAREABLE_LINK_PERMISSION_CHANGED,
        ActivityType_DOCUMENT_SHAREABLE_LINK_REMOVED,
        ActivityType_DOCUMENT_SHARED,
        ActivityType_DOCUMENT_SHARE_PERMISSION_CHANGED,
        ActivityType_DOCUMENT_UNSHARED,
        ActivityType_DOCUMENT_VERSION_DELETED,
        ActivityType_DOCUMENT_VERSION_DOWNLOADED,
        ActivityType_DOCUMENT_VERSION_UPLOADED,
        ActivityType_DOCUMENT_VERSION_VIEWED,
        ActivityType_FOLDER_CREATED,
        ActivityType_FOLDER_DELETED,
        ActivityType_FOLDER_MOVED,
        ActivityType_FOLDER_RECYCLED,
        ActivityType_FOLDER_RENAMED,
        ActivityType_FOLDER_RESTORED,
        ActivityType_FOLDER_SHAREABLE_LINK_CREATED,
        ActivityType_FOLDER_SHAREABLE_LINK_PERMISSION_CHANGED,
        ActivityType_FOLDER_SHAREABLE_LINK_REMOVED,
        ActivityType_FOLDER_SHARED,
        ActivityType_FOLDER_SHARE_PERMISSION_CHANGED,
        ActivityType_FOLDER_UNSHARED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ActivityType = ActivityType'
  { fromActivityType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern ActivityType_DOCUMENT_ANNOTATION_ADDED :: ActivityType
pattern ActivityType_DOCUMENT_ANNOTATION_ADDED = ActivityType' "DOCUMENT_ANNOTATION_ADDED"

pattern ActivityType_DOCUMENT_ANNOTATION_DELETED :: ActivityType
pattern ActivityType_DOCUMENT_ANNOTATION_DELETED = ActivityType' "DOCUMENT_ANNOTATION_DELETED"

pattern ActivityType_DOCUMENT_CHECKED_IN :: ActivityType
pattern ActivityType_DOCUMENT_CHECKED_IN = ActivityType' "DOCUMENT_CHECKED_IN"

pattern ActivityType_DOCUMENT_CHECKED_OUT :: ActivityType
pattern ActivityType_DOCUMENT_CHECKED_OUT = ActivityType' "DOCUMENT_CHECKED_OUT"

pattern ActivityType_DOCUMENT_COMMENT_ADDED :: ActivityType
pattern ActivityType_DOCUMENT_COMMENT_ADDED = ActivityType' "DOCUMENT_COMMENT_ADDED"

pattern ActivityType_DOCUMENT_COMMENT_DELETED :: ActivityType
pattern ActivityType_DOCUMENT_COMMENT_DELETED = ActivityType' "DOCUMENT_COMMENT_DELETED"

pattern ActivityType_DOCUMENT_MOVED :: ActivityType
pattern ActivityType_DOCUMENT_MOVED = ActivityType' "DOCUMENT_MOVED"

pattern ActivityType_DOCUMENT_RECYCLED :: ActivityType
pattern ActivityType_DOCUMENT_RECYCLED = ActivityType' "DOCUMENT_RECYCLED"

pattern ActivityType_DOCUMENT_RENAMED :: ActivityType
pattern ActivityType_DOCUMENT_RENAMED = ActivityType' "DOCUMENT_RENAMED"

pattern ActivityType_DOCUMENT_RESTORED :: ActivityType
pattern ActivityType_DOCUMENT_RESTORED = ActivityType' "DOCUMENT_RESTORED"

pattern ActivityType_DOCUMENT_REVERTED :: ActivityType
pattern ActivityType_DOCUMENT_REVERTED = ActivityType' "DOCUMENT_REVERTED"

pattern ActivityType_DOCUMENT_SHAREABLE_LINK_CREATED :: ActivityType
pattern ActivityType_DOCUMENT_SHAREABLE_LINK_CREATED = ActivityType' "DOCUMENT_SHAREABLE_LINK_CREATED"

pattern ActivityType_DOCUMENT_SHAREABLE_LINK_PERMISSION_CHANGED :: ActivityType
pattern ActivityType_DOCUMENT_SHAREABLE_LINK_PERMISSION_CHANGED = ActivityType' "DOCUMENT_SHAREABLE_LINK_PERMISSION_CHANGED"

pattern ActivityType_DOCUMENT_SHAREABLE_LINK_REMOVED :: ActivityType
pattern ActivityType_DOCUMENT_SHAREABLE_LINK_REMOVED = ActivityType' "DOCUMENT_SHAREABLE_LINK_REMOVED"

pattern ActivityType_DOCUMENT_SHARED :: ActivityType
pattern ActivityType_DOCUMENT_SHARED = ActivityType' "DOCUMENT_SHARED"

pattern ActivityType_DOCUMENT_SHARE_PERMISSION_CHANGED :: ActivityType
pattern ActivityType_DOCUMENT_SHARE_PERMISSION_CHANGED = ActivityType' "DOCUMENT_SHARE_PERMISSION_CHANGED"

pattern ActivityType_DOCUMENT_UNSHARED :: ActivityType
pattern ActivityType_DOCUMENT_UNSHARED = ActivityType' "DOCUMENT_UNSHARED"

pattern ActivityType_DOCUMENT_VERSION_DELETED :: ActivityType
pattern ActivityType_DOCUMENT_VERSION_DELETED = ActivityType' "DOCUMENT_VERSION_DELETED"

pattern ActivityType_DOCUMENT_VERSION_DOWNLOADED :: ActivityType
pattern ActivityType_DOCUMENT_VERSION_DOWNLOADED = ActivityType' "DOCUMENT_VERSION_DOWNLOADED"

pattern ActivityType_DOCUMENT_VERSION_UPLOADED :: ActivityType
pattern ActivityType_DOCUMENT_VERSION_UPLOADED = ActivityType' "DOCUMENT_VERSION_UPLOADED"

pattern ActivityType_DOCUMENT_VERSION_VIEWED :: ActivityType
pattern ActivityType_DOCUMENT_VERSION_VIEWED = ActivityType' "DOCUMENT_VERSION_VIEWED"

pattern ActivityType_FOLDER_CREATED :: ActivityType
pattern ActivityType_FOLDER_CREATED = ActivityType' "FOLDER_CREATED"

pattern ActivityType_FOLDER_DELETED :: ActivityType
pattern ActivityType_FOLDER_DELETED = ActivityType' "FOLDER_DELETED"

pattern ActivityType_FOLDER_MOVED :: ActivityType
pattern ActivityType_FOLDER_MOVED = ActivityType' "FOLDER_MOVED"

pattern ActivityType_FOLDER_RECYCLED :: ActivityType
pattern ActivityType_FOLDER_RECYCLED = ActivityType' "FOLDER_RECYCLED"

pattern ActivityType_FOLDER_RENAMED :: ActivityType
pattern ActivityType_FOLDER_RENAMED = ActivityType' "FOLDER_RENAMED"

pattern ActivityType_FOLDER_RESTORED :: ActivityType
pattern ActivityType_FOLDER_RESTORED = ActivityType' "FOLDER_RESTORED"

pattern ActivityType_FOLDER_SHAREABLE_LINK_CREATED :: ActivityType
pattern ActivityType_FOLDER_SHAREABLE_LINK_CREATED = ActivityType' "FOLDER_SHAREABLE_LINK_CREATED"

pattern ActivityType_FOLDER_SHAREABLE_LINK_PERMISSION_CHANGED :: ActivityType
pattern ActivityType_FOLDER_SHAREABLE_LINK_PERMISSION_CHANGED = ActivityType' "FOLDER_SHAREABLE_LINK_PERMISSION_CHANGED"

pattern ActivityType_FOLDER_SHAREABLE_LINK_REMOVED :: ActivityType
pattern ActivityType_FOLDER_SHAREABLE_LINK_REMOVED = ActivityType' "FOLDER_SHAREABLE_LINK_REMOVED"

pattern ActivityType_FOLDER_SHARED :: ActivityType
pattern ActivityType_FOLDER_SHARED = ActivityType' "FOLDER_SHARED"

pattern ActivityType_FOLDER_SHARE_PERMISSION_CHANGED :: ActivityType
pattern ActivityType_FOLDER_SHARE_PERMISSION_CHANGED = ActivityType' "FOLDER_SHARE_PERMISSION_CHANGED"

pattern ActivityType_FOLDER_UNSHARED :: ActivityType
pattern ActivityType_FOLDER_UNSHARED = ActivityType' "FOLDER_UNSHARED"

{-# COMPLETE
  ActivityType_DOCUMENT_ANNOTATION_ADDED,
  ActivityType_DOCUMENT_ANNOTATION_DELETED,
  ActivityType_DOCUMENT_CHECKED_IN,
  ActivityType_DOCUMENT_CHECKED_OUT,
  ActivityType_DOCUMENT_COMMENT_ADDED,
  ActivityType_DOCUMENT_COMMENT_DELETED,
  ActivityType_DOCUMENT_MOVED,
  ActivityType_DOCUMENT_RECYCLED,
  ActivityType_DOCUMENT_RENAMED,
  ActivityType_DOCUMENT_RESTORED,
  ActivityType_DOCUMENT_REVERTED,
  ActivityType_DOCUMENT_SHAREABLE_LINK_CREATED,
  ActivityType_DOCUMENT_SHAREABLE_LINK_PERMISSION_CHANGED,
  ActivityType_DOCUMENT_SHAREABLE_LINK_REMOVED,
  ActivityType_DOCUMENT_SHARED,
  ActivityType_DOCUMENT_SHARE_PERMISSION_CHANGED,
  ActivityType_DOCUMENT_UNSHARED,
  ActivityType_DOCUMENT_VERSION_DELETED,
  ActivityType_DOCUMENT_VERSION_DOWNLOADED,
  ActivityType_DOCUMENT_VERSION_UPLOADED,
  ActivityType_DOCUMENT_VERSION_VIEWED,
  ActivityType_FOLDER_CREATED,
  ActivityType_FOLDER_DELETED,
  ActivityType_FOLDER_MOVED,
  ActivityType_FOLDER_RECYCLED,
  ActivityType_FOLDER_RENAMED,
  ActivityType_FOLDER_RESTORED,
  ActivityType_FOLDER_SHAREABLE_LINK_CREATED,
  ActivityType_FOLDER_SHAREABLE_LINK_PERMISSION_CHANGED,
  ActivityType_FOLDER_SHAREABLE_LINK_REMOVED,
  ActivityType_FOLDER_SHARED,
  ActivityType_FOLDER_SHARE_PERMISSION_CHANGED,
  ActivityType_FOLDER_UNSHARED,
  ActivityType'
  #-}
