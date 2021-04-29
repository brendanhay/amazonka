{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.Activity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.Activity where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WorkDocs.Types.ActivityType
import Network.AWS.WorkDocs.Types.CommentMetadata
import Network.AWS.WorkDocs.Types.Participants
import Network.AWS.WorkDocs.Types.ResourceMetadata
import Network.AWS.WorkDocs.Types.UserMetadata

-- | Describes the activity information.
--
-- /See:/ 'newActivity' smart constructor.
data Activity = Activity'
  { -- | The metadata of the resource involved in the user action.
    resourceMetadata :: Prelude.Maybe ResourceMetadata,
    -- | The ID of the organization.
    organizationId :: Prelude.Maybe Prelude.Text,
    -- | The original parent of the resource. This is an optional field and is
    -- filled for move activities.
    originalParent :: Prelude.Maybe ResourceMetadata,
    -- | The list of users or groups impacted by this action. This is an optional
    -- field and is filled for the following sharing activities:
    -- DOCUMENT_SHARED, DOCUMENT_SHARED, DOCUMENT_UNSHARED, FOLDER_SHARED,
    -- FOLDER_UNSHARED.
    participants :: Prelude.Maybe Participants,
    -- | Metadata of the commenting activity. This is an optional field and is
    -- filled for commenting activities.
    commentMetadata :: Prelude.Maybe CommentMetadata,
    -- | The timestamp when the action was performed.
    timeStamp :: Prelude.Maybe Prelude.POSIX,
    -- | The user who performed the action.
    initiator :: Prelude.Maybe UserMetadata,
    -- | The activity type.
    type' :: Prelude.Maybe ActivityType,
    -- | Indicates whether an activity is indirect or direct. An indirect
    -- activity results from a direct activity performed on a parent resource.
    -- For example, sharing a parent folder (the direct activity) shares all of
    -- the subfolders and documents within the parent folder (the indirect
    -- activity).
    isIndirectActivity :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Activity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceMetadata', 'activity_resourceMetadata' - The metadata of the resource involved in the user action.
--
-- 'organizationId', 'activity_organizationId' - The ID of the organization.
--
-- 'originalParent', 'activity_originalParent' - The original parent of the resource. This is an optional field and is
-- filled for move activities.
--
-- 'participants', 'activity_participants' - The list of users or groups impacted by this action. This is an optional
-- field and is filled for the following sharing activities:
-- DOCUMENT_SHARED, DOCUMENT_SHARED, DOCUMENT_UNSHARED, FOLDER_SHARED,
-- FOLDER_UNSHARED.
--
-- 'commentMetadata', 'activity_commentMetadata' - Metadata of the commenting activity. This is an optional field and is
-- filled for commenting activities.
--
-- 'timeStamp', 'activity_timeStamp' - The timestamp when the action was performed.
--
-- 'initiator', 'activity_initiator' - The user who performed the action.
--
-- 'type'', 'activity_type' - The activity type.
--
-- 'isIndirectActivity', 'activity_isIndirectActivity' - Indicates whether an activity is indirect or direct. An indirect
-- activity results from a direct activity performed on a parent resource.
-- For example, sharing a parent folder (the direct activity) shares all of
-- the subfolders and documents within the parent folder (the indirect
-- activity).
newActivity ::
  Activity
newActivity =
  Activity'
    { resourceMetadata = Prelude.Nothing,
      organizationId = Prelude.Nothing,
      originalParent = Prelude.Nothing,
      participants = Prelude.Nothing,
      commentMetadata = Prelude.Nothing,
      timeStamp = Prelude.Nothing,
      initiator = Prelude.Nothing,
      type' = Prelude.Nothing,
      isIndirectActivity = Prelude.Nothing
    }

-- | The metadata of the resource involved in the user action.
activity_resourceMetadata :: Lens.Lens' Activity (Prelude.Maybe ResourceMetadata)
activity_resourceMetadata = Lens.lens (\Activity' {resourceMetadata} -> resourceMetadata) (\s@Activity' {} a -> s {resourceMetadata = a} :: Activity)

-- | The ID of the organization.
activity_organizationId :: Lens.Lens' Activity (Prelude.Maybe Prelude.Text)
activity_organizationId = Lens.lens (\Activity' {organizationId} -> organizationId) (\s@Activity' {} a -> s {organizationId = a} :: Activity)

-- | The original parent of the resource. This is an optional field and is
-- filled for move activities.
activity_originalParent :: Lens.Lens' Activity (Prelude.Maybe ResourceMetadata)
activity_originalParent = Lens.lens (\Activity' {originalParent} -> originalParent) (\s@Activity' {} a -> s {originalParent = a} :: Activity)

-- | The list of users or groups impacted by this action. This is an optional
-- field and is filled for the following sharing activities:
-- DOCUMENT_SHARED, DOCUMENT_SHARED, DOCUMENT_UNSHARED, FOLDER_SHARED,
-- FOLDER_UNSHARED.
activity_participants :: Lens.Lens' Activity (Prelude.Maybe Participants)
activity_participants = Lens.lens (\Activity' {participants} -> participants) (\s@Activity' {} a -> s {participants = a} :: Activity)

-- | Metadata of the commenting activity. This is an optional field and is
-- filled for commenting activities.
activity_commentMetadata :: Lens.Lens' Activity (Prelude.Maybe CommentMetadata)
activity_commentMetadata = Lens.lens (\Activity' {commentMetadata} -> commentMetadata) (\s@Activity' {} a -> s {commentMetadata = a} :: Activity)

-- | The timestamp when the action was performed.
activity_timeStamp :: Lens.Lens' Activity (Prelude.Maybe Prelude.UTCTime)
activity_timeStamp = Lens.lens (\Activity' {timeStamp} -> timeStamp) (\s@Activity' {} a -> s {timeStamp = a} :: Activity) Prelude.. Lens.mapping Prelude._Time

-- | The user who performed the action.
activity_initiator :: Lens.Lens' Activity (Prelude.Maybe UserMetadata)
activity_initiator = Lens.lens (\Activity' {initiator} -> initiator) (\s@Activity' {} a -> s {initiator = a} :: Activity)

-- | The activity type.
activity_type :: Lens.Lens' Activity (Prelude.Maybe ActivityType)
activity_type = Lens.lens (\Activity' {type'} -> type') (\s@Activity' {} a -> s {type' = a} :: Activity)

-- | Indicates whether an activity is indirect or direct. An indirect
-- activity results from a direct activity performed on a parent resource.
-- For example, sharing a parent folder (the direct activity) shares all of
-- the subfolders and documents within the parent folder (the indirect
-- activity).
activity_isIndirectActivity :: Lens.Lens' Activity (Prelude.Maybe Prelude.Bool)
activity_isIndirectActivity = Lens.lens (\Activity' {isIndirectActivity} -> isIndirectActivity) (\s@Activity' {} a -> s {isIndirectActivity = a} :: Activity)

instance Prelude.FromJSON Activity where
  parseJSON =
    Prelude.withObject
      "Activity"
      ( \x ->
          Activity'
            Prelude.<$> (x Prelude..:? "ResourceMetadata")
            Prelude.<*> (x Prelude..:? "OrganizationId")
            Prelude.<*> (x Prelude..:? "OriginalParent")
            Prelude.<*> (x Prelude..:? "Participants")
            Prelude.<*> (x Prelude..:? "CommentMetadata")
            Prelude.<*> (x Prelude..:? "TimeStamp")
            Prelude.<*> (x Prelude..:? "Initiator")
            Prelude.<*> (x Prelude..:? "Type")
            Prelude.<*> (x Prelude..:? "IsIndirectActivity")
      )

instance Prelude.Hashable Activity

instance Prelude.NFData Activity
