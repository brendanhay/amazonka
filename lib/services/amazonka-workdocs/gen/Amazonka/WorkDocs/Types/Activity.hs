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
-- Module      : Amazonka.WorkDocs.Types.Activity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.Activity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkDocs.Types.ActivityType
import Amazonka.WorkDocs.Types.CommentMetadata
import Amazonka.WorkDocs.Types.Participants
import Amazonka.WorkDocs.Types.ResourceMetadata
import Amazonka.WorkDocs.Types.UserMetadata

-- | Describes the activity information.
--
-- /See:/ 'newActivity' smart constructor.
data Activity = Activity'
  { -- | Metadata of the commenting activity. This is an optional field and is
    -- filled for commenting activities.
    commentMetadata :: Prelude.Maybe CommentMetadata,
    -- | The user who performed the action.
    initiator :: Prelude.Maybe UserMetadata,
    -- | Indicates whether an activity is indirect or direct. An indirect
    -- activity results from a direct activity performed on a parent resource.
    -- For example, sharing a parent folder (the direct activity) shares all of
    -- the subfolders and documents within the parent folder (the indirect
    -- activity).
    isIndirectActivity :: Prelude.Maybe Prelude.Bool,
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
    -- | The metadata of the resource involved in the user action.
    resourceMetadata :: Prelude.Maybe ResourceMetadata,
    -- | The timestamp when the action was performed.
    timeStamp :: Prelude.Maybe Data.POSIX,
    -- | The activity type.
    type' :: Prelude.Maybe ActivityType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Activity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commentMetadata', 'activity_commentMetadata' - Metadata of the commenting activity. This is an optional field and is
-- filled for commenting activities.
--
-- 'initiator', 'activity_initiator' - The user who performed the action.
--
-- 'isIndirectActivity', 'activity_isIndirectActivity' - Indicates whether an activity is indirect or direct. An indirect
-- activity results from a direct activity performed on a parent resource.
-- For example, sharing a parent folder (the direct activity) shares all of
-- the subfolders and documents within the parent folder (the indirect
-- activity).
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
-- 'resourceMetadata', 'activity_resourceMetadata' - The metadata of the resource involved in the user action.
--
-- 'timeStamp', 'activity_timeStamp' - The timestamp when the action was performed.
--
-- 'type'', 'activity_type' - The activity type.
newActivity ::
  Activity
newActivity =
  Activity'
    { commentMetadata = Prelude.Nothing,
      initiator = Prelude.Nothing,
      isIndirectActivity = Prelude.Nothing,
      organizationId = Prelude.Nothing,
      originalParent = Prelude.Nothing,
      participants = Prelude.Nothing,
      resourceMetadata = Prelude.Nothing,
      timeStamp = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Metadata of the commenting activity. This is an optional field and is
-- filled for commenting activities.
activity_commentMetadata :: Lens.Lens' Activity (Prelude.Maybe CommentMetadata)
activity_commentMetadata = Lens.lens (\Activity' {commentMetadata} -> commentMetadata) (\s@Activity' {} a -> s {commentMetadata = a} :: Activity)

-- | The user who performed the action.
activity_initiator :: Lens.Lens' Activity (Prelude.Maybe UserMetadata)
activity_initiator = Lens.lens (\Activity' {initiator} -> initiator) (\s@Activity' {} a -> s {initiator = a} :: Activity)

-- | Indicates whether an activity is indirect or direct. An indirect
-- activity results from a direct activity performed on a parent resource.
-- For example, sharing a parent folder (the direct activity) shares all of
-- the subfolders and documents within the parent folder (the indirect
-- activity).
activity_isIndirectActivity :: Lens.Lens' Activity (Prelude.Maybe Prelude.Bool)
activity_isIndirectActivity = Lens.lens (\Activity' {isIndirectActivity} -> isIndirectActivity) (\s@Activity' {} a -> s {isIndirectActivity = a} :: Activity)

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

-- | The metadata of the resource involved in the user action.
activity_resourceMetadata :: Lens.Lens' Activity (Prelude.Maybe ResourceMetadata)
activity_resourceMetadata = Lens.lens (\Activity' {resourceMetadata} -> resourceMetadata) (\s@Activity' {} a -> s {resourceMetadata = a} :: Activity)

-- | The timestamp when the action was performed.
activity_timeStamp :: Lens.Lens' Activity (Prelude.Maybe Prelude.UTCTime)
activity_timeStamp = Lens.lens (\Activity' {timeStamp} -> timeStamp) (\s@Activity' {} a -> s {timeStamp = a} :: Activity) Prelude.. Lens.mapping Data._Time

-- | The activity type.
activity_type :: Lens.Lens' Activity (Prelude.Maybe ActivityType)
activity_type = Lens.lens (\Activity' {type'} -> type') (\s@Activity' {} a -> s {type' = a} :: Activity)

instance Data.FromJSON Activity where
  parseJSON =
    Data.withObject
      "Activity"
      ( \x ->
          Activity'
            Prelude.<$> (x Data..:? "CommentMetadata")
            Prelude.<*> (x Data..:? "Initiator")
            Prelude.<*> (x Data..:? "IsIndirectActivity")
            Prelude.<*> (x Data..:? "OrganizationId")
            Prelude.<*> (x Data..:? "OriginalParent")
            Prelude.<*> (x Data..:? "Participants")
            Prelude.<*> (x Data..:? "ResourceMetadata")
            Prelude.<*> (x Data..:? "TimeStamp")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable Activity where
  hashWithSalt _salt Activity' {..} =
    _salt `Prelude.hashWithSalt` commentMetadata
      `Prelude.hashWithSalt` initiator
      `Prelude.hashWithSalt` isIndirectActivity
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` originalParent
      `Prelude.hashWithSalt` participants
      `Prelude.hashWithSalt` resourceMetadata
      `Prelude.hashWithSalt` timeStamp
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Activity where
  rnf Activity' {..} =
    Prelude.rnf commentMetadata
      `Prelude.seq` Prelude.rnf initiator
      `Prelude.seq` Prelude.rnf isIndirectActivity
      `Prelude.seq` Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf originalParent
      `Prelude.seq` Prelude.rnf participants
      `Prelude.seq` Prelude.rnf resourceMetadata
      `Prelude.seq` Prelude.rnf timeStamp
      `Prelude.seq` Prelude.rnf type'
