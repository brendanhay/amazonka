{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.Activity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.Activity
  ( Activity (..),

    -- * Smart constructor
    mkActivity,

    -- * Lenses
    aResourceMetadata,
    aIsIndirectActivity,
    aInitiator,
    aParticipants,
    aOriginalParent,
    aType,
    aCommentMetadata,
    aTimeStamp,
    aOrganizationId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkDocs.Types.ActivityType
import Network.AWS.WorkDocs.Types.CommentMetadata
import Network.AWS.WorkDocs.Types.Participants
import Network.AWS.WorkDocs.Types.ResourceMetadata
import Network.AWS.WorkDocs.Types.UserMetadata

-- | Describes the activity information.
--
-- /See:/ 'mkActivity' smart constructor.
data Activity = Activity'
  { resourceMetadata ::
      Lude.Maybe ResourceMetadata,
    isIndirectActivity :: Lude.Maybe Lude.Bool,
    initiator :: Lude.Maybe UserMetadata,
    participants :: Lude.Maybe Participants,
    originalParent :: Lude.Maybe ResourceMetadata,
    type' :: Lude.Maybe ActivityType,
    commentMetadata :: Lude.Maybe CommentMetadata,
    timeStamp :: Lude.Maybe Lude.Timestamp,
    organizationId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Activity' with the minimum fields required to make a request.
--
-- * 'commentMetadata' - Metadata of the commenting activity. This is an optional field and is filled for commenting activities.
-- * 'initiator' - The user who performed the action.
-- * 'isIndirectActivity' - Indicates whether an activity is indirect or direct. An indirect activity results from a direct activity performed on a parent resource. For example, sharing a parent folder (the direct activity) shares all of the subfolders and documents within the parent folder (the indirect activity).
-- * 'organizationId' - The ID of the organization.
-- * 'originalParent' - The original parent of the resource. This is an optional field and is filled for move activities.
-- * 'participants' - The list of users or groups impacted by this action. This is an optional field and is filled for the following sharing activities: DOCUMENT_SHARED, DOCUMENT_SHARED, DOCUMENT_UNSHARED, FOLDER_SHARED, FOLDER_UNSHARED.
-- * 'resourceMetadata' - The metadata of the resource involved in the user action.
-- * 'timeStamp' - The timestamp when the action was performed.
-- * 'type'' - The activity type.
mkActivity ::
  Activity
mkActivity =
  Activity'
    { resourceMetadata = Lude.Nothing,
      isIndirectActivity = Lude.Nothing,
      initiator = Lude.Nothing,
      participants = Lude.Nothing,
      originalParent = Lude.Nothing,
      type' = Lude.Nothing,
      commentMetadata = Lude.Nothing,
      timeStamp = Lude.Nothing,
      organizationId = Lude.Nothing
    }

-- | The metadata of the resource involved in the user action.
--
-- /Note:/ Consider using 'resourceMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aResourceMetadata :: Lens.Lens' Activity (Lude.Maybe ResourceMetadata)
aResourceMetadata = Lens.lens (resourceMetadata :: Activity -> Lude.Maybe ResourceMetadata) (\s a -> s {resourceMetadata = a} :: Activity)
{-# DEPRECATED aResourceMetadata "Use generic-lens or generic-optics with 'resourceMetadata' instead." #-}

-- | Indicates whether an activity is indirect or direct. An indirect activity results from a direct activity performed on a parent resource. For example, sharing a parent folder (the direct activity) shares all of the subfolders and documents within the parent folder (the indirect activity).
--
-- /Note:/ Consider using 'isIndirectActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aIsIndirectActivity :: Lens.Lens' Activity (Lude.Maybe Lude.Bool)
aIsIndirectActivity = Lens.lens (isIndirectActivity :: Activity -> Lude.Maybe Lude.Bool) (\s a -> s {isIndirectActivity = a} :: Activity)
{-# DEPRECATED aIsIndirectActivity "Use generic-lens or generic-optics with 'isIndirectActivity' instead." #-}

-- | The user who performed the action.
--
-- /Note:/ Consider using 'initiator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aInitiator :: Lens.Lens' Activity (Lude.Maybe UserMetadata)
aInitiator = Lens.lens (initiator :: Activity -> Lude.Maybe UserMetadata) (\s a -> s {initiator = a} :: Activity)
{-# DEPRECATED aInitiator "Use generic-lens or generic-optics with 'initiator' instead." #-}

-- | The list of users or groups impacted by this action. This is an optional field and is filled for the following sharing activities: DOCUMENT_SHARED, DOCUMENT_SHARED, DOCUMENT_UNSHARED, FOLDER_SHARED, FOLDER_UNSHARED.
--
-- /Note:/ Consider using 'participants' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aParticipants :: Lens.Lens' Activity (Lude.Maybe Participants)
aParticipants = Lens.lens (participants :: Activity -> Lude.Maybe Participants) (\s a -> s {participants = a} :: Activity)
{-# DEPRECATED aParticipants "Use generic-lens or generic-optics with 'participants' instead." #-}

-- | The original parent of the resource. This is an optional field and is filled for move activities.
--
-- /Note:/ Consider using 'originalParent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aOriginalParent :: Lens.Lens' Activity (Lude.Maybe ResourceMetadata)
aOriginalParent = Lens.lens (originalParent :: Activity -> Lude.Maybe ResourceMetadata) (\s a -> s {originalParent = a} :: Activity)
{-# DEPRECATED aOriginalParent "Use generic-lens or generic-optics with 'originalParent' instead." #-}

-- | The activity type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aType :: Lens.Lens' Activity (Lude.Maybe ActivityType)
aType = Lens.lens (type' :: Activity -> Lude.Maybe ActivityType) (\s a -> s {type' = a} :: Activity)
{-# DEPRECATED aType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Metadata of the commenting activity. This is an optional field and is filled for commenting activities.
--
-- /Note:/ Consider using 'commentMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCommentMetadata :: Lens.Lens' Activity (Lude.Maybe CommentMetadata)
aCommentMetadata = Lens.lens (commentMetadata :: Activity -> Lude.Maybe CommentMetadata) (\s a -> s {commentMetadata = a} :: Activity)
{-# DEPRECATED aCommentMetadata "Use generic-lens or generic-optics with 'commentMetadata' instead." #-}

-- | The timestamp when the action was performed.
--
-- /Note:/ Consider using 'timeStamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aTimeStamp :: Lens.Lens' Activity (Lude.Maybe Lude.Timestamp)
aTimeStamp = Lens.lens (timeStamp :: Activity -> Lude.Maybe Lude.Timestamp) (\s a -> s {timeStamp = a} :: Activity)
{-# DEPRECATED aTimeStamp "Use generic-lens or generic-optics with 'timeStamp' instead." #-}

-- | The ID of the organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aOrganizationId :: Lens.Lens' Activity (Lude.Maybe Lude.Text)
aOrganizationId = Lens.lens (organizationId :: Activity -> Lude.Maybe Lude.Text) (\s a -> s {organizationId = a} :: Activity)
{-# DEPRECATED aOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Lude.FromJSON Activity where
  parseJSON =
    Lude.withObject
      "Activity"
      ( \x ->
          Activity'
            Lude.<$> (x Lude..:? "ResourceMetadata")
            Lude.<*> (x Lude..:? "IsIndirectActivity")
            Lude.<*> (x Lude..:? "Initiator")
            Lude.<*> (x Lude..:? "Participants")
            Lude.<*> (x Lude..:? "OriginalParent")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "CommentMetadata")
            Lude.<*> (x Lude..:? "TimeStamp")
            Lude.<*> (x Lude..:? "OrganizationId")
      )
