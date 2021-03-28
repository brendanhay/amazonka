{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.Activity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkDocs.Types.Activity
  ( Activity (..)
  -- * Smart constructor
  , mkActivity
  -- * Lenses
  , aCommentMetadata
  , aInitiator
  , aIsIndirectActivity
  , aOrganizationId
  , aOriginalParent
  , aParticipants
  , aResourceMetadata
  , aTimeStamp
  , aType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkDocs.Types.ActivityType as Types
import qualified Network.AWS.WorkDocs.Types.CommentMetadata as Types
import qualified Network.AWS.WorkDocs.Types.IdType as Types
import qualified Network.AWS.WorkDocs.Types.Participants as Types
import qualified Network.AWS.WorkDocs.Types.ResourceMetadata as Types
import qualified Network.AWS.WorkDocs.Types.UserMetadata as Types

-- | Describes the activity information.
--
-- /See:/ 'mkActivity' smart constructor.
data Activity = Activity'
  { commentMetadata :: Core.Maybe Types.CommentMetadata
    -- ^ Metadata of the commenting activity. This is an optional field and is filled for commenting activities.
  , initiator :: Core.Maybe Types.UserMetadata
    -- ^ The user who performed the action.
  , isIndirectActivity :: Core.Maybe Core.Bool
    -- ^ Indicates whether an activity is indirect or direct. An indirect activity results from a direct activity performed on a parent resource. For example, sharing a parent folder (the direct activity) shares all of the subfolders and documents within the parent folder (the indirect activity).
  , organizationId :: Core.Maybe Types.IdType
    -- ^ The ID of the organization.
  , originalParent :: Core.Maybe Types.ResourceMetadata
    -- ^ The original parent of the resource. This is an optional field and is filled for move activities.
  , participants :: Core.Maybe Types.Participants
    -- ^ The list of users or groups impacted by this action. This is an optional field and is filled for the following sharing activities: DOCUMENT_SHARED, DOCUMENT_SHARED, DOCUMENT_UNSHARED, FOLDER_SHARED, FOLDER_UNSHARED.
  , resourceMetadata :: Core.Maybe Types.ResourceMetadata
    -- ^ The metadata of the resource involved in the user action.
  , timeStamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp when the action was performed.
  , type' :: Core.Maybe Types.ActivityType
    -- ^ The activity type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Activity' value with any optional fields omitted.
mkActivity
    :: Activity
mkActivity
  = Activity'{commentMetadata = Core.Nothing,
              initiator = Core.Nothing, isIndirectActivity = Core.Nothing,
              organizationId = Core.Nothing, originalParent = Core.Nothing,
              participants = Core.Nothing, resourceMetadata = Core.Nothing,
              timeStamp = Core.Nothing, type' = Core.Nothing}

-- | Metadata of the commenting activity. This is an optional field and is filled for commenting activities.
--
-- /Note:/ Consider using 'commentMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCommentMetadata :: Lens.Lens' Activity (Core.Maybe Types.CommentMetadata)
aCommentMetadata = Lens.field @"commentMetadata"
{-# INLINEABLE aCommentMetadata #-}
{-# DEPRECATED commentMetadata "Use generic-lens or generic-optics with 'commentMetadata' instead"  #-}

-- | The user who performed the action.
--
-- /Note:/ Consider using 'initiator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aInitiator :: Lens.Lens' Activity (Core.Maybe Types.UserMetadata)
aInitiator = Lens.field @"initiator"
{-# INLINEABLE aInitiator #-}
{-# DEPRECATED initiator "Use generic-lens or generic-optics with 'initiator' instead"  #-}

-- | Indicates whether an activity is indirect or direct. An indirect activity results from a direct activity performed on a parent resource. For example, sharing a parent folder (the direct activity) shares all of the subfolders and documents within the parent folder (the indirect activity).
--
-- /Note:/ Consider using 'isIndirectActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aIsIndirectActivity :: Lens.Lens' Activity (Core.Maybe Core.Bool)
aIsIndirectActivity = Lens.field @"isIndirectActivity"
{-# INLINEABLE aIsIndirectActivity #-}
{-# DEPRECATED isIndirectActivity "Use generic-lens or generic-optics with 'isIndirectActivity' instead"  #-}

-- | The ID of the organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aOrganizationId :: Lens.Lens' Activity (Core.Maybe Types.IdType)
aOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE aOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

-- | The original parent of the resource. This is an optional field and is filled for move activities.
--
-- /Note:/ Consider using 'originalParent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aOriginalParent :: Lens.Lens' Activity (Core.Maybe Types.ResourceMetadata)
aOriginalParent = Lens.field @"originalParent"
{-# INLINEABLE aOriginalParent #-}
{-# DEPRECATED originalParent "Use generic-lens or generic-optics with 'originalParent' instead"  #-}

-- | The list of users or groups impacted by this action. This is an optional field and is filled for the following sharing activities: DOCUMENT_SHARED, DOCUMENT_SHARED, DOCUMENT_UNSHARED, FOLDER_SHARED, FOLDER_UNSHARED.
--
-- /Note:/ Consider using 'participants' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aParticipants :: Lens.Lens' Activity (Core.Maybe Types.Participants)
aParticipants = Lens.field @"participants"
{-# INLINEABLE aParticipants #-}
{-# DEPRECATED participants "Use generic-lens or generic-optics with 'participants' instead"  #-}

-- | The metadata of the resource involved in the user action.
--
-- /Note:/ Consider using 'resourceMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aResourceMetadata :: Lens.Lens' Activity (Core.Maybe Types.ResourceMetadata)
aResourceMetadata = Lens.field @"resourceMetadata"
{-# INLINEABLE aResourceMetadata #-}
{-# DEPRECATED resourceMetadata "Use generic-lens or generic-optics with 'resourceMetadata' instead"  #-}

-- | The timestamp when the action was performed.
--
-- /Note:/ Consider using 'timeStamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aTimeStamp :: Lens.Lens' Activity (Core.Maybe Core.NominalDiffTime)
aTimeStamp = Lens.field @"timeStamp"
{-# INLINEABLE aTimeStamp #-}
{-# DEPRECATED timeStamp "Use generic-lens or generic-optics with 'timeStamp' instead"  #-}

-- | The activity type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aType :: Lens.Lens' Activity (Core.Maybe Types.ActivityType)
aType = Lens.field @"type'"
{-# INLINEABLE aType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON Activity where
        parseJSON
          = Core.withObject "Activity" Core.$
              \ x ->
                Activity' Core.<$>
                  (x Core..:? "CommentMetadata") Core.<*> x Core..:? "Initiator"
                    Core.<*> x Core..:? "IsIndirectActivity"
                    Core.<*> x Core..:? "OrganizationId"
                    Core.<*> x Core..:? "OriginalParent"
                    Core.<*> x Core..:? "Participants"
                    Core.<*> x Core..:? "ResourceMetadata"
                    Core.<*> x Core..:? "TimeStamp"
                    Core.<*> x Core..:? "Type"
