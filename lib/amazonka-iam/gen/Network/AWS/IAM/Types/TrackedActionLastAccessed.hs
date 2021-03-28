{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.TrackedActionLastAccessed
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.TrackedActionLastAccessed
  ( TrackedActionLastAccessed (..)
  -- * Smart constructor
  , mkTrackedActionLastAccessed
  -- * Lenses
  , talaActionName
  , talaLastAccessedEntity
  , talaLastAccessedRegion
  , talaLastAccessedTime
  ) where

import qualified Network.AWS.IAM.Types.ActionName as Types
import qualified Network.AWS.IAM.Types.LastAccessedEntity as Types
import qualified Network.AWS.IAM.Types.LastAccessedRegion as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains details about the most recent attempt to access an action within the service.
--
-- This data type is used as a response element in the 'GetServiceLastAccessedDetails' operation.
--
-- /See:/ 'mkTrackedActionLastAccessed' smart constructor.
data TrackedActionLastAccessed = TrackedActionLastAccessed'
  { actionName :: Core.Maybe Types.ActionName
    -- ^ The name of the tracked action to which access was attempted. Tracked actions are actions that report activity to IAM.
  , lastAccessedEntity :: Core.Maybe Types.LastAccessedEntity
  , lastAccessedRegion :: Core.Maybe Types.LastAccessedRegion
    -- ^ The Region from which the authenticated entity (user or role) last attempted to access the tracked action. AWS does not report unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
  , lastAccessedTime :: Core.Maybe Core.UTCTime
    -- ^ The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when an authenticated entity most recently attempted to access the tracked service. AWS does not report unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'TrackedActionLastAccessed' value with any optional fields omitted.
mkTrackedActionLastAccessed
    :: TrackedActionLastAccessed
mkTrackedActionLastAccessed
  = TrackedActionLastAccessed'{actionName = Core.Nothing,
                               lastAccessedEntity = Core.Nothing,
                               lastAccessedRegion = Core.Nothing, lastAccessedTime = Core.Nothing}

-- | The name of the tracked action to which access was attempted. Tracked actions are actions that report activity to IAM.
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
talaActionName :: Lens.Lens' TrackedActionLastAccessed (Core.Maybe Types.ActionName)
talaActionName = Lens.field @"actionName"
{-# INLINEABLE talaActionName #-}
{-# DEPRECATED actionName "Use generic-lens or generic-optics with 'actionName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'lastAccessedEntity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
talaLastAccessedEntity :: Lens.Lens' TrackedActionLastAccessed (Core.Maybe Types.LastAccessedEntity)
talaLastAccessedEntity = Lens.field @"lastAccessedEntity"
{-# INLINEABLE talaLastAccessedEntity #-}
{-# DEPRECATED lastAccessedEntity "Use generic-lens or generic-optics with 'lastAccessedEntity' instead"  #-}

-- | The Region from which the authenticated entity (user or role) last attempted to access the tracked action. AWS does not report unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
--
-- /Note:/ Consider using 'lastAccessedRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
talaLastAccessedRegion :: Lens.Lens' TrackedActionLastAccessed (Core.Maybe Types.LastAccessedRegion)
talaLastAccessedRegion = Lens.field @"lastAccessedRegion"
{-# INLINEABLE talaLastAccessedRegion #-}
{-# DEPRECATED lastAccessedRegion "Use generic-lens or generic-optics with 'lastAccessedRegion' instead"  #-}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when an authenticated entity most recently attempted to access the tracked service. AWS does not report unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
--
-- /Note:/ Consider using 'lastAccessedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
talaLastAccessedTime :: Lens.Lens' TrackedActionLastAccessed (Core.Maybe Core.UTCTime)
talaLastAccessedTime = Lens.field @"lastAccessedTime"
{-# INLINEABLE talaLastAccessedTime #-}
{-# DEPRECATED lastAccessedTime "Use generic-lens or generic-optics with 'lastAccessedTime' instead"  #-}

instance Core.FromXML TrackedActionLastAccessed where
        parseXML x
          = TrackedActionLastAccessed' Core.<$>
              (x Core..@? "ActionName") Core.<*> x Core..@? "LastAccessedEntity"
                Core.<*> x Core..@? "LastAccessedRegion"
                Core.<*> x Core..@? "LastAccessedTime"
