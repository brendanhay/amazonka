-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.TrackedActionLastAccessed
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.TrackedActionLastAccessed
  ( TrackedActionLastAccessed (..),

    -- * Smart constructor
    mkTrackedActionLastAccessed,

    -- * Lenses
    talaLastAccessedTime,
    talaActionName,
    talaLastAccessedEntity,
    talaLastAccessedRegion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains details about the most recent attempt to access an action within the service.
--
-- This data type is used as a response element in the 'GetServiceLastAccessedDetails' operation.
--
-- /See:/ 'mkTrackedActionLastAccessed' smart constructor.
data TrackedActionLastAccessed = TrackedActionLastAccessed'
  { lastAccessedTime ::
      Lude.Maybe Lude.ISO8601,
    actionName :: Lude.Maybe Lude.Text,
    lastAccessedEntity ::
      Lude.Maybe Lude.Text,
    lastAccessedRegion ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TrackedActionLastAccessed' with the minimum fields required to make a request.
--
-- * 'actionName' - The name of the tracked action to which access was attempted. Tracked actions are actions that report activity to IAM.
-- * 'lastAccessedEntity' - Undocumented field.
-- * 'lastAccessedRegion' - The Region from which the authenticated entity (user or role) last attempted to access the tracked action. AWS does not report unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
-- * 'lastAccessedTime' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when an authenticated entity most recently attempted to access the tracked service. AWS does not report unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
mkTrackedActionLastAccessed ::
  TrackedActionLastAccessed
mkTrackedActionLastAccessed =
  TrackedActionLastAccessed'
    { lastAccessedTime = Lude.Nothing,
      actionName = Lude.Nothing,
      lastAccessedEntity = Lude.Nothing,
      lastAccessedRegion = Lude.Nothing
    }

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when an authenticated entity most recently attempted to access the tracked service. AWS does not report unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
--
-- /Note:/ Consider using 'lastAccessedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
talaLastAccessedTime :: Lens.Lens' TrackedActionLastAccessed (Lude.Maybe Lude.ISO8601)
talaLastAccessedTime = Lens.lens (lastAccessedTime :: TrackedActionLastAccessed -> Lude.Maybe Lude.ISO8601) (\s a -> s {lastAccessedTime = a} :: TrackedActionLastAccessed)
{-# DEPRECATED talaLastAccessedTime "Use generic-lens or generic-optics with 'lastAccessedTime' instead." #-}

-- | The name of the tracked action to which access was attempted. Tracked actions are actions that report activity to IAM.
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
talaActionName :: Lens.Lens' TrackedActionLastAccessed (Lude.Maybe Lude.Text)
talaActionName = Lens.lens (actionName :: TrackedActionLastAccessed -> Lude.Maybe Lude.Text) (\s a -> s {actionName = a} :: TrackedActionLastAccessed)
{-# DEPRECATED talaActionName "Use generic-lens or generic-optics with 'actionName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'lastAccessedEntity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
talaLastAccessedEntity :: Lens.Lens' TrackedActionLastAccessed (Lude.Maybe Lude.Text)
talaLastAccessedEntity = Lens.lens (lastAccessedEntity :: TrackedActionLastAccessed -> Lude.Maybe Lude.Text) (\s a -> s {lastAccessedEntity = a} :: TrackedActionLastAccessed)
{-# DEPRECATED talaLastAccessedEntity "Use generic-lens or generic-optics with 'lastAccessedEntity' instead." #-}

-- | The Region from which the authenticated entity (user or role) last attempted to access the tracked action. AWS does not report unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
--
-- /Note:/ Consider using 'lastAccessedRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
talaLastAccessedRegion :: Lens.Lens' TrackedActionLastAccessed (Lude.Maybe Lude.Text)
talaLastAccessedRegion = Lens.lens (lastAccessedRegion :: TrackedActionLastAccessed -> Lude.Maybe Lude.Text) (\s a -> s {lastAccessedRegion = a} :: TrackedActionLastAccessed)
{-# DEPRECATED talaLastAccessedRegion "Use generic-lens or generic-optics with 'lastAccessedRegion' instead." #-}

instance Lude.FromXML TrackedActionLastAccessed where
  parseXML x =
    TrackedActionLastAccessed'
      Lude.<$> (x Lude..@? "LastAccessedTime")
      Lude.<*> (x Lude..@? "ActionName")
      Lude.<*> (x Lude..@? "LastAccessedEntity")
      Lude.<*> (x Lude..@? "LastAccessedRegion")
