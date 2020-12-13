{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.Change
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.Change
  ( Change (..),

    -- * Smart constructor
    mkChange,

    -- * Lenses
    cAction,
    cResourceRecordSet,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.ChangeAction
import Network.AWS.Route53.Types.ResourceRecordSet

-- | The information for each resource record set that you want to change.
--
-- /See:/ 'mkChange' smart constructor.
data Change = Change'
  { -- | The action to perform:
    --
    --
    --     * @CREATE@ : Creates a resource record set that has the specified values.
    --
    --
    --     * @DELETE@ : Deletes a existing resource record set.
    -- /Important:/ To delete the resource record set that is associated with a traffic policy instance, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_DeleteTrafficPolicyInstance.html DeleteTrafficPolicyInstance> . Amazon Route 53 will delete the resource record set automatically. If you delete the resource record set by using @ChangeResourceRecordSets@ , Route 53 doesn't automatically delete the traffic policy instance, and you'll continue to be charged for it even though it's no longer in use.
    --
    --
    --     * @UPSERT@ : If a resource record set doesn't already exist, Route 53 creates it. If a resource record set does exist, Route 53 updates it with the values in the request.
    action :: ChangeAction,
    -- | Information about the resource record set to create, delete, or update.
    resourceRecordSet :: ResourceRecordSet
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Change' with the minimum fields required to make a request.
--
-- * 'action' - The action to perform:
--
--
--     * @CREATE@ : Creates a resource record set that has the specified values.
--
--
--     * @DELETE@ : Deletes a existing resource record set.
-- /Important:/ To delete the resource record set that is associated with a traffic policy instance, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_DeleteTrafficPolicyInstance.html DeleteTrafficPolicyInstance> . Amazon Route 53 will delete the resource record set automatically. If you delete the resource record set by using @ChangeResourceRecordSets@ , Route 53 doesn't automatically delete the traffic policy instance, and you'll continue to be charged for it even though it's no longer in use.
--
--
--     * @UPSERT@ : If a resource record set doesn't already exist, Route 53 creates it. If a resource record set does exist, Route 53 updates it with the values in the request.
--
--
-- * 'resourceRecordSet' - Information about the resource record set to create, delete, or update.
mkChange ::
  -- | 'action'
  ChangeAction ->
  -- | 'resourceRecordSet'
  ResourceRecordSet ->
  Change
mkChange pAction_ pResourceRecordSet_ =
  Change'
    { action = pAction_,
      resourceRecordSet = pResourceRecordSet_
    }

-- | The action to perform:
--
--
--     * @CREATE@ : Creates a resource record set that has the specified values.
--
--
--     * @DELETE@ : Deletes a existing resource record set.
-- /Important:/ To delete the resource record set that is associated with a traffic policy instance, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_DeleteTrafficPolicyInstance.html DeleteTrafficPolicyInstance> . Amazon Route 53 will delete the resource record set automatically. If you delete the resource record set by using @ChangeResourceRecordSets@ , Route 53 doesn't automatically delete the traffic policy instance, and you'll continue to be charged for it even though it's no longer in use.
--
--
--     * @UPSERT@ : If a resource record set doesn't already exist, Route 53 creates it. If a resource record set does exist, Route 53 updates it with the values in the request.
--
--
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAction :: Lens.Lens' Change ChangeAction
cAction = Lens.lens (action :: Change -> ChangeAction) (\s a -> s {action = a} :: Change)
{-# DEPRECATED cAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | Information about the resource record set to create, delete, or update.
--
-- /Note:/ Consider using 'resourceRecordSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cResourceRecordSet :: Lens.Lens' Change ResourceRecordSet
cResourceRecordSet = Lens.lens (resourceRecordSet :: Change -> ResourceRecordSet) (\s a -> s {resourceRecordSet = a} :: Change)
{-# DEPRECATED cResourceRecordSet "Use generic-lens or generic-optics with 'resourceRecordSet' instead." #-}

instance Lude.ToXML Change where
  toXML Change' {..} =
    Lude.mconcat
      [ "Action" Lude.@= action,
        "ResourceRecordSet" Lude.@= resourceRecordSet
      ]
