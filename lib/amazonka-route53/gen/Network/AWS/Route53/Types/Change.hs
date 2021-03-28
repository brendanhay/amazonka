{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.Change
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Types.Change
  ( Change (..)
  -- * Smart constructor
  , mkChange
  -- * Lenses
  , cAction
  , cResourceRecordSet
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53.Internal as Types
import qualified Network.AWS.Route53.Types.ChangeAction as Types
import qualified Network.AWS.Route53.Types.ResourceRecordSet as Types

-- | The information for each resource record set that you want to change.
--
-- /See:/ 'mkChange' smart constructor.
data Change = Change'
  { action :: Types.ChangeAction
    -- ^ The action to perform:
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
  , resourceRecordSet :: Types.ResourceRecordSet
    -- ^ Information about the resource record set to create, delete, or update.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Change' value with any optional fields omitted.
mkChange
    :: Types.ChangeAction -- ^ 'action'
    -> Types.ResourceRecordSet -- ^ 'resourceRecordSet'
    -> Change
mkChange action resourceRecordSet
  = Change'{action, resourceRecordSet}

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
cAction :: Lens.Lens' Change Types.ChangeAction
cAction = Lens.field @"action"
{-# INLINEABLE cAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

-- | Information about the resource record set to create, delete, or update.
--
-- /Note:/ Consider using 'resourceRecordSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cResourceRecordSet :: Lens.Lens' Change Types.ResourceRecordSet
cResourceRecordSet = Lens.field @"resourceRecordSet"
{-# INLINEABLE cResourceRecordSet #-}
{-# DEPRECATED resourceRecordSet "Use generic-lens or generic-optics with 'resourceRecordSet' instead"  #-}

instance Core.ToXML Change where
        toXML Change{..}
          = Core.toXMLElement "Action" action Core.<>
              Core.toXMLElement "ResourceRecordSet" resourceRecordSet
