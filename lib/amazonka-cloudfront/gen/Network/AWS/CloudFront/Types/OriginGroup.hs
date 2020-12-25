{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginGroup
  ( OriginGroup (..),

    -- * Smart constructor
    mkOriginGroup,

    -- * Lenses
    ogId,
    ogFailoverCriteria,
    ogMembers,
  )
where

import qualified Network.AWS.CloudFront.Types.OriginGroupFailoverCriteria as Types
import qualified Network.AWS.CloudFront.Types.OriginGroupMembers as Types
import qualified Network.AWS.CloudFront.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An origin group includes two origins (a primary origin and a second origin to failover to) and a failover criteria that you specify. You create an origin group to support origin failover in CloudFront. When you create or update a distribution, you can specifiy the origin group instead of a single origin, and CloudFront will failover from the primary origin to the second origin under the failover conditions that you've chosen.
--
-- /See:/ 'mkOriginGroup' smart constructor.
data OriginGroup = OriginGroup'
  { -- | The origin group's ID.
    id :: Types.String,
    -- | A complex type that contains information about the failover criteria for an origin group.
    failoverCriteria :: Types.OriginGroupFailoverCriteria,
    -- | A complex type that contains information about the origins in an origin group.
    members :: Types.OriginGroupMembers
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OriginGroup' value with any optional fields omitted.
mkOriginGroup ::
  -- | 'id'
  Types.String ->
  -- | 'failoverCriteria'
  Types.OriginGroupFailoverCriteria ->
  -- | 'members'
  Types.OriginGroupMembers ->
  OriginGroup
mkOriginGroup id failoverCriteria members =
  OriginGroup' {id, failoverCriteria, members}

-- | The origin group's ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogId :: Lens.Lens' OriginGroup Types.String
ogId = Lens.field @"id"
{-# DEPRECATED ogId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A complex type that contains information about the failover criteria for an origin group.
--
-- /Note:/ Consider using 'failoverCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogFailoverCriteria :: Lens.Lens' OriginGroup Types.OriginGroupFailoverCriteria
ogFailoverCriteria = Lens.field @"failoverCriteria"
{-# DEPRECATED ogFailoverCriteria "Use generic-lens or generic-optics with 'failoverCriteria' instead." #-}

-- | A complex type that contains information about the origins in an origin group.
--
-- /Note:/ Consider using 'members' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogMembers :: Lens.Lens' OriginGroup Types.OriginGroupMembers
ogMembers = Lens.field @"members"
{-# DEPRECATED ogMembers "Use generic-lens or generic-optics with 'members' instead." #-}

instance Core.ToXML OriginGroup where
  toXML OriginGroup {..} =
    Core.toXMLNode "Id" id
      Core.<> Core.toXMLNode "FailoverCriteria" failoverCriteria
      Core.<> Core.toXMLNode "Members" members

instance Core.FromXML OriginGroup where
  parseXML x =
    OriginGroup'
      Core.<$> (x Core..@ "Id")
      Core.<*> (x Core..@ "FailoverCriteria")
      Core.<*> (x Core..@ "Members")
