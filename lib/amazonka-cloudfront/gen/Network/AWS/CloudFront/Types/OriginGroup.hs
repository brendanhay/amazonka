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

import Network.AWS.CloudFront.Types.OriginGroupFailoverCriteria
import Network.AWS.CloudFront.Types.OriginGroupMembers
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An origin group includes two origins (a primary origin and a second origin to failover to) and a failover criteria that you specify. You create an origin group to support origin failover in CloudFront. When you create or update a distribution, you can specifiy the origin group instead of a single origin, and CloudFront will failover from the primary origin to the second origin under the failover conditions that you've chosen.
--
-- /See:/ 'mkOriginGroup' smart constructor.
data OriginGroup = OriginGroup'
  { id :: Lude.Text,
    failoverCriteria :: OriginGroupFailoverCriteria,
    members :: OriginGroupMembers
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OriginGroup' with the minimum fields required to make a request.
--
-- * 'failoverCriteria' - A complex type that contains information about the failover criteria for an origin group.
-- * 'id' - The origin group's ID.
-- * 'members' - A complex type that contains information about the origins in an origin group.
mkOriginGroup ::
  -- | 'id'
  Lude.Text ->
  -- | 'failoverCriteria'
  OriginGroupFailoverCriteria ->
  -- | 'members'
  OriginGroupMembers ->
  OriginGroup
mkOriginGroup pId_ pFailoverCriteria_ pMembers_ =
  OriginGroup'
    { id = pId_,
      failoverCriteria = pFailoverCriteria_,
      members = pMembers_
    }

-- | The origin group's ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogId :: Lens.Lens' OriginGroup Lude.Text
ogId = Lens.lens (id :: OriginGroup -> Lude.Text) (\s a -> s {id = a} :: OriginGroup)
{-# DEPRECATED ogId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A complex type that contains information about the failover criteria for an origin group.
--
-- /Note:/ Consider using 'failoverCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogFailoverCriteria :: Lens.Lens' OriginGroup OriginGroupFailoverCriteria
ogFailoverCriteria = Lens.lens (failoverCriteria :: OriginGroup -> OriginGroupFailoverCriteria) (\s a -> s {failoverCriteria = a} :: OriginGroup)
{-# DEPRECATED ogFailoverCriteria "Use generic-lens or generic-optics with 'failoverCriteria' instead." #-}

-- | A complex type that contains information about the origins in an origin group.
--
-- /Note:/ Consider using 'members' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogMembers :: Lens.Lens' OriginGroup OriginGroupMembers
ogMembers = Lens.lens (members :: OriginGroup -> OriginGroupMembers) (\s a -> s {members = a} :: OriginGroup)
{-# DEPRECATED ogMembers "Use generic-lens or generic-optics with 'members' instead." #-}

instance Lude.FromXML OriginGroup where
  parseXML x =
    OriginGroup'
      Lude.<$> (x Lude..@ "Id")
      Lude.<*> (x Lude..@ "FailoverCriteria")
      Lude.<*> (x Lude..@ "Members")

instance Lude.ToXML OriginGroup where
  toXML OriginGroup' {..} =
    Lude.mconcat
      [ "Id" Lude.@= id,
        "FailoverCriteria" Lude.@= failoverCriteria,
        "Members" Lude.@= members
      ]
