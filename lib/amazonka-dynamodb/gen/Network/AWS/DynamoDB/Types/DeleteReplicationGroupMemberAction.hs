-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.DeleteReplicationGroupMemberAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.DeleteReplicationGroupMemberAction
  ( DeleteReplicationGroupMemberAction (..),

    -- * Smart constructor
    mkDeleteReplicationGroupMemberAction,

    -- * Lenses
    drgmaRegionName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a replica to be deleted.
--
-- /See:/ 'mkDeleteReplicationGroupMemberAction' smart constructor.
newtype DeleteReplicationGroupMemberAction = DeleteReplicationGroupMemberAction'
  { regionName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteReplicationGroupMemberAction' with the minimum fields required to make a request.
--
-- * 'regionName' - The Region where the replica exists.
mkDeleteReplicationGroupMemberAction ::
  -- | 'regionName'
  Lude.Text ->
  DeleteReplicationGroupMemberAction
mkDeleteReplicationGroupMemberAction pRegionName_ =
  DeleteReplicationGroupMemberAction' {regionName = pRegionName_}

-- | The Region where the replica exists.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgmaRegionName :: Lens.Lens' DeleteReplicationGroupMemberAction Lude.Text
drgmaRegionName = Lens.lens (regionName :: DeleteReplicationGroupMemberAction -> Lude.Text) (\s a -> s {regionName = a} :: DeleteReplicationGroupMemberAction)
{-# DEPRECATED drgmaRegionName "Use generic-lens or generic-optics with 'regionName' instead." #-}

instance Lude.ToJSON DeleteReplicationGroupMemberAction where
  toJSON DeleteReplicationGroupMemberAction' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("RegionName" Lude..= regionName)])
