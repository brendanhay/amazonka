-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.GlobalReplicationGroupInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.GlobalReplicationGroupInfo
  ( GlobalReplicationGroupInfo (..),

    -- * Smart constructor
    mkGlobalReplicationGroupInfo,

    -- * Lenses
    grgiGlobalReplicationGroupMemberRole,
    grgiGlobalReplicationGroupId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The name of the Global Datastore and role of this replication group in the Global Datastore.
--
-- /See:/ 'mkGlobalReplicationGroupInfo' smart constructor.
data GlobalReplicationGroupInfo = GlobalReplicationGroupInfo'
  { globalReplicationGroupMemberRole ::
      Lude.Maybe Lude.Text,
    globalReplicationGroupId ::
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

-- | Creates a value of 'GlobalReplicationGroupInfo' with the minimum fields required to make a request.
--
-- * 'globalReplicationGroupId' - The name of the Global Datastore
-- * 'globalReplicationGroupMemberRole' - The role of the replication group in a Global Datastore. Can be primary or secondary.
mkGlobalReplicationGroupInfo ::
  GlobalReplicationGroupInfo
mkGlobalReplicationGroupInfo =
  GlobalReplicationGroupInfo'
    { globalReplicationGroupMemberRole =
        Lude.Nothing,
      globalReplicationGroupId = Lude.Nothing
    }

-- | The role of the replication group in a Global Datastore. Can be primary or secondary.
--
-- /Note:/ Consider using 'globalReplicationGroupMemberRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgiGlobalReplicationGroupMemberRole :: Lens.Lens' GlobalReplicationGroupInfo (Lude.Maybe Lude.Text)
grgiGlobalReplicationGroupMemberRole = Lens.lens (globalReplicationGroupMemberRole :: GlobalReplicationGroupInfo -> Lude.Maybe Lude.Text) (\s a -> s {globalReplicationGroupMemberRole = a} :: GlobalReplicationGroupInfo)
{-# DEPRECATED grgiGlobalReplicationGroupMemberRole "Use generic-lens or generic-optics with 'globalReplicationGroupMemberRole' instead." #-}

-- | The name of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgiGlobalReplicationGroupId :: Lens.Lens' GlobalReplicationGroupInfo (Lude.Maybe Lude.Text)
grgiGlobalReplicationGroupId = Lens.lens (globalReplicationGroupId :: GlobalReplicationGroupInfo -> Lude.Maybe Lude.Text) (\s a -> s {globalReplicationGroupId = a} :: GlobalReplicationGroupInfo)
{-# DEPRECATED grgiGlobalReplicationGroupId "Use generic-lens or generic-optics with 'globalReplicationGroupId' instead." #-}

instance Lude.FromXML GlobalReplicationGroupInfo where
  parseXML x =
    GlobalReplicationGroupInfo'
      Lude.<$> (x Lude..@? "GlobalReplicationGroupMemberRole")
      Lude.<*> (x Lude..@? "GlobalReplicationGroupId")
