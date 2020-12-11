-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.GlobalClusterMember
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.GlobalClusterMember
  ( GlobalClusterMember (..),

    -- * Smart constructor
    mkGlobalClusterMember,

    -- * Lenses
    gcmReaders,
    gcmDBClusterARN,
    gcmIsWriter,
    gcmGlobalWriteForwardingStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types.WriteForwardingStatus

-- | A data structure with information about any primary and secondary clusters associated with an Aurora global database.
--
-- /See:/ 'mkGlobalClusterMember' smart constructor.
data GlobalClusterMember = GlobalClusterMember'
  { readers ::
      Lude.Maybe [Lude.Text],
    dbClusterARN :: Lude.Maybe Lude.Text,
    isWriter :: Lude.Maybe Lude.Bool,
    globalWriteForwardingStatus ::
      Lude.Maybe WriteForwardingStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GlobalClusterMember' with the minimum fields required to make a request.
--
-- * 'dbClusterARN' - The Amazon Resource Name (ARN) for each Aurora cluster.
-- * 'globalWriteForwardingStatus' - Specifies whether a secondary cluster in an Aurora global database has write forwarding enabled, not enabled, or is in the process of enabling it.
-- * 'isWriter' - Specifies whether the Aurora cluster is the primary cluster (that is, has read-write capability) for the Aurora global database with which it is associated.
-- * 'readers' - The Amazon Resource Name (ARN) for each read-only secondary cluster associated with the Aurora global database.
mkGlobalClusterMember ::
  GlobalClusterMember
mkGlobalClusterMember =
  GlobalClusterMember'
    { readers = Lude.Nothing,
      dbClusterARN = Lude.Nothing,
      isWriter = Lude.Nothing,
      globalWriteForwardingStatus = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) for each read-only secondary cluster associated with the Aurora global database.
--
-- /Note:/ Consider using 'readers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmReaders :: Lens.Lens' GlobalClusterMember (Lude.Maybe [Lude.Text])
gcmReaders = Lens.lens (readers :: GlobalClusterMember -> Lude.Maybe [Lude.Text]) (\s a -> s {readers = a} :: GlobalClusterMember)
{-# DEPRECATED gcmReaders "Use generic-lens or generic-optics with 'readers' instead." #-}

-- | The Amazon Resource Name (ARN) for each Aurora cluster.
--
-- /Note:/ Consider using 'dbClusterARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmDBClusterARN :: Lens.Lens' GlobalClusterMember (Lude.Maybe Lude.Text)
gcmDBClusterARN = Lens.lens (dbClusterARN :: GlobalClusterMember -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterARN = a} :: GlobalClusterMember)
{-# DEPRECATED gcmDBClusterARN "Use generic-lens or generic-optics with 'dbClusterARN' instead." #-}

-- | Specifies whether the Aurora cluster is the primary cluster (that is, has read-write capability) for the Aurora global database with which it is associated.
--
-- /Note:/ Consider using 'isWriter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmIsWriter :: Lens.Lens' GlobalClusterMember (Lude.Maybe Lude.Bool)
gcmIsWriter = Lens.lens (isWriter :: GlobalClusterMember -> Lude.Maybe Lude.Bool) (\s a -> s {isWriter = a} :: GlobalClusterMember)
{-# DEPRECATED gcmIsWriter "Use generic-lens or generic-optics with 'isWriter' instead." #-}

-- | Specifies whether a secondary cluster in an Aurora global database has write forwarding enabled, not enabled, or is in the process of enabling it.
--
-- /Note:/ Consider using 'globalWriteForwardingStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmGlobalWriteForwardingStatus :: Lens.Lens' GlobalClusterMember (Lude.Maybe WriteForwardingStatus)
gcmGlobalWriteForwardingStatus = Lens.lens (globalWriteForwardingStatus :: GlobalClusterMember -> Lude.Maybe WriteForwardingStatus) (\s a -> s {globalWriteForwardingStatus = a} :: GlobalClusterMember)
{-# DEPRECATED gcmGlobalWriteForwardingStatus "Use generic-lens or generic-optics with 'globalWriteForwardingStatus' instead." #-}

instance Lude.FromXML GlobalClusterMember where
  parseXML x =
    GlobalClusterMember'
      Lude.<$> ( x Lude..@? "Readers" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "DBClusterArn")
      Lude.<*> (x Lude..@? "IsWriter")
      Lude.<*> (x Lude..@? "GlobalWriteForwardingStatus")
