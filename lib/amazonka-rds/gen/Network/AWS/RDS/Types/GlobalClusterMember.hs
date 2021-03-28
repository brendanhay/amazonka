{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.GlobalClusterMember
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.GlobalClusterMember
  ( GlobalClusterMember (..)
  -- * Smart constructor
  , mkGlobalClusterMember
  -- * Lenses
  , gcmDBClusterArn
  , gcmGlobalWriteForwardingStatus
  , gcmIsWriter
  , gcmReaders
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.WriteForwardingStatus as Types

-- | A data structure with information about any primary and secondary clusters associated with an Aurora global database. 
--
-- /See:/ 'mkGlobalClusterMember' smart constructor.
data GlobalClusterMember = GlobalClusterMember'
  { dBClusterArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) for each Aurora cluster. 
  , globalWriteForwardingStatus :: Core.Maybe Types.WriteForwardingStatus
    -- ^ Specifies whether a secondary cluster in an Aurora global database has write forwarding enabled, not enabled, or is in the process of enabling it.
  , isWriter :: Core.Maybe Core.Bool
    -- ^ Specifies whether the Aurora cluster is the primary cluster (that is, has read-write capability) for the Aurora global database with which it is associated. 
  , readers :: Core.Maybe [Core.Text]
    -- ^ The Amazon Resource Name (ARN) for each read-only secondary cluster associated with the Aurora global database. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GlobalClusterMember' value with any optional fields omitted.
mkGlobalClusterMember
    :: GlobalClusterMember
mkGlobalClusterMember
  = GlobalClusterMember'{dBClusterArn = Core.Nothing,
                         globalWriteForwardingStatus = Core.Nothing,
                         isWriter = Core.Nothing, readers = Core.Nothing}

-- | The Amazon Resource Name (ARN) for each Aurora cluster. 
--
-- /Note:/ Consider using 'dBClusterArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmDBClusterArn :: Lens.Lens' GlobalClusterMember (Core.Maybe Core.Text)
gcmDBClusterArn = Lens.field @"dBClusterArn"
{-# INLINEABLE gcmDBClusterArn #-}
{-# DEPRECATED dBClusterArn "Use generic-lens or generic-optics with 'dBClusterArn' instead"  #-}

-- | Specifies whether a secondary cluster in an Aurora global database has write forwarding enabled, not enabled, or is in the process of enabling it.
--
-- /Note:/ Consider using 'globalWriteForwardingStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmGlobalWriteForwardingStatus :: Lens.Lens' GlobalClusterMember (Core.Maybe Types.WriteForwardingStatus)
gcmGlobalWriteForwardingStatus = Lens.field @"globalWriteForwardingStatus"
{-# INLINEABLE gcmGlobalWriteForwardingStatus #-}
{-# DEPRECATED globalWriteForwardingStatus "Use generic-lens or generic-optics with 'globalWriteForwardingStatus' instead"  #-}

-- | Specifies whether the Aurora cluster is the primary cluster (that is, has read-write capability) for the Aurora global database with which it is associated. 
--
-- /Note:/ Consider using 'isWriter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmIsWriter :: Lens.Lens' GlobalClusterMember (Core.Maybe Core.Bool)
gcmIsWriter = Lens.field @"isWriter"
{-# INLINEABLE gcmIsWriter #-}
{-# DEPRECATED isWriter "Use generic-lens or generic-optics with 'isWriter' instead"  #-}

-- | The Amazon Resource Name (ARN) for each read-only secondary cluster associated with the Aurora global database. 
--
-- /Note:/ Consider using 'readers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmReaders :: Lens.Lens' GlobalClusterMember (Core.Maybe [Core.Text])
gcmReaders = Lens.field @"readers"
{-# INLINEABLE gcmReaders #-}
{-# DEPRECATED readers "Use generic-lens or generic-optics with 'readers' instead"  #-}

instance Core.FromXML GlobalClusterMember where
        parseXML x
          = GlobalClusterMember' Core.<$>
              (x Core..@? "DBClusterArn") Core.<*>
                x Core..@? "GlobalWriteForwardingStatus"
                Core.<*> x Core..@? "IsWriter"
                Core.<*> x Core..@? "Readers" Core..<@> Core.parseXMLList "member"
