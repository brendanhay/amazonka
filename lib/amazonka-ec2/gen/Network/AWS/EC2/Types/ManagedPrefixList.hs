{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ManagedPrefixList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ManagedPrefixList
  ( ManagedPrefixList (..)
  -- * Smart constructor
  , mkManagedPrefixList
  -- * Lenses
  , mplAddressFamily
  , mplMaxEntries
  , mplOwnerId
  , mplPrefixListArn
  , mplPrefixListId
  , mplPrefixListName
  , mplState
  , mplStateMessage
  , mplTags
  , mplVersion
  ) where

import qualified Network.AWS.EC2.Types.PrefixListArn as Types
import qualified Network.AWS.EC2.Types.PrefixListResourceId as Types
import qualified Network.AWS.EC2.Types.PrefixListState as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a managed prefix list.
--
-- /See:/ 'mkManagedPrefixList' smart constructor.
data ManagedPrefixList = ManagedPrefixList'
  { addressFamily :: Core.Maybe Core.Text
    -- ^ The IP address version.
  , maxEntries :: Core.Maybe Core.Int
    -- ^ The maximum number of entries for the prefix list.
  , ownerId :: Core.Maybe Core.Text
    -- ^ The ID of the owner of the prefix list.
  , prefixListArn :: Core.Maybe Types.PrefixListArn
    -- ^ The Amazon Resource Name (ARN) for the prefix list.
  , prefixListId :: Core.Maybe Types.PrefixListResourceId
    -- ^ The ID of the prefix list.
  , prefixListName :: Core.Maybe Core.Text
    -- ^ The name of the prefix list.
  , state :: Core.Maybe Types.PrefixListState
    -- ^ The state of the prefix list.
  , stateMessage :: Core.Maybe Core.Text
    -- ^ The state message.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags for the prefix list.
  , version :: Core.Maybe Core.Integer
    -- ^ The version of the prefix list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ManagedPrefixList' value with any optional fields omitted.
mkManagedPrefixList
    :: ManagedPrefixList
mkManagedPrefixList
  = ManagedPrefixList'{addressFamily = Core.Nothing,
                       maxEntries = Core.Nothing, ownerId = Core.Nothing,
                       prefixListArn = Core.Nothing, prefixListId = Core.Nothing,
                       prefixListName = Core.Nothing, state = Core.Nothing,
                       stateMessage = Core.Nothing, tags = Core.Nothing,
                       version = Core.Nothing}

-- | The IP address version.
--
-- /Note:/ Consider using 'addressFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mplAddressFamily :: Lens.Lens' ManagedPrefixList (Core.Maybe Core.Text)
mplAddressFamily = Lens.field @"addressFamily"
{-# INLINEABLE mplAddressFamily #-}
{-# DEPRECATED addressFamily "Use generic-lens or generic-optics with 'addressFamily' instead"  #-}

-- | The maximum number of entries for the prefix list.
--
-- /Note:/ Consider using 'maxEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mplMaxEntries :: Lens.Lens' ManagedPrefixList (Core.Maybe Core.Int)
mplMaxEntries = Lens.field @"maxEntries"
{-# INLINEABLE mplMaxEntries #-}
{-# DEPRECATED maxEntries "Use generic-lens or generic-optics with 'maxEntries' instead"  #-}

-- | The ID of the owner of the prefix list.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mplOwnerId :: Lens.Lens' ManagedPrefixList (Core.Maybe Core.Text)
mplOwnerId = Lens.field @"ownerId"
{-# INLINEABLE mplOwnerId #-}
{-# DEPRECATED ownerId "Use generic-lens or generic-optics with 'ownerId' instead"  #-}

-- | The Amazon Resource Name (ARN) for the prefix list.
--
-- /Note:/ Consider using 'prefixListArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mplPrefixListArn :: Lens.Lens' ManagedPrefixList (Core.Maybe Types.PrefixListArn)
mplPrefixListArn = Lens.field @"prefixListArn"
{-# INLINEABLE mplPrefixListArn #-}
{-# DEPRECATED prefixListArn "Use generic-lens or generic-optics with 'prefixListArn' instead"  #-}

-- | The ID of the prefix list.
--
-- /Note:/ Consider using 'prefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mplPrefixListId :: Lens.Lens' ManagedPrefixList (Core.Maybe Types.PrefixListResourceId)
mplPrefixListId = Lens.field @"prefixListId"
{-# INLINEABLE mplPrefixListId #-}
{-# DEPRECATED prefixListId "Use generic-lens or generic-optics with 'prefixListId' instead"  #-}

-- | The name of the prefix list.
--
-- /Note:/ Consider using 'prefixListName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mplPrefixListName :: Lens.Lens' ManagedPrefixList (Core.Maybe Core.Text)
mplPrefixListName = Lens.field @"prefixListName"
{-# INLINEABLE mplPrefixListName #-}
{-# DEPRECATED prefixListName "Use generic-lens or generic-optics with 'prefixListName' instead"  #-}

-- | The state of the prefix list.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mplState :: Lens.Lens' ManagedPrefixList (Core.Maybe Types.PrefixListState)
mplState = Lens.field @"state"
{-# INLINEABLE mplState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The state message.
--
-- /Note:/ Consider using 'stateMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mplStateMessage :: Lens.Lens' ManagedPrefixList (Core.Maybe Core.Text)
mplStateMessage = Lens.field @"stateMessage"
{-# INLINEABLE mplStateMessage #-}
{-# DEPRECATED stateMessage "Use generic-lens or generic-optics with 'stateMessage' instead"  #-}

-- | The tags for the prefix list.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mplTags :: Lens.Lens' ManagedPrefixList (Core.Maybe [Types.Tag])
mplTags = Lens.field @"tags"
{-# INLINEABLE mplTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The version of the prefix list.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mplVersion :: Lens.Lens' ManagedPrefixList (Core.Maybe Core.Integer)
mplVersion = Lens.field @"version"
{-# INLINEABLE mplVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromXML ManagedPrefixList where
        parseXML x
          = ManagedPrefixList' Core.<$>
              (x Core..@? "addressFamily") Core.<*> x Core..@? "maxEntries"
                Core.<*> x Core..@? "ownerId"
                Core.<*> x Core..@? "prefixListArn"
                Core.<*> x Core..@? "prefixListId"
                Core.<*> x Core..@? "prefixListName"
                Core.<*> x Core..@? "state"
                Core.<*> x Core..@? "stateMessage"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "version"
