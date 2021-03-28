{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.ProtocolsListDataSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.FMS.Types.ProtocolsListDataSummary
  ( ProtocolsListDataSummary (..)
  -- * Smart constructor
  , mkProtocolsListDataSummary
  -- * Lenses
  , pldsListArn
  , pldsListId
  , pldsListName
  , pldsProtocolsList
  ) where

import qualified Network.AWS.FMS.Types.ListId as Types
import qualified Network.AWS.FMS.Types.Protocol as Types
import qualified Network.AWS.FMS.Types.ResourceArn as Types
import qualified Network.AWS.FMS.Types.ResourceName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details of the AWS Firewall Manager protocols list.
--
-- /See:/ 'mkProtocolsListDataSummary' smart constructor.
data ProtocolsListDataSummary = ProtocolsListDataSummary'
  { listArn :: Core.Maybe Types.ResourceArn
    -- ^ The Amazon Resource Name (ARN) of the specified protocols list.
  , listId :: Core.Maybe Types.ListId
    -- ^ The ID of the specified protocols list.
  , listName :: Core.Maybe Types.ResourceName
    -- ^ The name of the specified protocols list.
  , protocolsList :: Core.Maybe [Types.Protocol]
    -- ^ An array of protocols in the AWS Firewall Manager protocols list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProtocolsListDataSummary' value with any optional fields omitted.
mkProtocolsListDataSummary
    :: ProtocolsListDataSummary
mkProtocolsListDataSummary
  = ProtocolsListDataSummary'{listArn = Core.Nothing,
                              listId = Core.Nothing, listName = Core.Nothing,
                              protocolsList = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the specified protocols list.
--
-- /Note:/ Consider using 'listArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pldsListArn :: Lens.Lens' ProtocolsListDataSummary (Core.Maybe Types.ResourceArn)
pldsListArn = Lens.field @"listArn"
{-# INLINEABLE pldsListArn #-}
{-# DEPRECATED listArn "Use generic-lens or generic-optics with 'listArn' instead"  #-}

-- | The ID of the specified protocols list.
--
-- /Note:/ Consider using 'listId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pldsListId :: Lens.Lens' ProtocolsListDataSummary (Core.Maybe Types.ListId)
pldsListId = Lens.field @"listId"
{-# INLINEABLE pldsListId #-}
{-# DEPRECATED listId "Use generic-lens or generic-optics with 'listId' instead"  #-}

-- | The name of the specified protocols list.
--
-- /Note:/ Consider using 'listName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pldsListName :: Lens.Lens' ProtocolsListDataSummary (Core.Maybe Types.ResourceName)
pldsListName = Lens.field @"listName"
{-# INLINEABLE pldsListName #-}
{-# DEPRECATED listName "Use generic-lens or generic-optics with 'listName' instead"  #-}

-- | An array of protocols in the AWS Firewall Manager protocols list.
--
-- /Note:/ Consider using 'protocolsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pldsProtocolsList :: Lens.Lens' ProtocolsListDataSummary (Core.Maybe [Types.Protocol])
pldsProtocolsList = Lens.field @"protocolsList"
{-# INLINEABLE pldsProtocolsList #-}
{-# DEPRECATED protocolsList "Use generic-lens or generic-optics with 'protocolsList' instead"  #-}

instance Core.FromJSON ProtocolsListDataSummary where
        parseJSON
          = Core.withObject "ProtocolsListDataSummary" Core.$
              \ x ->
                ProtocolsListDataSummary' Core.<$>
                  (x Core..:? "ListArn") Core.<*> x Core..:? "ListId" Core.<*>
                    x Core..:? "ListName"
                    Core.<*> x Core..:? "ProtocolsList"
