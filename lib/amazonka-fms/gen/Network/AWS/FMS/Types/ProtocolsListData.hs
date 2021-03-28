{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.ProtocolsListData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.FMS.Types.ProtocolsListData
  ( ProtocolsListData (..)
  -- * Smart constructor
  , mkProtocolsListData
  -- * Lenses
  , pldListName
  , pldProtocolsList
  , pldCreateTime
  , pldLastUpdateTime
  , pldListId
  , pldListUpdateToken
  , pldPreviousProtocolsList
  ) where

import qualified Network.AWS.FMS.Types.ListId as Types
import qualified Network.AWS.FMS.Types.ListName as Types
import qualified Network.AWS.FMS.Types.ListUpdateToken as Types
import qualified Network.AWS.FMS.Types.PreviousListVersion as Types
import qualified Network.AWS.FMS.Types.Protocol as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An AWS Firewall Manager protocols list.
--
-- /See:/ 'mkProtocolsListData' smart constructor.
data ProtocolsListData = ProtocolsListData'
  { listName :: Types.ListName
    -- ^ The name of the AWS Firewall Manager protocols list.
  , protocolsList :: [Types.Protocol]
    -- ^ An array of protocols in the AWS Firewall Manager protocols list.
  , createTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that the AWS Firewall Manager protocols list was created.
  , lastUpdateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that the AWS Firewall Manager protocols list was last updated.
  , listId :: Core.Maybe Types.ListId
    -- ^ The ID of the AWS Firewall Manager protocols list.
  , listUpdateToken :: Core.Maybe Types.ListUpdateToken
    -- ^ A unique identifier for each update to the list. When you update the list, the update token must match the token of the current version of the application list. You can retrieve the update token by getting the list. 
  , previousProtocolsList :: Core.Maybe (Core.HashMap Types.PreviousListVersion [Types.Protocol])
    -- ^ A map of previous version numbers to their corresponding protocol arrays.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ProtocolsListData' value with any optional fields omitted.
mkProtocolsListData
    :: Types.ListName -- ^ 'listName'
    -> ProtocolsListData
mkProtocolsListData listName
  = ProtocolsListData'{listName, protocolsList = Core.mempty,
                       createTime = Core.Nothing, lastUpdateTime = Core.Nothing,
                       listId = Core.Nothing, listUpdateToken = Core.Nothing,
                       previousProtocolsList = Core.Nothing}

-- | The name of the AWS Firewall Manager protocols list.
--
-- /Note:/ Consider using 'listName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pldListName :: Lens.Lens' ProtocolsListData Types.ListName
pldListName = Lens.field @"listName"
{-# INLINEABLE pldListName #-}
{-# DEPRECATED listName "Use generic-lens or generic-optics with 'listName' instead"  #-}

-- | An array of protocols in the AWS Firewall Manager protocols list.
--
-- /Note:/ Consider using 'protocolsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pldProtocolsList :: Lens.Lens' ProtocolsListData [Types.Protocol]
pldProtocolsList = Lens.field @"protocolsList"
{-# INLINEABLE pldProtocolsList #-}
{-# DEPRECATED protocolsList "Use generic-lens or generic-optics with 'protocolsList' instead"  #-}

-- | The time that the AWS Firewall Manager protocols list was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pldCreateTime :: Lens.Lens' ProtocolsListData (Core.Maybe Core.NominalDiffTime)
pldCreateTime = Lens.field @"createTime"
{-# INLINEABLE pldCreateTime #-}
{-# DEPRECATED createTime "Use generic-lens or generic-optics with 'createTime' instead"  #-}

-- | The time that the AWS Firewall Manager protocols list was last updated.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pldLastUpdateTime :: Lens.Lens' ProtocolsListData (Core.Maybe Core.NominalDiffTime)
pldLastUpdateTime = Lens.field @"lastUpdateTime"
{-# INLINEABLE pldLastUpdateTime #-}
{-# DEPRECATED lastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead"  #-}

-- | The ID of the AWS Firewall Manager protocols list.
--
-- /Note:/ Consider using 'listId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pldListId :: Lens.Lens' ProtocolsListData (Core.Maybe Types.ListId)
pldListId = Lens.field @"listId"
{-# INLINEABLE pldListId #-}
{-# DEPRECATED listId "Use generic-lens or generic-optics with 'listId' instead"  #-}

-- | A unique identifier for each update to the list. When you update the list, the update token must match the token of the current version of the application list. You can retrieve the update token by getting the list. 
--
-- /Note:/ Consider using 'listUpdateToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pldListUpdateToken :: Lens.Lens' ProtocolsListData (Core.Maybe Types.ListUpdateToken)
pldListUpdateToken = Lens.field @"listUpdateToken"
{-# INLINEABLE pldListUpdateToken #-}
{-# DEPRECATED listUpdateToken "Use generic-lens or generic-optics with 'listUpdateToken' instead"  #-}

-- | A map of previous version numbers to their corresponding protocol arrays.
--
-- /Note:/ Consider using 'previousProtocolsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pldPreviousProtocolsList :: Lens.Lens' ProtocolsListData (Core.Maybe (Core.HashMap Types.PreviousListVersion [Types.Protocol]))
pldPreviousProtocolsList = Lens.field @"previousProtocolsList"
{-# INLINEABLE pldPreviousProtocolsList #-}
{-# DEPRECATED previousProtocolsList "Use generic-lens or generic-optics with 'previousProtocolsList' instead"  #-}

instance Core.FromJSON ProtocolsListData where
        toJSON ProtocolsListData{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ListName" Core..= listName),
                  Core.Just ("ProtocolsList" Core..= protocolsList),
                  ("CreateTime" Core..=) Core.<$> createTime,
                  ("LastUpdateTime" Core..=) Core.<$> lastUpdateTime,
                  ("ListId" Core..=) Core.<$> listId,
                  ("ListUpdateToken" Core..=) Core.<$> listUpdateToken,
                  ("PreviousProtocolsList" Core..=) Core.<$> previousProtocolsList])

instance Core.FromJSON ProtocolsListData where
        parseJSON
          = Core.withObject "ProtocolsListData" Core.$
              \ x ->
                ProtocolsListData' Core.<$>
                  (x Core..: "ListName") Core.<*>
                    x Core..:? "ProtocolsList" Core..!= Core.mempty
                    Core.<*> x Core..:? "CreateTime"
                    Core.<*> x Core..:? "LastUpdateTime"
                    Core.<*> x Core..:? "ListId"
                    Core.<*> x Core..:? "ListUpdateToken"
                    Core.<*> x Core..:? "PreviousProtocolsList"
