{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Attachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.Attachment
  ( Attachment (..)
  -- * Smart constructor
  , mkAttachment
  -- * Lenses
  , aDetails
  , aId
  , aStatus
  , aType
  ) where

import qualified Network.AWS.ECS.Types.KeyValuePair as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing a container instance or task attachment.
--
-- /See:/ 'mkAttachment' smart constructor.
data Attachment = Attachment'
  { details :: Core.Maybe [Types.KeyValuePair]
    -- ^ Details of the attachment. For elastic network interfaces, this includes the network interface ID, the MAC address, the subnet ID, and the private IPv4 address.
  , id :: Core.Maybe Core.Text
    -- ^ The unique identifier for the attachment.
  , status :: Core.Maybe Core.Text
    -- ^ The status of the attachment. Valid values are @PRECREATED@ , @CREATED@ , @ATTACHING@ , @ATTACHED@ , @DETACHING@ , @DETACHED@ , and @DELETED@ .
  , type' :: Core.Maybe Core.Text
    -- ^ The type of the attachment, such as @ElasticNetworkInterface@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Attachment' value with any optional fields omitted.
mkAttachment
    :: Attachment
mkAttachment
  = Attachment'{details = Core.Nothing, id = Core.Nothing,
                status = Core.Nothing, type' = Core.Nothing}

-- | Details of the attachment. For elastic network interfaces, this includes the network interface ID, the MAC address, the subnet ID, and the private IPv4 address.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDetails :: Lens.Lens' Attachment (Core.Maybe [Types.KeyValuePair])
aDetails = Lens.field @"details"
{-# INLINEABLE aDetails #-}
{-# DEPRECATED details "Use generic-lens or generic-optics with 'details' instead"  #-}

-- | The unique identifier for the attachment.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aId :: Lens.Lens' Attachment (Core.Maybe Core.Text)
aId = Lens.field @"id"
{-# INLINEABLE aId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The status of the attachment. Valid values are @PRECREATED@ , @CREATED@ , @ATTACHING@ , @ATTACHED@ , @DETACHING@ , @DETACHED@ , and @DELETED@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStatus :: Lens.Lens' Attachment (Core.Maybe Core.Text)
aStatus = Lens.field @"status"
{-# INLINEABLE aStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The type of the attachment, such as @ElasticNetworkInterface@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aType :: Lens.Lens' Attachment (Core.Maybe Core.Text)
aType = Lens.field @"type'"
{-# INLINEABLE aType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON Attachment where
        parseJSON
          = Core.withObject "Attachment" Core.$
              \ x ->
                Attachment' Core.<$>
                  (x Core..:? "details") Core.<*> x Core..:? "id" Core.<*>
                    x Core..:? "status"
                    Core.<*> x Core..:? "type"
