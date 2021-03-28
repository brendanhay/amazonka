{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.StorageConnector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.StorageConnector
  ( StorageConnector (..)
  -- * Smart constructor
  , mkStorageConnector
  -- * Lenses
  , scConnectorType
  , scDomains
  , scResourceIdentifier
  ) where

import qualified Network.AWS.AppStream.Types.Domain as Types
import qualified Network.AWS.AppStream.Types.ResourceIdentifier as Types
import qualified Network.AWS.AppStream.Types.StorageConnectorType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a connector that enables persistent storage for users.
--
-- /See:/ 'mkStorageConnector' smart constructor.
data StorageConnector = StorageConnector'
  { connectorType :: Types.StorageConnectorType
    -- ^ The type of storage connector.
  , domains :: Core.Maybe [Types.Domain]
    -- ^ The names of the domains for the account.
  , resourceIdentifier :: Core.Maybe Types.ResourceIdentifier
    -- ^ The ARN of the storage connector.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StorageConnector' value with any optional fields omitted.
mkStorageConnector
    :: Types.StorageConnectorType -- ^ 'connectorType'
    -> StorageConnector
mkStorageConnector connectorType
  = StorageConnector'{connectorType, domains = Core.Nothing,
                      resourceIdentifier = Core.Nothing}

-- | The type of storage connector.
--
-- /Note:/ Consider using 'connectorType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scConnectorType :: Lens.Lens' StorageConnector Types.StorageConnectorType
scConnectorType = Lens.field @"connectorType"
{-# INLINEABLE scConnectorType #-}
{-# DEPRECATED connectorType "Use generic-lens or generic-optics with 'connectorType' instead"  #-}

-- | The names of the domains for the account.
--
-- /Note:/ Consider using 'domains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scDomains :: Lens.Lens' StorageConnector (Core.Maybe [Types.Domain])
scDomains = Lens.field @"domains"
{-# INLINEABLE scDomains #-}
{-# DEPRECATED domains "Use generic-lens or generic-optics with 'domains' instead"  #-}

-- | The ARN of the storage connector.
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scResourceIdentifier :: Lens.Lens' StorageConnector (Core.Maybe Types.ResourceIdentifier)
scResourceIdentifier = Lens.field @"resourceIdentifier"
{-# INLINEABLE scResourceIdentifier #-}
{-# DEPRECATED resourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead"  #-}

instance Core.FromJSON StorageConnector where
        toJSON StorageConnector{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ConnectorType" Core..= connectorType),
                  ("Domains" Core..=) Core.<$> domains,
                  ("ResourceIdentifier" Core..=) Core.<$> resourceIdentifier])

instance Core.FromJSON StorageConnector where
        parseJSON
          = Core.withObject "StorageConnector" Core.$
              \ x ->
                StorageConnector' Core.<$>
                  (x Core..: "ConnectorType") Core.<*> x Core..:? "Domains" Core.<*>
                    x Core..:? "ResourceIdentifier"
