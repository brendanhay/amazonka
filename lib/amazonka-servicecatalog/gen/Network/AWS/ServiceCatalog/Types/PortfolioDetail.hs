{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.PortfolioDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.PortfolioDetail
  ( PortfolioDetail (..)
  -- * Smart constructor
  , mkPortfolioDetail
  -- * Lenses
  , pdARN
  , pdCreatedTime
  , pdDescription
  , pdDisplayName
  , pdId
  , pdProviderName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.ARN as Types
import qualified Network.AWS.ServiceCatalog.Types.Description as Types
import qualified Network.AWS.ServiceCatalog.Types.Id as Types
import qualified Network.AWS.ServiceCatalog.Types.PortfolioDisplayName as Types
import qualified Network.AWS.ServiceCatalog.Types.ProviderName as Types

-- | Information about a portfolio.
--
-- /See:/ 'mkPortfolioDetail' smart constructor.
data PortfolioDetail = PortfolioDetail'
  { arn :: Core.Maybe Types.ARN
    -- ^ The ARN assigned to the portfolio.
  , createdTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The UTC time stamp of the creation time.
  , description :: Core.Maybe Types.Description
    -- ^ The description of the portfolio.
  , displayName :: Core.Maybe Types.PortfolioDisplayName
    -- ^ The name to use for display purposes.
  , id :: Core.Maybe Types.Id
    -- ^ The portfolio identifier.
  , providerName :: Core.Maybe Types.ProviderName
    -- ^ The name of the portfolio provider.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PortfolioDetail' value with any optional fields omitted.
mkPortfolioDetail
    :: PortfolioDetail
mkPortfolioDetail
  = PortfolioDetail'{arn = Core.Nothing, createdTime = Core.Nothing,
                     description = Core.Nothing, displayName = Core.Nothing,
                     id = Core.Nothing, providerName = Core.Nothing}

-- | The ARN assigned to the portfolio.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdARN :: Lens.Lens' PortfolioDetail (Core.Maybe Types.ARN)
pdARN = Lens.field @"arn"
{-# INLINEABLE pdARN #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The UTC time stamp of the creation time.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdCreatedTime :: Lens.Lens' PortfolioDetail (Core.Maybe Core.NominalDiffTime)
pdCreatedTime = Lens.field @"createdTime"
{-# INLINEABLE pdCreatedTime #-}
{-# DEPRECATED createdTime "Use generic-lens or generic-optics with 'createdTime' instead"  #-}

-- | The description of the portfolio.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdDescription :: Lens.Lens' PortfolioDetail (Core.Maybe Types.Description)
pdDescription = Lens.field @"description"
{-# INLINEABLE pdDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The name to use for display purposes.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdDisplayName :: Lens.Lens' PortfolioDetail (Core.Maybe Types.PortfolioDisplayName)
pdDisplayName = Lens.field @"displayName"
{-# INLINEABLE pdDisplayName #-}
{-# DEPRECATED displayName "Use generic-lens or generic-optics with 'displayName' instead"  #-}

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdId :: Lens.Lens' PortfolioDetail (Core.Maybe Types.Id)
pdId = Lens.field @"id"
{-# INLINEABLE pdId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The name of the portfolio provider.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdProviderName :: Lens.Lens' PortfolioDetail (Core.Maybe Types.ProviderName)
pdProviderName = Lens.field @"providerName"
{-# INLINEABLE pdProviderName #-}
{-# DEPRECATED providerName "Use generic-lens or generic-optics with 'providerName' instead"  #-}

instance Core.FromJSON PortfolioDetail where
        parseJSON
          = Core.withObject "PortfolioDetail" Core.$
              \ x ->
                PortfolioDetail' Core.<$>
                  (x Core..:? "ARN") Core.<*> x Core..:? "CreatedTime" Core.<*>
                    x Core..:? "Description"
                    Core.<*> x Core..:? "DisplayName"
                    Core.<*> x Core..:? "Id"
                    Core.<*> x Core..:? "ProviderName"
