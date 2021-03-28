{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.DomainInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.DomainInformation
  ( DomainInformation (..)
  -- * Smart constructor
  , mkDomainInformation
  -- * Lenses
  , dDomainName
  , dOwnerId
  , dRegion
  ) where

import qualified Network.AWS.ElasticSearch.Types.DomainName as Types
import qualified Network.AWS.ElasticSearch.Types.OwnerId as Types
import qualified Network.AWS.ElasticSearch.Types.Region as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /See:/ 'mkDomainInformation' smart constructor.
data DomainInformation = DomainInformation'
  { domainName :: Types.DomainName
  , ownerId :: Core.Maybe Types.OwnerId
  , region :: Core.Maybe Types.Region
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DomainInformation' value with any optional fields omitted.
mkDomainInformation
    :: Types.DomainName -- ^ 'domainName'
    -> DomainInformation
mkDomainInformation domainName
  = DomainInformation'{domainName, ownerId = Core.Nothing,
                       region = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDomainName :: Lens.Lens' DomainInformation Types.DomainName
dDomainName = Lens.field @"domainName"
{-# INLINEABLE dDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dOwnerId :: Lens.Lens' DomainInformation (Core.Maybe Types.OwnerId)
dOwnerId = Lens.field @"ownerId"
{-# INLINEABLE dOwnerId #-}
{-# DEPRECATED ownerId "Use generic-lens or generic-optics with 'ownerId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRegion :: Lens.Lens' DomainInformation (Core.Maybe Types.Region)
dRegion = Lens.field @"region"
{-# INLINEABLE dRegion #-}
{-# DEPRECATED region "Use generic-lens or generic-optics with 'region' instead"  #-}

instance Core.FromJSON DomainInformation where
        toJSON DomainInformation{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DomainName" Core..= domainName),
                  ("OwnerId" Core..=) Core.<$> ownerId,
                  ("Region" Core..=) Core.<$> region])

instance Core.FromJSON DomainInformation where
        parseJSON
          = Core.withObject "DomainInformation" Core.$
              \ x ->
                DomainInformation' Core.<$>
                  (x Core..: "DomainName") Core.<*> x Core..:? "OwnerId" Core.<*>
                    x Core..:? "Region"
