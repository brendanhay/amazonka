{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types.IdentityUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoSync.Types.IdentityUsage
  ( IdentityUsage (..)
  -- * Smart constructor
  , mkIdentityUsage
  -- * Lenses
  , iuDataStorage
  , iuDatasetCount
  , iuIdentityId
  , iuIdentityPoolId
  , iuLastModifiedDate
  ) where

import qualified Network.AWS.CognitoSync.Types.IdentityId as Types
import qualified Network.AWS.CognitoSync.Types.IdentityPoolId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Usage information for the identity.
--
-- /See:/ 'mkIdentityUsage' smart constructor.
data IdentityUsage = IdentityUsage'
  { dataStorage :: Core.Maybe Core.Integer
    -- ^ Total data storage for this identity.
  , datasetCount :: Core.Maybe Core.Int
    -- ^ Number of datasets for the identity.
  , identityId :: Core.Maybe Types.IdentityId
    -- ^ A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
  , identityPoolId :: Core.Maybe Types.IdentityPoolId
    -- ^ A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
  , lastModifiedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ Date on which the identity was last modified.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'IdentityUsage' value with any optional fields omitted.
mkIdentityUsage
    :: IdentityUsage
mkIdentityUsage
  = IdentityUsage'{dataStorage = Core.Nothing,
                   datasetCount = Core.Nothing, identityId = Core.Nothing,
                   identityPoolId = Core.Nothing, lastModifiedDate = Core.Nothing}

-- | Total data storage for this identity.
--
-- /Note:/ Consider using 'dataStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iuDataStorage :: Lens.Lens' IdentityUsage (Core.Maybe Core.Integer)
iuDataStorage = Lens.field @"dataStorage"
{-# INLINEABLE iuDataStorage #-}
{-# DEPRECATED dataStorage "Use generic-lens or generic-optics with 'dataStorage' instead"  #-}

-- | Number of datasets for the identity.
--
-- /Note:/ Consider using 'datasetCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iuDatasetCount :: Lens.Lens' IdentityUsage (Core.Maybe Core.Int)
iuDatasetCount = Lens.field @"datasetCount"
{-# INLINEABLE iuDatasetCount #-}
{-# DEPRECATED datasetCount "Use generic-lens or generic-optics with 'datasetCount' instead"  #-}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iuIdentityId :: Lens.Lens' IdentityUsage (Core.Maybe Types.IdentityId)
iuIdentityId = Lens.field @"identityId"
{-# INLINEABLE iuIdentityId #-}
{-# DEPRECATED identityId "Use generic-lens or generic-optics with 'identityId' instead"  #-}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iuIdentityPoolId :: Lens.Lens' IdentityUsage (Core.Maybe Types.IdentityPoolId)
iuIdentityPoolId = Lens.field @"identityPoolId"
{-# INLINEABLE iuIdentityPoolId #-}
{-# DEPRECATED identityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead"  #-}

-- | Date on which the identity was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iuLastModifiedDate :: Lens.Lens' IdentityUsage (Core.Maybe Core.NominalDiffTime)
iuLastModifiedDate = Lens.field @"lastModifiedDate"
{-# INLINEABLE iuLastModifiedDate #-}
{-# DEPRECATED lastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead"  #-}

instance Core.FromJSON IdentityUsage where
        parseJSON
          = Core.withObject "IdentityUsage" Core.$
              \ x ->
                IdentityUsage' Core.<$>
                  (x Core..:? "DataStorage") Core.<*> x Core..:? "DatasetCount"
                    Core.<*> x Core..:? "IdentityId"
                    Core.<*> x Core..:? "IdentityPoolId"
                    Core.<*> x Core..:? "LastModifiedDate"
