{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types.IdentityPoolUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoSync.Types.IdentityPoolUsage
  ( IdentityPoolUsage (..)
  -- * Smart constructor
  , mkIdentityPoolUsage
  -- * Lenses
  , ipuDataStorage
  , ipuIdentityPoolId
  , ipuLastModifiedDate
  , ipuSyncSessionsCount
  ) where

import qualified Network.AWS.CognitoSync.Types.IdentityPoolId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Usage information for the identity pool.
--
-- /See:/ 'mkIdentityPoolUsage' smart constructor.
data IdentityPoolUsage = IdentityPoolUsage'
  { dataStorage :: Core.Maybe Core.Integer
    -- ^ Data storage information for the identity pool.
  , identityPoolId :: Core.Maybe Types.IdentityPoolId
    -- ^ A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
  , lastModifiedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ Date on which the identity pool was last modified.
  , syncSessionsCount :: Core.Maybe Core.Integer
    -- ^ Number of sync sessions for the identity pool.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'IdentityPoolUsage' value with any optional fields omitted.
mkIdentityPoolUsage
    :: IdentityPoolUsage
mkIdentityPoolUsage
  = IdentityPoolUsage'{dataStorage = Core.Nothing,
                       identityPoolId = Core.Nothing, lastModifiedDate = Core.Nothing,
                       syncSessionsCount = Core.Nothing}

-- | Data storage information for the identity pool.
--
-- /Note:/ Consider using 'dataStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipuDataStorage :: Lens.Lens' IdentityPoolUsage (Core.Maybe Core.Integer)
ipuDataStorage = Lens.field @"dataStorage"
{-# INLINEABLE ipuDataStorage #-}
{-# DEPRECATED dataStorage "Use generic-lens or generic-optics with 'dataStorage' instead"  #-}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipuIdentityPoolId :: Lens.Lens' IdentityPoolUsage (Core.Maybe Types.IdentityPoolId)
ipuIdentityPoolId = Lens.field @"identityPoolId"
{-# INLINEABLE ipuIdentityPoolId #-}
{-# DEPRECATED identityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead"  #-}

-- | Date on which the identity pool was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipuLastModifiedDate :: Lens.Lens' IdentityPoolUsage (Core.Maybe Core.NominalDiffTime)
ipuLastModifiedDate = Lens.field @"lastModifiedDate"
{-# INLINEABLE ipuLastModifiedDate #-}
{-# DEPRECATED lastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead"  #-}

-- | Number of sync sessions for the identity pool.
--
-- /Note:/ Consider using 'syncSessionsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipuSyncSessionsCount :: Lens.Lens' IdentityPoolUsage (Core.Maybe Core.Integer)
ipuSyncSessionsCount = Lens.field @"syncSessionsCount"
{-# INLINEABLE ipuSyncSessionsCount #-}
{-# DEPRECATED syncSessionsCount "Use generic-lens or generic-optics with 'syncSessionsCount' instead"  #-}

instance Core.FromJSON IdentityPoolUsage where
        parseJSON
          = Core.withObject "IdentityPoolUsage" Core.$
              \ x ->
                IdentityPoolUsage' Core.<$>
                  (x Core..:? "DataStorage") Core.<*> x Core..:? "IdentityPoolId"
                    Core.<*> x Core..:? "LastModifiedDate"
                    Core.<*> x Core..:? "SyncSessionsCount"
