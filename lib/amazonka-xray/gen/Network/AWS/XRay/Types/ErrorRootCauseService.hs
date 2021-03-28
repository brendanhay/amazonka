{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.ErrorRootCauseService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.XRay.Types.ErrorRootCauseService
  ( ErrorRootCauseService (..)
  -- * Smart constructor
  , mkErrorRootCauseService
  -- * Lenses
  , ercsAccountId
  , ercsEntityPath
  , ercsInferred
  , ercsName
  , ercsNames
  , ercsType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.ErrorRootCauseEntity as Types

-- | A collection of fields identifying the services in a trace summary error.
--
-- /See:/ 'mkErrorRootCauseService' smart constructor.
data ErrorRootCauseService = ErrorRootCauseService'
  { accountId :: Core.Maybe Core.Text
    -- ^ The account ID associated to the service.
  , entityPath :: Core.Maybe [Types.ErrorRootCauseEntity]
    -- ^ The path of root cause entities found on the service. 
  , inferred :: Core.Maybe Core.Bool
    -- ^ A Boolean value indicating if the service is inferred from the trace.
  , name :: Core.Maybe Core.Text
    -- ^ The service name.
  , names :: Core.Maybe [Core.Text]
    -- ^ A collection of associated service names.
  , type' :: Core.Maybe Core.Text
    -- ^ The type associated to the service.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ErrorRootCauseService' value with any optional fields omitted.
mkErrorRootCauseService
    :: ErrorRootCauseService
mkErrorRootCauseService
  = ErrorRootCauseService'{accountId = Core.Nothing,
                           entityPath = Core.Nothing, inferred = Core.Nothing,
                           name = Core.Nothing, names = Core.Nothing, type' = Core.Nothing}

-- | The account ID associated to the service.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ercsAccountId :: Lens.Lens' ErrorRootCauseService (Core.Maybe Core.Text)
ercsAccountId = Lens.field @"accountId"
{-# INLINEABLE ercsAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The path of root cause entities found on the service. 
--
-- /Note:/ Consider using 'entityPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ercsEntityPath :: Lens.Lens' ErrorRootCauseService (Core.Maybe [Types.ErrorRootCauseEntity])
ercsEntityPath = Lens.field @"entityPath"
{-# INLINEABLE ercsEntityPath #-}
{-# DEPRECATED entityPath "Use generic-lens or generic-optics with 'entityPath' instead"  #-}

-- | A Boolean value indicating if the service is inferred from the trace.
--
-- /Note:/ Consider using 'inferred' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ercsInferred :: Lens.Lens' ErrorRootCauseService (Core.Maybe Core.Bool)
ercsInferred = Lens.field @"inferred"
{-# INLINEABLE ercsInferred #-}
{-# DEPRECATED inferred "Use generic-lens or generic-optics with 'inferred' instead"  #-}

-- | The service name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ercsName :: Lens.Lens' ErrorRootCauseService (Core.Maybe Core.Text)
ercsName = Lens.field @"name"
{-# INLINEABLE ercsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A collection of associated service names.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ercsNames :: Lens.Lens' ErrorRootCauseService (Core.Maybe [Core.Text])
ercsNames = Lens.field @"names"
{-# INLINEABLE ercsNames #-}
{-# DEPRECATED names "Use generic-lens or generic-optics with 'names' instead"  #-}

-- | The type associated to the service.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ercsType :: Lens.Lens' ErrorRootCauseService (Core.Maybe Core.Text)
ercsType = Lens.field @"type'"
{-# INLINEABLE ercsType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON ErrorRootCauseService where
        parseJSON
          = Core.withObject "ErrorRootCauseService" Core.$
              \ x ->
                ErrorRootCauseService' Core.<$>
                  (x Core..:? "AccountId") Core.<*> x Core..:? "EntityPath" Core.<*>
                    x Core..:? "Inferred"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "Names"
                    Core.<*> x Core..:? "Type"
