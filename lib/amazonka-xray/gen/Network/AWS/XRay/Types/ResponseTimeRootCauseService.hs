{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.ResponseTimeRootCauseService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.XRay.Types.ResponseTimeRootCauseService
  ( ResponseTimeRootCauseService (..)
  -- * Smart constructor
  , mkResponseTimeRootCauseService
  -- * Lenses
  , rtrcsAccountId
  , rtrcsEntityPath
  , rtrcsInferred
  , rtrcsName
  , rtrcsNames
  , rtrcsType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.ResponseTimeRootCauseEntity as Types

-- | A collection of fields identifying the service in a response time warning.
--
-- /See:/ 'mkResponseTimeRootCauseService' smart constructor.
data ResponseTimeRootCauseService = ResponseTimeRootCauseService'
  { accountId :: Core.Maybe Core.Text
    -- ^ The account ID associated to the service.
  , entityPath :: Core.Maybe [Types.ResponseTimeRootCauseEntity]
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

-- | Creates a 'ResponseTimeRootCauseService' value with any optional fields omitted.
mkResponseTimeRootCauseService
    :: ResponseTimeRootCauseService
mkResponseTimeRootCauseService
  = ResponseTimeRootCauseService'{accountId = Core.Nothing,
                                  entityPath = Core.Nothing, inferred = Core.Nothing,
                                  name = Core.Nothing, names = Core.Nothing, type' = Core.Nothing}

-- | The account ID associated to the service.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrcsAccountId :: Lens.Lens' ResponseTimeRootCauseService (Core.Maybe Core.Text)
rtrcsAccountId = Lens.field @"accountId"
{-# INLINEABLE rtrcsAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The path of root cause entities found on the service. 
--
-- /Note:/ Consider using 'entityPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrcsEntityPath :: Lens.Lens' ResponseTimeRootCauseService (Core.Maybe [Types.ResponseTimeRootCauseEntity])
rtrcsEntityPath = Lens.field @"entityPath"
{-# INLINEABLE rtrcsEntityPath #-}
{-# DEPRECATED entityPath "Use generic-lens or generic-optics with 'entityPath' instead"  #-}

-- | A Boolean value indicating if the service is inferred from the trace.
--
-- /Note:/ Consider using 'inferred' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrcsInferred :: Lens.Lens' ResponseTimeRootCauseService (Core.Maybe Core.Bool)
rtrcsInferred = Lens.field @"inferred"
{-# INLINEABLE rtrcsInferred #-}
{-# DEPRECATED inferred "Use generic-lens or generic-optics with 'inferred' instead"  #-}

-- | The service name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrcsName :: Lens.Lens' ResponseTimeRootCauseService (Core.Maybe Core.Text)
rtrcsName = Lens.field @"name"
{-# INLINEABLE rtrcsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A collection of associated service names.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrcsNames :: Lens.Lens' ResponseTimeRootCauseService (Core.Maybe [Core.Text])
rtrcsNames = Lens.field @"names"
{-# INLINEABLE rtrcsNames #-}
{-# DEPRECATED names "Use generic-lens or generic-optics with 'names' instead"  #-}

-- | The type associated to the service.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrcsType :: Lens.Lens' ResponseTimeRootCauseService (Core.Maybe Core.Text)
rtrcsType = Lens.field @"type'"
{-# INLINEABLE rtrcsType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON ResponseTimeRootCauseService where
        parseJSON
          = Core.withObject "ResponseTimeRootCauseService" Core.$
              \ x ->
                ResponseTimeRootCauseService' Core.<$>
                  (x Core..:? "AccountId") Core.<*> x Core..:? "EntityPath" Core.<*>
                    x Core..:? "Inferred"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "Names"
                    Core.<*> x Core..:? "Type"
