{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.ResponseTimeRootCauseService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ResponseTimeRootCauseService
  ( ResponseTimeRootCauseService (..),

    -- * Smart constructor
    mkResponseTimeRootCauseService,

    -- * Lenses
    rtrcsAccountId,
    rtrcsEntityPath,
    rtrcsInferred,
    rtrcsName,
    rtrcsNames,
    rtrcsType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.AccountId as Types
import qualified Network.AWS.XRay.Types.Name as Types
import qualified Network.AWS.XRay.Types.ResponseTimeRootCauseEntity as Types
import qualified Network.AWS.XRay.Types.String as Types
import qualified Network.AWS.XRay.Types.Type as Types

-- | A collection of fields identifying the service in a response time warning.
--
-- /See:/ 'mkResponseTimeRootCauseService' smart constructor.
data ResponseTimeRootCauseService = ResponseTimeRootCauseService'
  { -- | The account ID associated to the service.
    accountId :: Core.Maybe Types.AccountId,
    -- | The path of root cause entities found on the service.
    entityPath :: Core.Maybe [Types.ResponseTimeRootCauseEntity],
    -- | A Boolean value indicating if the service is inferred from the trace.
    inferred :: Core.Maybe Core.Bool,
    -- | The service name.
    name :: Core.Maybe Types.Name,
    -- | A collection of associated service names.
    names :: Core.Maybe [Types.String],
    -- | The type associated to the service.
    type' :: Core.Maybe Types.Type
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResponseTimeRootCauseService' value with any optional fields omitted.
mkResponseTimeRootCauseService ::
  ResponseTimeRootCauseService
mkResponseTimeRootCauseService =
  ResponseTimeRootCauseService'
    { accountId = Core.Nothing,
      entityPath = Core.Nothing,
      inferred = Core.Nothing,
      name = Core.Nothing,
      names = Core.Nothing,
      type' = Core.Nothing
    }

-- | The account ID associated to the service.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrcsAccountId :: Lens.Lens' ResponseTimeRootCauseService (Core.Maybe Types.AccountId)
rtrcsAccountId = Lens.field @"accountId"
{-# DEPRECATED rtrcsAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The path of root cause entities found on the service.
--
-- /Note:/ Consider using 'entityPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrcsEntityPath :: Lens.Lens' ResponseTimeRootCauseService (Core.Maybe [Types.ResponseTimeRootCauseEntity])
rtrcsEntityPath = Lens.field @"entityPath"
{-# DEPRECATED rtrcsEntityPath "Use generic-lens or generic-optics with 'entityPath' instead." #-}

-- | A Boolean value indicating if the service is inferred from the trace.
--
-- /Note:/ Consider using 'inferred' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrcsInferred :: Lens.Lens' ResponseTimeRootCauseService (Core.Maybe Core.Bool)
rtrcsInferred = Lens.field @"inferred"
{-# DEPRECATED rtrcsInferred "Use generic-lens or generic-optics with 'inferred' instead." #-}

-- | The service name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrcsName :: Lens.Lens' ResponseTimeRootCauseService (Core.Maybe Types.Name)
rtrcsName = Lens.field @"name"
{-# DEPRECATED rtrcsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A collection of associated service names.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrcsNames :: Lens.Lens' ResponseTimeRootCauseService (Core.Maybe [Types.String])
rtrcsNames = Lens.field @"names"
{-# DEPRECATED rtrcsNames "Use generic-lens or generic-optics with 'names' instead." #-}

-- | The type associated to the service.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrcsType :: Lens.Lens' ResponseTimeRootCauseService (Core.Maybe Types.Type)
rtrcsType = Lens.field @"type'"
{-# DEPRECATED rtrcsType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON ResponseTimeRootCauseService where
  parseJSON =
    Core.withObject "ResponseTimeRootCauseService" Core.$
      \x ->
        ResponseTimeRootCauseService'
          Core.<$> (x Core..:? "AccountId")
          Core.<*> (x Core..:? "EntityPath")
          Core.<*> (x Core..:? "Inferred")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "Names")
          Core.<*> (x Core..:? "Type")
