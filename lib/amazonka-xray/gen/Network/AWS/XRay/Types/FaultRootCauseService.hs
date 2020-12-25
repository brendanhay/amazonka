{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.FaultRootCauseService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.FaultRootCauseService
  ( FaultRootCauseService (..),

    -- * Smart constructor
    mkFaultRootCauseService,

    -- * Lenses
    frcsAccountId,
    frcsEntityPath,
    frcsInferred,
    frcsName,
    frcsNames,
    frcsType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.FaultRootCauseEntity as Types
import qualified Network.AWS.XRay.Types.String as Types

-- | A collection of fields identifying the services in a trace summary fault.
--
-- /See:/ 'mkFaultRootCauseService' smart constructor.
data FaultRootCauseService = FaultRootCauseService'
  { -- | The account ID associated to the service.
    accountId :: Core.Maybe Types.String,
    -- | The path of root cause entities found on the service.
    entityPath :: Core.Maybe [Types.FaultRootCauseEntity],
    -- | A Boolean value indicating if the service is inferred from the trace.
    inferred :: Core.Maybe Core.Bool,
    -- | The service name.
    name :: Core.Maybe Types.String,
    -- | A collection of associated service names.
    names :: Core.Maybe [Types.String],
    -- | The type associated to the service.
    type' :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FaultRootCauseService' value with any optional fields omitted.
mkFaultRootCauseService ::
  FaultRootCauseService
mkFaultRootCauseService =
  FaultRootCauseService'
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
frcsAccountId :: Lens.Lens' FaultRootCauseService (Core.Maybe Types.String)
frcsAccountId = Lens.field @"accountId"
{-# DEPRECATED frcsAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The path of root cause entities found on the service.
--
-- /Note:/ Consider using 'entityPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frcsEntityPath :: Lens.Lens' FaultRootCauseService (Core.Maybe [Types.FaultRootCauseEntity])
frcsEntityPath = Lens.field @"entityPath"
{-# DEPRECATED frcsEntityPath "Use generic-lens or generic-optics with 'entityPath' instead." #-}

-- | A Boolean value indicating if the service is inferred from the trace.
--
-- /Note:/ Consider using 'inferred' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frcsInferred :: Lens.Lens' FaultRootCauseService (Core.Maybe Core.Bool)
frcsInferred = Lens.field @"inferred"
{-# DEPRECATED frcsInferred "Use generic-lens or generic-optics with 'inferred' instead." #-}

-- | The service name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frcsName :: Lens.Lens' FaultRootCauseService (Core.Maybe Types.String)
frcsName = Lens.field @"name"
{-# DEPRECATED frcsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A collection of associated service names.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frcsNames :: Lens.Lens' FaultRootCauseService (Core.Maybe [Types.String])
frcsNames = Lens.field @"names"
{-# DEPRECATED frcsNames "Use generic-lens or generic-optics with 'names' instead." #-}

-- | The type associated to the service.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frcsType :: Lens.Lens' FaultRootCauseService (Core.Maybe Types.String)
frcsType = Lens.field @"type'"
{-# DEPRECATED frcsType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON FaultRootCauseService where
  parseJSON =
    Core.withObject "FaultRootCauseService" Core.$
      \x ->
        FaultRootCauseService'
          Core.<$> (x Core..:? "AccountId")
          Core.<*> (x Core..:? "EntityPath")
          Core.<*> (x Core..:? "Inferred")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "Names")
          Core.<*> (x Core..:? "Type")
