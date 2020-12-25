{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.ErrorRootCauseService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ErrorRootCauseService
  ( ErrorRootCauseService (..),

    -- * Smart constructor
    mkErrorRootCauseService,

    -- * Lenses
    ercsAccountId,
    ercsEntityPath,
    ercsInferred,
    ercsName,
    ercsNames,
    ercsType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.AccountId as Types
import qualified Network.AWS.XRay.Types.ErrorRootCauseEntity as Types
import qualified Network.AWS.XRay.Types.Name as Types
import qualified Network.AWS.XRay.Types.String as Types
import qualified Network.AWS.XRay.Types.Type as Types

-- | A collection of fields identifying the services in a trace summary error.
--
-- /See:/ 'mkErrorRootCauseService' smart constructor.
data ErrorRootCauseService = ErrorRootCauseService'
  { -- | The account ID associated to the service.
    accountId :: Core.Maybe Types.AccountId,
    -- | The path of root cause entities found on the service.
    entityPath :: Core.Maybe [Types.ErrorRootCauseEntity],
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

-- | Creates a 'ErrorRootCauseService' value with any optional fields omitted.
mkErrorRootCauseService ::
  ErrorRootCauseService
mkErrorRootCauseService =
  ErrorRootCauseService'
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
ercsAccountId :: Lens.Lens' ErrorRootCauseService (Core.Maybe Types.AccountId)
ercsAccountId = Lens.field @"accountId"
{-# DEPRECATED ercsAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The path of root cause entities found on the service.
--
-- /Note:/ Consider using 'entityPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ercsEntityPath :: Lens.Lens' ErrorRootCauseService (Core.Maybe [Types.ErrorRootCauseEntity])
ercsEntityPath = Lens.field @"entityPath"
{-# DEPRECATED ercsEntityPath "Use generic-lens or generic-optics with 'entityPath' instead." #-}

-- | A Boolean value indicating if the service is inferred from the trace.
--
-- /Note:/ Consider using 'inferred' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ercsInferred :: Lens.Lens' ErrorRootCauseService (Core.Maybe Core.Bool)
ercsInferred = Lens.field @"inferred"
{-# DEPRECATED ercsInferred "Use generic-lens or generic-optics with 'inferred' instead." #-}

-- | The service name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ercsName :: Lens.Lens' ErrorRootCauseService (Core.Maybe Types.Name)
ercsName = Lens.field @"name"
{-# DEPRECATED ercsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A collection of associated service names.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ercsNames :: Lens.Lens' ErrorRootCauseService (Core.Maybe [Types.String])
ercsNames = Lens.field @"names"
{-# DEPRECATED ercsNames "Use generic-lens or generic-optics with 'names' instead." #-}

-- | The type associated to the service.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ercsType :: Lens.Lens' ErrorRootCauseService (Core.Maybe Types.Type)
ercsType = Lens.field @"type'"
{-# DEPRECATED ercsType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON ErrorRootCauseService where
  parseJSON =
    Core.withObject "ErrorRootCauseService" Core.$
      \x ->
        ErrorRootCauseService'
          Core.<$> (x Core..:? "AccountId")
          Core.<*> (x Core..:? "EntityPath")
          Core.<*> (x Core..:? "Inferred")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "Names")
          Core.<*> (x Core..:? "Type")
