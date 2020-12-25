{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ExecutionParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ExecutionParameter
  ( ExecutionParameter (..),

    -- * Smart constructor
    mkExecutionParameter,

    -- * Lenses
    epDefaultValues,
    epName,
    epType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.ExecutionParameterKey as Types
import qualified Network.AWS.ServiceCatalog.Types.ExecutionParameterValue as Types
import qualified Network.AWS.ServiceCatalog.Types.Type as Types

-- | Details of an execution parameter value that is passed to a self-service action when executed on a provisioned product.
--
-- /See:/ 'mkExecutionParameter' smart constructor.
data ExecutionParameter = ExecutionParameter'
  { -- | The default values for the execution parameter.
    defaultValues :: Core.Maybe [Types.ExecutionParameterValue],
    -- | The name of the execution parameter.
    name :: Core.Maybe Types.ExecutionParameterKey,
    -- | The execution parameter type.
    type' :: Core.Maybe Types.Type
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExecutionParameter' value with any optional fields omitted.
mkExecutionParameter ::
  ExecutionParameter
mkExecutionParameter =
  ExecutionParameter'
    { defaultValues = Core.Nothing,
      name = Core.Nothing,
      type' = Core.Nothing
    }

-- | The default values for the execution parameter.
--
-- /Note:/ Consider using 'defaultValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epDefaultValues :: Lens.Lens' ExecutionParameter (Core.Maybe [Types.ExecutionParameterValue])
epDefaultValues = Lens.field @"defaultValues"
{-# DEPRECATED epDefaultValues "Use generic-lens or generic-optics with 'defaultValues' instead." #-}

-- | The name of the execution parameter.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epName :: Lens.Lens' ExecutionParameter (Core.Maybe Types.ExecutionParameterKey)
epName = Lens.field @"name"
{-# DEPRECATED epName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The execution parameter type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epType :: Lens.Lens' ExecutionParameter (Core.Maybe Types.Type)
epType = Lens.field @"type'"
{-# DEPRECATED epType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON ExecutionParameter where
  parseJSON =
    Core.withObject "ExecutionParameter" Core.$
      \x ->
        ExecutionParameter'
          Core.<$> (x Core..:? "DefaultValues")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "Type")
