{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.ExportFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.ExportFilter
  ( ExportFilter (..),

    -- * Smart constructor
    mkExportFilter,

    -- * Lenses
    efName,
    efValues,
    efCondition,
  )
where

import qualified Network.AWS.Discovery.Types.Condition as Types
import qualified Network.AWS.Discovery.Types.FilterName as Types
import qualified Network.AWS.Discovery.Types.FilterValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Used to select which agent's data is to be exported. A single agent ID may be selected for export using the <http://docs.aws.amazon.com/application-discovery/latest/APIReference/API_StartExportTask.html StartExportTask> action.
--
-- /See:/ 'mkExportFilter' smart constructor.
data ExportFilter = ExportFilter'
  { -- | A single @ExportFilter@ name. Supported filters: @agentId@ .
    name :: Types.FilterName,
    -- | A single @agentId@ for a Discovery Agent. An @agentId@ can be found using the <http://docs.aws.amazon.com/application-discovery/latest/APIReference/API_DescribeExportTasks.html DescribeAgents> action. Typically an ADS @agentId@ is in the form @o-0123456789abcdef0@ .
    values :: [Types.FilterValue],
    -- | Supported condition: @EQUALS@
    condition :: Types.Condition
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExportFilter' value with any optional fields omitted.
mkExportFilter ::
  -- | 'name'
  Types.FilterName ->
  -- | 'condition'
  Types.Condition ->
  ExportFilter
mkExportFilter name condition =
  ExportFilter' {name, values = Core.mempty, condition}

-- | A single @ExportFilter@ name. Supported filters: @agentId@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efName :: Lens.Lens' ExportFilter Types.FilterName
efName = Lens.field @"name"
{-# DEPRECATED efName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A single @agentId@ for a Discovery Agent. An @agentId@ can be found using the <http://docs.aws.amazon.com/application-discovery/latest/APIReference/API_DescribeExportTasks.html DescribeAgents> action. Typically an ADS @agentId@ is in the form @o-0123456789abcdef0@ .
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efValues :: Lens.Lens' ExportFilter [Types.FilterValue]
efValues = Lens.field @"values"
{-# DEPRECATED efValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | Supported condition: @EQUALS@
--
-- /Note:/ Consider using 'condition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efCondition :: Lens.Lens' ExportFilter Types.Condition
efCondition = Lens.field @"condition"
{-# DEPRECATED efCondition "Use generic-lens or generic-optics with 'condition' instead." #-}

instance Core.FromJSON ExportFilter where
  toJSON ExportFilter {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            Core.Just ("values" Core..= values),
            Core.Just ("condition" Core..= condition)
          ]
      )
