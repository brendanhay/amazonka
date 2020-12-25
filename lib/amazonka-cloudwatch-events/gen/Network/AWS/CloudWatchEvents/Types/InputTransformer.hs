{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.InputTransformer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.InputTransformer
  ( InputTransformer (..),

    -- * Smart constructor
    mkInputTransformer,

    -- * Lenses
    itInputTemplate,
    itInputPathsMap,
  )
where

import qualified Network.AWS.CloudWatchEvents.Types.InputTemplate as Types
import qualified Network.AWS.CloudWatchEvents.Types.InputTransformerPathKey as Types
import qualified Network.AWS.CloudWatchEvents.Types.TargetInputPath as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the parameters needed for you to provide custom input to a target based on one or more pieces of data extracted from the event.
--
-- /See:/ 'mkInputTransformer' smart constructor.
data InputTransformer = InputTransformer'
  { -- | Input template where you specify placeholders that will be filled with the values of the keys from @InputPathsMap@ to customize the data sent to the target. Enclose each @InputPathsMaps@ value in brackets: </value/ > The InputTemplate must be valid JSON.
    --
    -- If @InputTemplate@ is a JSON object (surrounded by curly braces), the following restrictions apply:
    --
    --     * The placeholder cannot be used as an object key.
    --
    --
    --     * Object values cannot include quote marks.
    --
    --
    -- The following example shows the syntax for using @InputPathsMap@ and @InputTemplate@ .
    -- @"InputTransformer":@
    -- @{@
    -- @"InputPathsMap": {"instance": "$.detail.instance","status": "$.detail.status"},@
    -- @"InputTemplate": "<instance> is in state <status>"@
    -- @}@
    -- To have the @InputTemplate@ include quote marks within a JSON string, escape each quote marks with a slash, as in the following example:
    -- @"InputTransformer":@
    -- @{@
    -- @"InputPathsMap": {"instance": "$.detail.instance","status": "$.detail.status"},@
    -- @"InputTemplate": "<instance> is in state \"<status>\""@
    -- @}@
    inputTemplate :: Types.InputTemplate,
    -- | Map of JSON paths to be extracted from the event. You can then insert these in the template in @InputTemplate@ to produce the output you want to be sent to the target.
    --
    -- @InputPathsMap@ is an array key-value pairs, where each value is a valid JSON path. You can have as many as 10 key-value pairs. You must use JSON dot notation, not bracket notation.
    -- The keys cannot start with "AWS."
    inputPathsMap :: Core.Maybe (Core.HashMap Types.InputTransformerPathKey Types.TargetInputPath)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InputTransformer' value with any optional fields omitted.
mkInputTransformer ::
  -- | 'inputTemplate'
  Types.InputTemplate ->
  InputTransformer
mkInputTransformer inputTemplate =
  InputTransformer' {inputTemplate, inputPathsMap = Core.Nothing}

-- | Input template where you specify placeholders that will be filled with the values of the keys from @InputPathsMap@ to customize the data sent to the target. Enclose each @InputPathsMaps@ value in brackets: </value/ > The InputTemplate must be valid JSON.
--
-- If @InputTemplate@ is a JSON object (surrounded by curly braces), the following restrictions apply:
--
--     * The placeholder cannot be used as an object key.
--
--
--     * Object values cannot include quote marks.
--
--
-- The following example shows the syntax for using @InputPathsMap@ and @InputTemplate@ .
-- @"InputTransformer":@
-- @{@
-- @"InputPathsMap": {"instance": "$.detail.instance","status": "$.detail.status"},@
-- @"InputTemplate": "<instance> is in state <status>"@
-- @}@
-- To have the @InputTemplate@ include quote marks within a JSON string, escape each quote marks with a slash, as in the following example:
-- @"InputTransformer":@
-- @{@
-- @"InputPathsMap": {"instance": "$.detail.instance","status": "$.detail.status"},@
-- @"InputTemplate": "<instance> is in state \"<status>\""@
-- @}@
--
-- /Note:/ Consider using 'inputTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itInputTemplate :: Lens.Lens' InputTransformer Types.InputTemplate
itInputTemplate = Lens.field @"inputTemplate"
{-# DEPRECATED itInputTemplate "Use generic-lens or generic-optics with 'inputTemplate' instead." #-}

-- | Map of JSON paths to be extracted from the event. You can then insert these in the template in @InputTemplate@ to produce the output you want to be sent to the target.
--
-- @InputPathsMap@ is an array key-value pairs, where each value is a valid JSON path. You can have as many as 10 key-value pairs. You must use JSON dot notation, not bracket notation.
-- The keys cannot start with "AWS."
--
-- /Note:/ Consider using 'inputPathsMap' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itInputPathsMap :: Lens.Lens' InputTransformer (Core.Maybe (Core.HashMap Types.InputTransformerPathKey Types.TargetInputPath))
itInputPathsMap = Lens.field @"inputPathsMap"
{-# DEPRECATED itInputPathsMap "Use generic-lens or generic-optics with 'inputPathsMap' instead." #-}

instance Core.FromJSON InputTransformer where
  toJSON InputTransformer {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("InputTemplate" Core..= inputTemplate),
            ("InputPathsMap" Core..=) Core.<$> inputPathsMap
          ]
      )

instance Core.FromJSON InputTransformer where
  parseJSON =
    Core.withObject "InputTransformer" Core.$
      \x ->
        InputTransformer'
          Core.<$> (x Core..: "InputTemplate") Core.<*> (x Core..:? "InputPathsMap")
