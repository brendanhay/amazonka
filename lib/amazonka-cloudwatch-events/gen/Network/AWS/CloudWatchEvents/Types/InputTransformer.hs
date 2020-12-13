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
    itInputPathsMap,
    itInputTemplate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the parameters needed for you to provide custom input to a target based on one or more pieces of data extracted from the event.
--
-- /See:/ 'mkInputTransformer' smart constructor.
data InputTransformer = InputTransformer'
  { -- | Map of JSON paths to be extracted from the event. You can then insert these in the template in @InputTemplate@ to produce the output you want to be sent to the target.
    --
    -- @InputPathsMap@ is an array key-value pairs, where each value is a valid JSON path. You can have as many as 10 key-value pairs. You must use JSON dot notation, not bracket notation.
    -- The keys cannot start with "AWS."
    inputPathsMap :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
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
    inputTemplate :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputTransformer' with the minimum fields required to make a request.
--
-- * 'inputPathsMap' - Map of JSON paths to be extracted from the event. You can then insert these in the template in @InputTemplate@ to produce the output you want to be sent to the target.
--
-- @InputPathsMap@ is an array key-value pairs, where each value is a valid JSON path. You can have as many as 10 key-value pairs. You must use JSON dot notation, not bracket notation.
-- The keys cannot start with "AWS."
-- * 'inputTemplate' - Input template where you specify placeholders that will be filled with the values of the keys from @InputPathsMap@ to customize the data sent to the target. Enclose each @InputPathsMaps@ value in brackets: </value/ > The InputTemplate must be valid JSON.
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
mkInputTransformer ::
  -- | 'inputTemplate'
  Lude.Text ->
  InputTransformer
mkInputTransformer pInputTemplate_ =
  InputTransformer'
    { inputPathsMap = Lude.Nothing,
      inputTemplate = pInputTemplate_
    }

-- | Map of JSON paths to be extracted from the event. You can then insert these in the template in @InputTemplate@ to produce the output you want to be sent to the target.
--
-- @InputPathsMap@ is an array key-value pairs, where each value is a valid JSON path. You can have as many as 10 key-value pairs. You must use JSON dot notation, not bracket notation.
-- The keys cannot start with "AWS."
--
-- /Note:/ Consider using 'inputPathsMap' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itInputPathsMap :: Lens.Lens' InputTransformer (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
itInputPathsMap = Lens.lens (inputPathsMap :: InputTransformer -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {inputPathsMap = a} :: InputTransformer)
{-# DEPRECATED itInputPathsMap "Use generic-lens or generic-optics with 'inputPathsMap' instead." #-}

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
itInputTemplate :: Lens.Lens' InputTransformer Lude.Text
itInputTemplate = Lens.lens (inputTemplate :: InputTransformer -> Lude.Text) (\s a -> s {inputTemplate = a} :: InputTransformer)
{-# DEPRECATED itInputTemplate "Use generic-lens or generic-optics with 'inputTemplate' instead." #-}

instance Lude.FromJSON InputTransformer where
  parseJSON =
    Lude.withObject
      "InputTransformer"
      ( \x ->
          InputTransformer'
            Lude.<$> (x Lude..:? "InputPathsMap" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "InputTemplate")
      )

instance Lude.ToJSON InputTransformer where
  toJSON InputTransformer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InputPathsMap" Lude..=) Lude.<$> inputPathsMap,
            Lude.Just ("InputTemplate" Lude..= inputTemplate)
          ]
      )
