{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.InputTransformer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.InputTransformer where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains the parameters needed for you to provide custom input to a
-- target based on one or more pieces of data extracted from the event.
--
-- /See:/ 'newInputTransformer' smart constructor.
data InputTransformer = InputTransformer'
  { -- | Map of JSON paths to be extracted from the event. You can then insert
    -- these in the template in @InputTemplate@ to produce the output you want
    -- to be sent to the target.
    --
    -- @InputPathsMap@ is an array key-value pairs, where each value is a valid
    -- JSON path. You can have as many as 100 key-value pairs. You must use
    -- JSON dot notation, not bracket notation.
    --
    -- The keys cannot start with \"AWS.\"
    inputPathsMap :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Input template where you specify placeholders that will be filled with
    -- the values of the keys from @InputPathsMap@ to customize the data sent
    -- to the target. Enclose each @InputPathsMaps@ value in brackets:
    -- \</value/> The InputTemplate must be valid JSON.
    --
    -- If @InputTemplate@ is a JSON object (surrounded by curly braces), the
    -- following restrictions apply:
    --
    -- -   The placeholder cannot be used as an object key.
    --
    -- The following example shows the syntax for using @InputPathsMap@ and
    -- @InputTemplate@.
    --
    -- @ \"InputTransformer\":@
    --
    -- @{@
    --
    -- @\"InputPathsMap\": {\"instance\": \"$.detail.instance\",\"status\": \"$.detail.status\"},@
    --
    -- @\"InputTemplate\": \"\<instance> is in state \<status>\"@
    --
    -- @}@
    --
    -- To have the @InputTemplate@ include quote marks within a JSON string,
    -- escape each quote marks with a slash, as in the following example:
    --
    -- @ \"InputTransformer\":@
    --
    -- @{@
    --
    -- @\"InputPathsMap\": {\"instance\": \"$.detail.instance\",\"status\": \"$.detail.status\"},@
    --
    -- @\"InputTemplate\": \"\<instance> is in state \\\"\<status>\\\"\"@
    --
    -- @}@
    --
    -- The @InputTemplate@ can also be valid JSON with varibles in quotes or
    -- out, as in the following example:
    --
    -- @ \"InputTransformer\":@
    --
    -- @{@
    --
    -- @\"InputPathsMap\": {\"instance\": \"$.detail.instance\",\"status\": \"$.detail.status\"},@
    --
    -- @\"InputTemplate\": \'{\"myInstance\": \<instance>,\"myStatus\": \"\<instance> is in state \\\"\<status>\\\"\"}\'@
    --
    -- @}@
    inputTemplate :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InputTransformer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputPathsMap', 'inputTransformer_inputPathsMap' - Map of JSON paths to be extracted from the event. You can then insert
-- these in the template in @InputTemplate@ to produce the output you want
-- to be sent to the target.
--
-- @InputPathsMap@ is an array key-value pairs, where each value is a valid
-- JSON path. You can have as many as 100 key-value pairs. You must use
-- JSON dot notation, not bracket notation.
--
-- The keys cannot start with \"AWS.\"
--
-- 'inputTemplate', 'inputTransformer_inputTemplate' - Input template where you specify placeholders that will be filled with
-- the values of the keys from @InputPathsMap@ to customize the data sent
-- to the target. Enclose each @InputPathsMaps@ value in brackets:
-- \</value/> The InputTemplate must be valid JSON.
--
-- If @InputTemplate@ is a JSON object (surrounded by curly braces), the
-- following restrictions apply:
--
-- -   The placeholder cannot be used as an object key.
--
-- The following example shows the syntax for using @InputPathsMap@ and
-- @InputTemplate@.
--
-- @ \"InputTransformer\":@
--
-- @{@
--
-- @\"InputPathsMap\": {\"instance\": \"$.detail.instance\",\"status\": \"$.detail.status\"},@
--
-- @\"InputTemplate\": \"\<instance> is in state \<status>\"@
--
-- @}@
--
-- To have the @InputTemplate@ include quote marks within a JSON string,
-- escape each quote marks with a slash, as in the following example:
--
-- @ \"InputTransformer\":@
--
-- @{@
--
-- @\"InputPathsMap\": {\"instance\": \"$.detail.instance\",\"status\": \"$.detail.status\"},@
--
-- @\"InputTemplate\": \"\<instance> is in state \\\"\<status>\\\"\"@
--
-- @}@
--
-- The @InputTemplate@ can also be valid JSON with varibles in quotes or
-- out, as in the following example:
--
-- @ \"InputTransformer\":@
--
-- @{@
--
-- @\"InputPathsMap\": {\"instance\": \"$.detail.instance\",\"status\": \"$.detail.status\"},@
--
-- @\"InputTemplate\": \'{\"myInstance\": \<instance>,\"myStatus\": \"\<instance> is in state \\\"\<status>\\\"\"}\'@
--
-- @}@
newInputTransformer ::
  -- | 'inputTemplate'
  Core.Text ->
  InputTransformer
newInputTransformer pInputTemplate_ =
  InputTransformer'
    { inputPathsMap = Core.Nothing,
      inputTemplate = pInputTemplate_
    }

-- | Map of JSON paths to be extracted from the event. You can then insert
-- these in the template in @InputTemplate@ to produce the output you want
-- to be sent to the target.
--
-- @InputPathsMap@ is an array key-value pairs, where each value is a valid
-- JSON path. You can have as many as 100 key-value pairs. You must use
-- JSON dot notation, not bracket notation.
--
-- The keys cannot start with \"AWS.\"
inputTransformer_inputPathsMap :: Lens.Lens' InputTransformer (Core.Maybe (Core.HashMap Core.Text Core.Text))
inputTransformer_inputPathsMap = Lens.lens (\InputTransformer' {inputPathsMap} -> inputPathsMap) (\s@InputTransformer' {} a -> s {inputPathsMap = a} :: InputTransformer) Core.. Lens.mapping Lens._Coerce

-- | Input template where you specify placeholders that will be filled with
-- the values of the keys from @InputPathsMap@ to customize the data sent
-- to the target. Enclose each @InputPathsMaps@ value in brackets:
-- \</value/> The InputTemplate must be valid JSON.
--
-- If @InputTemplate@ is a JSON object (surrounded by curly braces), the
-- following restrictions apply:
--
-- -   The placeholder cannot be used as an object key.
--
-- The following example shows the syntax for using @InputPathsMap@ and
-- @InputTemplate@.
--
-- @ \"InputTransformer\":@
--
-- @{@
--
-- @\"InputPathsMap\": {\"instance\": \"$.detail.instance\",\"status\": \"$.detail.status\"},@
--
-- @\"InputTemplate\": \"\<instance> is in state \<status>\"@
--
-- @}@
--
-- To have the @InputTemplate@ include quote marks within a JSON string,
-- escape each quote marks with a slash, as in the following example:
--
-- @ \"InputTransformer\":@
--
-- @{@
--
-- @\"InputPathsMap\": {\"instance\": \"$.detail.instance\",\"status\": \"$.detail.status\"},@
--
-- @\"InputTemplate\": \"\<instance> is in state \\\"\<status>\\\"\"@
--
-- @}@
--
-- The @InputTemplate@ can also be valid JSON with varibles in quotes or
-- out, as in the following example:
--
-- @ \"InputTransformer\":@
--
-- @{@
--
-- @\"InputPathsMap\": {\"instance\": \"$.detail.instance\",\"status\": \"$.detail.status\"},@
--
-- @\"InputTemplate\": \'{\"myInstance\": \<instance>,\"myStatus\": \"\<instance> is in state \\\"\<status>\\\"\"}\'@
--
-- @}@
inputTransformer_inputTemplate :: Lens.Lens' InputTransformer Core.Text
inputTransformer_inputTemplate = Lens.lens (\InputTransformer' {inputTemplate} -> inputTemplate) (\s@InputTransformer' {} a -> s {inputTemplate = a} :: InputTransformer)

instance Core.FromJSON InputTransformer where
  parseJSON =
    Core.withObject
      "InputTransformer"
      ( \x ->
          InputTransformer'
            Core.<$> (x Core..:? "InputPathsMap" Core..!= Core.mempty)
            Core.<*> (x Core..: "InputTemplate")
      )

instance Core.Hashable InputTransformer

instance Core.NFData InputTransformer

instance Core.ToJSON InputTransformer where
  toJSON InputTransformer' {..} =
    Core.object
      ( Core.catMaybes
          [ ("InputPathsMap" Core..=) Core.<$> inputPathsMap,
            Core.Just ("InputTemplate" Core..= inputTemplate)
          ]
      )
