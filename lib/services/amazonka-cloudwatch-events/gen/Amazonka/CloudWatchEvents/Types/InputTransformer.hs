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
-- Module      : Amazonka.CloudWatchEvents.Types.InputTransformer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.InputTransformer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
    -- The keys cannot start with \"Amazon Web Services.\"
    inputPathsMap :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Input template where you specify placeholders that will be filled with
    -- the values of the keys from @InputPathsMap@ to customize the data sent
    -- to the target. Enclose each @InputPathsMaps@ value in brackets:
    -- \</value/>
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
    inputTemplate :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- The keys cannot start with \"Amazon Web Services.\"
--
-- 'inputTemplate', 'inputTransformer_inputTemplate' - Input template where you specify placeholders that will be filled with
-- the values of the keys from @InputPathsMap@ to customize the data sent
-- to the target. Enclose each @InputPathsMaps@ value in brackets:
-- \</value/>
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
  Prelude.Text ->
  InputTransformer
newInputTransformer pInputTemplate_ =
  InputTransformer'
    { inputPathsMap = Prelude.Nothing,
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
-- The keys cannot start with \"Amazon Web Services.\"
inputTransformer_inputPathsMap :: Lens.Lens' InputTransformer (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
inputTransformer_inputPathsMap = Lens.lens (\InputTransformer' {inputPathsMap} -> inputPathsMap) (\s@InputTransformer' {} a -> s {inputPathsMap = a} :: InputTransformer) Prelude.. Lens.mapping Lens.coerced

-- | Input template where you specify placeholders that will be filled with
-- the values of the keys from @InputPathsMap@ to customize the data sent
-- to the target. Enclose each @InputPathsMaps@ value in brackets:
-- \</value/>
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
inputTransformer_inputTemplate :: Lens.Lens' InputTransformer Prelude.Text
inputTransformer_inputTemplate = Lens.lens (\InputTransformer' {inputTemplate} -> inputTemplate) (\s@InputTransformer' {} a -> s {inputTemplate = a} :: InputTransformer)

instance Data.FromJSON InputTransformer where
  parseJSON =
    Data.withObject
      "InputTransformer"
      ( \x ->
          InputTransformer'
            Prelude.<$> (x Data..:? "InputPathsMap" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "InputTemplate")
      )

instance Prelude.Hashable InputTransformer where
  hashWithSalt _salt InputTransformer' {..} =
    _salt
      `Prelude.hashWithSalt` inputPathsMap
      `Prelude.hashWithSalt` inputTemplate

instance Prelude.NFData InputTransformer where
  rnf InputTransformer' {..} =
    Prelude.rnf inputPathsMap
      `Prelude.seq` Prelude.rnf inputTemplate

instance Data.ToJSON InputTransformer where
  toJSON InputTransformer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InputPathsMap" Data..=) Prelude.<$> inputPathsMap,
            Prelude.Just
              ("InputTemplate" Data..= inputTemplate)
          ]
      )
