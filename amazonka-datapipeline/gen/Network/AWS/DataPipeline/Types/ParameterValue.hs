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
-- Module      : Network.AWS.DataPipeline.Types.ParameterValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.ParameterValue where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A value or list of parameter values.
--
-- /See:/ 'newParameterValue' smart constructor.
data ParameterValue = ParameterValue'
  { -- | The ID of the parameter value.
    id :: Core.Text,
    -- | The field value, expressed as a String.
    stringValue :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ParameterValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'parameterValue_id' - The ID of the parameter value.
--
-- 'stringValue', 'parameterValue_stringValue' - The field value, expressed as a String.
newParameterValue ::
  -- | 'id'
  Core.Text ->
  -- | 'stringValue'
  Core.Text ->
  ParameterValue
newParameterValue pId_ pStringValue_ =
  ParameterValue'
    { id = pId_,
      stringValue = pStringValue_
    }

-- | The ID of the parameter value.
parameterValue_id :: Lens.Lens' ParameterValue Core.Text
parameterValue_id = Lens.lens (\ParameterValue' {id} -> id) (\s@ParameterValue' {} a -> s {id = a} :: ParameterValue)

-- | The field value, expressed as a String.
parameterValue_stringValue :: Lens.Lens' ParameterValue Core.Text
parameterValue_stringValue = Lens.lens (\ParameterValue' {stringValue} -> stringValue) (\s@ParameterValue' {} a -> s {stringValue = a} :: ParameterValue)

instance Core.FromJSON ParameterValue where
  parseJSON =
    Core.withObject
      "ParameterValue"
      ( \x ->
          ParameterValue'
            Core.<$> (x Core..: "id") Core.<*> (x Core..: "stringValue")
      )

instance Core.Hashable ParameterValue

instance Core.NFData ParameterValue

instance Core.ToJSON ParameterValue where
  toJSON ParameterValue' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("id" Core..= id),
            Core.Just ("stringValue" Core..= stringValue)
          ]
      )
