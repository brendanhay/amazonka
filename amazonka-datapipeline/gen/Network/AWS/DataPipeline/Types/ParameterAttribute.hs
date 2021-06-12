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
-- Module      : Network.AWS.DataPipeline.Types.ParameterAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.ParameterAttribute where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The attributes allowed or specified with a parameter object.
--
-- /See:/ 'newParameterAttribute' smart constructor.
data ParameterAttribute = ParameterAttribute'
  { -- | The field identifier.
    key :: Core.Text,
    -- | The field value, expressed as a String.
    stringValue :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ParameterAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'parameterAttribute_key' - The field identifier.
--
-- 'stringValue', 'parameterAttribute_stringValue' - The field value, expressed as a String.
newParameterAttribute ::
  -- | 'key'
  Core.Text ->
  -- | 'stringValue'
  Core.Text ->
  ParameterAttribute
newParameterAttribute pKey_ pStringValue_ =
  ParameterAttribute'
    { key = pKey_,
      stringValue = pStringValue_
    }

-- | The field identifier.
parameterAttribute_key :: Lens.Lens' ParameterAttribute Core.Text
parameterAttribute_key = Lens.lens (\ParameterAttribute' {key} -> key) (\s@ParameterAttribute' {} a -> s {key = a} :: ParameterAttribute)

-- | The field value, expressed as a String.
parameterAttribute_stringValue :: Lens.Lens' ParameterAttribute Core.Text
parameterAttribute_stringValue = Lens.lens (\ParameterAttribute' {stringValue} -> stringValue) (\s@ParameterAttribute' {} a -> s {stringValue = a} :: ParameterAttribute)

instance Core.FromJSON ParameterAttribute where
  parseJSON =
    Core.withObject
      "ParameterAttribute"
      ( \x ->
          ParameterAttribute'
            Core.<$> (x Core..: "key") Core.<*> (x Core..: "stringValue")
      )

instance Core.Hashable ParameterAttribute

instance Core.NFData ParameterAttribute

instance Core.ToJSON ParameterAttribute where
  toJSON ParameterAttribute' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("key" Core..= key),
            Core.Just ("stringValue" Core..= stringValue)
          ]
      )
