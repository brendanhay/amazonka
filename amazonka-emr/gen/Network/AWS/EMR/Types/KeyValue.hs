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
-- Module      : Network.AWS.EMR.Types.KeyValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.KeyValue where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A key-value pair.
--
-- /See:/ 'newKeyValue' smart constructor.
data KeyValue = KeyValue'
  { -- | The unique identifier of a key-value pair.
    key :: Core.Maybe Core.Text,
    -- | The value part of the identified key.
    value :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'KeyValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'keyValue_key' - The unique identifier of a key-value pair.
--
-- 'value', 'keyValue_value' - The value part of the identified key.
newKeyValue ::
  KeyValue
newKeyValue =
  KeyValue' {key = Core.Nothing, value = Core.Nothing}

-- | The unique identifier of a key-value pair.
keyValue_key :: Lens.Lens' KeyValue (Core.Maybe Core.Text)
keyValue_key = Lens.lens (\KeyValue' {key} -> key) (\s@KeyValue' {} a -> s {key = a} :: KeyValue)

-- | The value part of the identified key.
keyValue_value :: Lens.Lens' KeyValue (Core.Maybe Core.Text)
keyValue_value = Lens.lens (\KeyValue' {value} -> value) (\s@KeyValue' {} a -> s {value = a} :: KeyValue)

instance Core.Hashable KeyValue

instance Core.NFData KeyValue

instance Core.ToJSON KeyValue where
  toJSON KeyValue' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Key" Core..=) Core.<$> key,
            ("Value" Core..=) Core.<$> value
          ]
      )
