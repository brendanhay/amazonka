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
-- Module      : Network.AWS.CloudWatchEvents.Types.ConnectionBodyParameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.ConnectionBodyParameter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Additional parameter included in the body. You can include up to 100
-- additional body parameters per request. An event payload cannot exceed
-- 64 KB.
--
-- /See:/ 'newConnectionBodyParameter' smart constructor.
data ConnectionBodyParameter = ConnectionBodyParameter'
  { -- | The key for the parameter.
    key :: Core.Maybe Core.Text,
    -- | Specified whether the value is secret.
    isValueSecret :: Core.Maybe Core.Bool,
    -- | The value associated with the key.
    value :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConnectionBodyParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'connectionBodyParameter_key' - The key for the parameter.
--
-- 'isValueSecret', 'connectionBodyParameter_isValueSecret' - Specified whether the value is secret.
--
-- 'value', 'connectionBodyParameter_value' - The value associated with the key.
newConnectionBodyParameter ::
  ConnectionBodyParameter
newConnectionBodyParameter =
  ConnectionBodyParameter'
    { key = Core.Nothing,
      isValueSecret = Core.Nothing,
      value = Core.Nothing
    }

-- | The key for the parameter.
connectionBodyParameter_key :: Lens.Lens' ConnectionBodyParameter (Core.Maybe Core.Text)
connectionBodyParameter_key = Lens.lens (\ConnectionBodyParameter' {key} -> key) (\s@ConnectionBodyParameter' {} a -> s {key = a} :: ConnectionBodyParameter)

-- | Specified whether the value is secret.
connectionBodyParameter_isValueSecret :: Lens.Lens' ConnectionBodyParameter (Core.Maybe Core.Bool)
connectionBodyParameter_isValueSecret = Lens.lens (\ConnectionBodyParameter' {isValueSecret} -> isValueSecret) (\s@ConnectionBodyParameter' {} a -> s {isValueSecret = a} :: ConnectionBodyParameter)

-- | The value associated with the key.
connectionBodyParameter_value :: Lens.Lens' ConnectionBodyParameter (Core.Maybe Core.Text)
connectionBodyParameter_value = Lens.lens (\ConnectionBodyParameter' {value} -> value) (\s@ConnectionBodyParameter' {} a -> s {value = a} :: ConnectionBodyParameter)

instance Core.FromJSON ConnectionBodyParameter where
  parseJSON =
    Core.withObject
      "ConnectionBodyParameter"
      ( \x ->
          ConnectionBodyParameter'
            Core.<$> (x Core..:? "Key")
            Core.<*> (x Core..:? "IsValueSecret")
            Core.<*> (x Core..:? "Value")
      )

instance Core.Hashable ConnectionBodyParameter

instance Core.NFData ConnectionBodyParameter

instance Core.ToJSON ConnectionBodyParameter where
  toJSON ConnectionBodyParameter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Key" Core..=) Core.<$> key,
            ("IsValueSecret" Core..=) Core.<$> isValueSecret,
            ("Value" Core..=) Core.<$> value
          ]
      )
