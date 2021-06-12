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
-- Module      : Network.AWS.CloudWatchEvents.Types.ConnectionHeaderParameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.ConnectionHeaderParameter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Additional parameter included in the header. You can include up to 100
-- additional header parameters per request. An event payload cannot exceed
-- 64 KB.
--
-- /See:/ 'newConnectionHeaderParameter' smart constructor.
data ConnectionHeaderParameter = ConnectionHeaderParameter'
  { -- | The key for the parameter.
    key :: Core.Maybe Core.Text,
    -- | Specified whether the value is a secret.
    isValueSecret :: Core.Maybe Core.Bool,
    -- | The value associated with the key.
    value :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConnectionHeaderParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'connectionHeaderParameter_key' - The key for the parameter.
--
-- 'isValueSecret', 'connectionHeaderParameter_isValueSecret' - Specified whether the value is a secret.
--
-- 'value', 'connectionHeaderParameter_value' - The value associated with the key.
newConnectionHeaderParameter ::
  ConnectionHeaderParameter
newConnectionHeaderParameter =
  ConnectionHeaderParameter'
    { key = Core.Nothing,
      isValueSecret = Core.Nothing,
      value = Core.Nothing
    }

-- | The key for the parameter.
connectionHeaderParameter_key :: Lens.Lens' ConnectionHeaderParameter (Core.Maybe Core.Text)
connectionHeaderParameter_key = Lens.lens (\ConnectionHeaderParameter' {key} -> key) (\s@ConnectionHeaderParameter' {} a -> s {key = a} :: ConnectionHeaderParameter)

-- | Specified whether the value is a secret.
connectionHeaderParameter_isValueSecret :: Lens.Lens' ConnectionHeaderParameter (Core.Maybe Core.Bool)
connectionHeaderParameter_isValueSecret = Lens.lens (\ConnectionHeaderParameter' {isValueSecret} -> isValueSecret) (\s@ConnectionHeaderParameter' {} a -> s {isValueSecret = a} :: ConnectionHeaderParameter)

-- | The value associated with the key.
connectionHeaderParameter_value :: Lens.Lens' ConnectionHeaderParameter (Core.Maybe Core.Text)
connectionHeaderParameter_value = Lens.lens (\ConnectionHeaderParameter' {value} -> value) (\s@ConnectionHeaderParameter' {} a -> s {value = a} :: ConnectionHeaderParameter)

instance Core.FromJSON ConnectionHeaderParameter where
  parseJSON =
    Core.withObject
      "ConnectionHeaderParameter"
      ( \x ->
          ConnectionHeaderParameter'
            Core.<$> (x Core..:? "Key")
            Core.<*> (x Core..:? "IsValueSecret")
            Core.<*> (x Core..:? "Value")
      )

instance Core.Hashable ConnectionHeaderParameter

instance Core.NFData ConnectionHeaderParameter

instance Core.ToJSON ConnectionHeaderParameter where
  toJSON ConnectionHeaderParameter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Key" Core..=) Core.<$> key,
            ("IsValueSecret" Core..=) Core.<$> isValueSecret,
            ("Value" Core..=) Core.<$> value
          ]
      )
