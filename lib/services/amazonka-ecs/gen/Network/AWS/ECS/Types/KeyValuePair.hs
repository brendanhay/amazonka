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
-- Module      : Network.AWS.ECS.Types.KeyValuePair
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.KeyValuePair where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A key-value pair object.
--
-- /See:/ 'newKeyValuePair' smart constructor.
data KeyValuePair = KeyValuePair'
  { -- | The value of the key-value pair. For environment variables, this is the
    -- value of the environment variable.
    value :: Prelude.Maybe Prelude.Text,
    -- | The name of the key-value pair. For environment variables, this is the
    -- name of the environment variable.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KeyValuePair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'keyValuePair_value' - The value of the key-value pair. For environment variables, this is the
-- value of the environment variable.
--
-- 'name', 'keyValuePair_name' - The name of the key-value pair. For environment variables, this is the
-- name of the environment variable.
newKeyValuePair ::
  KeyValuePair
newKeyValuePair =
  KeyValuePair'
    { value = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The value of the key-value pair. For environment variables, this is the
-- value of the environment variable.
keyValuePair_value :: Lens.Lens' KeyValuePair (Prelude.Maybe Prelude.Text)
keyValuePair_value = Lens.lens (\KeyValuePair' {value} -> value) (\s@KeyValuePair' {} a -> s {value = a} :: KeyValuePair)

-- | The name of the key-value pair. For environment variables, this is the
-- name of the environment variable.
keyValuePair_name :: Lens.Lens' KeyValuePair (Prelude.Maybe Prelude.Text)
keyValuePair_name = Lens.lens (\KeyValuePair' {name} -> name) (\s@KeyValuePair' {} a -> s {name = a} :: KeyValuePair)

instance Core.FromJSON KeyValuePair where
  parseJSON =
    Core.withObject
      "KeyValuePair"
      ( \x ->
          KeyValuePair'
            Prelude.<$> (x Core..:? "value") Prelude.<*> (x Core..:? "name")
      )

instance Prelude.Hashable KeyValuePair

instance Prelude.NFData KeyValuePair

instance Core.ToJSON KeyValuePair where
  toJSON KeyValuePair' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("value" Core..=) Prelude.<$> value,
            ("name" Core..=) Prelude.<$> name
          ]
      )
