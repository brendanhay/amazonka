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
-- Module      : Amazonka.Pipes.Types.EcsEnvironmentVariable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.EcsEnvironmentVariable where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The environment variables to send to the container. You can add new
-- environment variables, which are added to the container at launch, or
-- you can override the existing environment variables from the Docker
-- image or the task definition. You must also specify a container name.
--
-- /See:/ 'newEcsEnvironmentVariable' smart constructor.
data EcsEnvironmentVariable = EcsEnvironmentVariable'
  { -- | The name of the key-value pair. For environment variables, this is the
    -- name of the environment variable.
    name :: Prelude.Maybe Prelude.Text,
    -- | The value of the key-value pair. For environment variables, this is the
    -- value of the environment variable.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EcsEnvironmentVariable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'ecsEnvironmentVariable_name' - The name of the key-value pair. For environment variables, this is the
-- name of the environment variable.
--
-- 'value', 'ecsEnvironmentVariable_value' - The value of the key-value pair. For environment variables, this is the
-- value of the environment variable.
newEcsEnvironmentVariable ::
  EcsEnvironmentVariable
newEcsEnvironmentVariable =
  EcsEnvironmentVariable'
    { name = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the key-value pair. For environment variables, this is the
-- name of the environment variable.
ecsEnvironmentVariable_name :: Lens.Lens' EcsEnvironmentVariable (Prelude.Maybe Prelude.Text)
ecsEnvironmentVariable_name = Lens.lens (\EcsEnvironmentVariable' {name} -> name) (\s@EcsEnvironmentVariable' {} a -> s {name = a} :: EcsEnvironmentVariable)

-- | The value of the key-value pair. For environment variables, this is the
-- value of the environment variable.
ecsEnvironmentVariable_value :: Lens.Lens' EcsEnvironmentVariable (Prelude.Maybe Prelude.Text)
ecsEnvironmentVariable_value = Lens.lens (\EcsEnvironmentVariable' {value} -> value) (\s@EcsEnvironmentVariable' {} a -> s {value = a} :: EcsEnvironmentVariable)

instance Data.FromJSON EcsEnvironmentVariable where
  parseJSON =
    Data.withObject
      "EcsEnvironmentVariable"
      ( \x ->
          EcsEnvironmentVariable'
            Prelude.<$> (x Data..:? "name") Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable EcsEnvironmentVariable where
  hashWithSalt _salt EcsEnvironmentVariable' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData EcsEnvironmentVariable where
  rnf EcsEnvironmentVariable' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance Data.ToJSON EcsEnvironmentVariable where
  toJSON EcsEnvironmentVariable' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("value" Data..=) Prelude.<$> value
          ]
      )
