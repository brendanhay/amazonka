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
-- Module      : Amazonka.Pipes.Types.BatchEnvironmentVariable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.BatchEnvironmentVariable where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The environment variables to send to the container. You can add new
-- environment variables, which are added to the container at launch, or
-- you can override the existing environment variables from the Docker
-- image or the task definition.
--
-- Environment variables cannot start with \"@Batch@\". This naming
-- convention is reserved for variables that Batch sets.
--
-- /See:/ 'newBatchEnvironmentVariable' smart constructor.
data BatchEnvironmentVariable = BatchEnvironmentVariable'
  { -- | The name of the key-value pair. For environment variables, this is the
    -- name of the environment variable.
    name :: Prelude.Maybe Prelude.Text,
    -- | The value of the key-value pair. For environment variables, this is the
    -- value of the environment variable.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchEnvironmentVariable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'batchEnvironmentVariable_name' - The name of the key-value pair. For environment variables, this is the
-- name of the environment variable.
--
-- 'value', 'batchEnvironmentVariable_value' - The value of the key-value pair. For environment variables, this is the
-- value of the environment variable.
newBatchEnvironmentVariable ::
  BatchEnvironmentVariable
newBatchEnvironmentVariable =
  BatchEnvironmentVariable'
    { name = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the key-value pair. For environment variables, this is the
-- name of the environment variable.
batchEnvironmentVariable_name :: Lens.Lens' BatchEnvironmentVariable (Prelude.Maybe Prelude.Text)
batchEnvironmentVariable_name = Lens.lens (\BatchEnvironmentVariable' {name} -> name) (\s@BatchEnvironmentVariable' {} a -> s {name = a} :: BatchEnvironmentVariable)

-- | The value of the key-value pair. For environment variables, this is the
-- value of the environment variable.
batchEnvironmentVariable_value :: Lens.Lens' BatchEnvironmentVariable (Prelude.Maybe Prelude.Text)
batchEnvironmentVariable_value = Lens.lens (\BatchEnvironmentVariable' {value} -> value) (\s@BatchEnvironmentVariable' {} a -> s {value = a} :: BatchEnvironmentVariable)

instance Data.FromJSON BatchEnvironmentVariable where
  parseJSON =
    Data.withObject
      "BatchEnvironmentVariable"
      ( \x ->
          BatchEnvironmentVariable'
            Prelude.<$> (x Data..:? "Name") Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable BatchEnvironmentVariable where
  hashWithSalt _salt BatchEnvironmentVariable' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData BatchEnvironmentVariable where
  rnf BatchEnvironmentVariable' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance Data.ToJSON BatchEnvironmentVariable where
  toJSON BatchEnvironmentVariable' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("Value" Data..=) Prelude.<$> value
          ]
      )
