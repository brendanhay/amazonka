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
-- Module      : Amazonka.Batch.Types.EksContainerEnvironmentVariable
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.EksContainerEnvironmentVariable where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An environment variable.
--
-- /See:/ 'newEksContainerEnvironmentVariable' smart constructor.
data EksContainerEnvironmentVariable = EksContainerEnvironmentVariable'
  { -- | The value of the environment variable.
    value :: Prelude.Maybe Prelude.Text,
    -- | The name of the environment variable.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EksContainerEnvironmentVariable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'eksContainerEnvironmentVariable_value' - The value of the environment variable.
--
-- 'name', 'eksContainerEnvironmentVariable_name' - The name of the environment variable.
newEksContainerEnvironmentVariable ::
  -- | 'name'
  Prelude.Text ->
  EksContainerEnvironmentVariable
newEksContainerEnvironmentVariable pName_ =
  EksContainerEnvironmentVariable'
    { value =
        Prelude.Nothing,
      name = pName_
    }

-- | The value of the environment variable.
eksContainerEnvironmentVariable_value :: Lens.Lens' EksContainerEnvironmentVariable (Prelude.Maybe Prelude.Text)
eksContainerEnvironmentVariable_value = Lens.lens (\EksContainerEnvironmentVariable' {value} -> value) (\s@EksContainerEnvironmentVariable' {} a -> s {value = a} :: EksContainerEnvironmentVariable)

-- | The name of the environment variable.
eksContainerEnvironmentVariable_name :: Lens.Lens' EksContainerEnvironmentVariable Prelude.Text
eksContainerEnvironmentVariable_name = Lens.lens (\EksContainerEnvironmentVariable' {name} -> name) (\s@EksContainerEnvironmentVariable' {} a -> s {name = a} :: EksContainerEnvironmentVariable)

instance
  Data.FromJSON
    EksContainerEnvironmentVariable
  where
  parseJSON =
    Data.withObject
      "EksContainerEnvironmentVariable"
      ( \x ->
          EksContainerEnvironmentVariable'
            Prelude.<$> (x Data..:? "value") Prelude.<*> (x Data..: "name")
      )

instance
  Prelude.Hashable
    EksContainerEnvironmentVariable
  where
  hashWithSalt
    _salt
    EksContainerEnvironmentVariable' {..} =
      _salt `Prelude.hashWithSalt` value
        `Prelude.hashWithSalt` name

instance
  Prelude.NFData
    EksContainerEnvironmentVariable
  where
  rnf EksContainerEnvironmentVariable' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf name

instance Data.ToJSON EksContainerEnvironmentVariable where
  toJSON EksContainerEnvironmentVariable' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("value" Data..=) Prelude.<$> value,
            Prelude.Just ("name" Data..= name)
          ]
      )
