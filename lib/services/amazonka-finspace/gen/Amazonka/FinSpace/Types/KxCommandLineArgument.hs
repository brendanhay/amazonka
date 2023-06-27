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
-- Module      : Amazonka.FinSpace.Types.KxCommandLineArgument
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpace.Types.KxCommandLineArgument where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines the key-value pairs to make them available inside the cluster.
--
-- /See:/ 'newKxCommandLineArgument' smart constructor.
data KxCommandLineArgument = KxCommandLineArgument'
  { -- | The name of the key.
    key :: Prelude.Maybe Prelude.Text,
    -- | The value of the key.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KxCommandLineArgument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'kxCommandLineArgument_key' - The name of the key.
--
-- 'value', 'kxCommandLineArgument_value' - The value of the key.
newKxCommandLineArgument ::
  KxCommandLineArgument
newKxCommandLineArgument =
  KxCommandLineArgument'
    { key = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the key.
kxCommandLineArgument_key :: Lens.Lens' KxCommandLineArgument (Prelude.Maybe Prelude.Text)
kxCommandLineArgument_key = Lens.lens (\KxCommandLineArgument' {key} -> key) (\s@KxCommandLineArgument' {} a -> s {key = a} :: KxCommandLineArgument)

-- | The value of the key.
kxCommandLineArgument_value :: Lens.Lens' KxCommandLineArgument (Prelude.Maybe Prelude.Text)
kxCommandLineArgument_value = Lens.lens (\KxCommandLineArgument' {value} -> value) (\s@KxCommandLineArgument' {} a -> s {value = a} :: KxCommandLineArgument)

instance Data.FromJSON KxCommandLineArgument where
  parseJSON =
    Data.withObject
      "KxCommandLineArgument"
      ( \x ->
          KxCommandLineArgument'
            Prelude.<$> (x Data..:? "key")
            Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable KxCommandLineArgument where
  hashWithSalt _salt KxCommandLineArgument' {..} =
    _salt
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData KxCommandLineArgument where
  rnf KxCommandLineArgument' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value

instance Data.ToJSON KxCommandLineArgument where
  toJSON KxCommandLineArgument' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("key" Data..=) Prelude.<$> key,
            ("value" Data..=) Prelude.<$> value
          ]
      )
