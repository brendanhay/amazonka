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
-- Module      : Amazonka.TNB.Types.ToscaOverride
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types.ToscaOverride where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Overrides of the TOSCA node.
--
-- /See:/ 'newToscaOverride' smart constructor.
data ToscaOverride = ToscaOverride'
  { -- | Default value for the override.
    defaultValue :: Prelude.Maybe Prelude.Text,
    -- | Name of the TOSCA override.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ToscaOverride' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultValue', 'toscaOverride_defaultValue' - Default value for the override.
--
-- 'name', 'toscaOverride_name' - Name of the TOSCA override.
newToscaOverride ::
  ToscaOverride
newToscaOverride =
  ToscaOverride'
    { defaultValue = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | Default value for the override.
toscaOverride_defaultValue :: Lens.Lens' ToscaOverride (Prelude.Maybe Prelude.Text)
toscaOverride_defaultValue = Lens.lens (\ToscaOverride' {defaultValue} -> defaultValue) (\s@ToscaOverride' {} a -> s {defaultValue = a} :: ToscaOverride)

-- | Name of the TOSCA override.
toscaOverride_name :: Lens.Lens' ToscaOverride (Prelude.Maybe Prelude.Text)
toscaOverride_name = Lens.lens (\ToscaOverride' {name} -> name) (\s@ToscaOverride' {} a -> s {name = a} :: ToscaOverride)

instance Data.FromJSON ToscaOverride where
  parseJSON =
    Data.withObject
      "ToscaOverride"
      ( \x ->
          ToscaOverride'
            Prelude.<$> (x Data..:? "defaultValue")
            Prelude.<*> (x Data..:? "name")
      )

instance Prelude.Hashable ToscaOverride where
  hashWithSalt _salt ToscaOverride' {..} =
    _salt
      `Prelude.hashWithSalt` defaultValue
      `Prelude.hashWithSalt` name

instance Prelude.NFData ToscaOverride where
  rnf ToscaOverride' {..} =
    Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf name
