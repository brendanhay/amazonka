{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.OpsWorksCM.Types.EngineAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorksCM.Types.EngineAttribute where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A name and value pair that is specific to the engine of the server.
--
-- /See:/ 'newEngineAttribute' smart constructor.
data EngineAttribute = EngineAttribute'
  { -- | The name of the engine attribute.
    name :: Prelude.Maybe Prelude.Text,
    -- | The value of the engine attribute.
    value :: Prelude.Maybe (Prelude.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EngineAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'engineAttribute_name' - The name of the engine attribute.
--
-- 'value', 'engineAttribute_value' - The value of the engine attribute.
newEngineAttribute ::
  EngineAttribute
newEngineAttribute =
  EngineAttribute'
    { name = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the engine attribute.
engineAttribute_name :: Lens.Lens' EngineAttribute (Prelude.Maybe Prelude.Text)
engineAttribute_name = Lens.lens (\EngineAttribute' {name} -> name) (\s@EngineAttribute' {} a -> s {name = a} :: EngineAttribute)

-- | The value of the engine attribute.
engineAttribute_value :: Lens.Lens' EngineAttribute (Prelude.Maybe Prelude.Text)
engineAttribute_value = Lens.lens (\EngineAttribute' {value} -> value) (\s@EngineAttribute' {} a -> s {value = a} :: EngineAttribute) Prelude.. Lens.mapping Prelude._Sensitive

instance Prelude.FromJSON EngineAttribute where
  parseJSON =
    Prelude.withObject
      "EngineAttribute"
      ( \x ->
          EngineAttribute'
            Prelude.<$> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Value")
      )

instance Prelude.Hashable EngineAttribute

instance Prelude.NFData EngineAttribute

instance Prelude.ToJSON EngineAttribute where
  toJSON EngineAttribute' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Name" Prelude..=) Prelude.<$> name,
            ("Value" Prelude..=) Prelude.<$> value
          ]
      )
