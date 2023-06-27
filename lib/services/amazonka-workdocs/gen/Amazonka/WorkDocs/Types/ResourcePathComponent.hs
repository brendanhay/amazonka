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
-- Module      : Amazonka.WorkDocs.Types.ResourcePathComponent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.ResourcePathComponent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the resource path.
--
-- /See:/ 'newResourcePathComponent' smart constructor.
data ResourcePathComponent = ResourcePathComponent'
  { -- | The ID of the resource path.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource path.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourcePathComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'resourcePathComponent_id' - The ID of the resource path.
--
-- 'name', 'resourcePathComponent_name' - The name of the resource path.
newResourcePathComponent ::
  ResourcePathComponent
newResourcePathComponent =
  ResourcePathComponent'
    { id = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The ID of the resource path.
resourcePathComponent_id :: Lens.Lens' ResourcePathComponent (Prelude.Maybe Prelude.Text)
resourcePathComponent_id = Lens.lens (\ResourcePathComponent' {id} -> id) (\s@ResourcePathComponent' {} a -> s {id = a} :: ResourcePathComponent)

-- | The name of the resource path.
resourcePathComponent_name :: Lens.Lens' ResourcePathComponent (Prelude.Maybe Prelude.Text)
resourcePathComponent_name = Lens.lens (\ResourcePathComponent' {name} -> name) (\s@ResourcePathComponent' {} a -> s {name = a} :: ResourcePathComponent) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON ResourcePathComponent where
  parseJSON =
    Data.withObject
      "ResourcePathComponent"
      ( \x ->
          ResourcePathComponent'
            Prelude.<$> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable ResourcePathComponent where
  hashWithSalt _salt ResourcePathComponent' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name

instance Prelude.NFData ResourcePathComponent where
  rnf ResourcePathComponent' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf name
