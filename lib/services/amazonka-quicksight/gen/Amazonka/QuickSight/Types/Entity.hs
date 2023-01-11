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
-- Module      : Amazonka.QuickSight.Types.Entity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.Entity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object, structure, or sub-structure of an analysis, template, or
-- dashboard.
--
-- /See:/ 'newEntity' smart constructor.
data Entity = Entity'
  { -- | The hierarchical path of the entity within the analysis, template, or
    -- dashboard definition tree.
    path :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Entity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'path', 'entity_path' - The hierarchical path of the entity within the analysis, template, or
-- dashboard definition tree.
newEntity ::
  Entity
newEntity = Entity' {path = Prelude.Nothing}

-- | The hierarchical path of the entity within the analysis, template, or
-- dashboard definition tree.
entity_path :: Lens.Lens' Entity (Prelude.Maybe Prelude.Text)
entity_path = Lens.lens (\Entity' {path} -> path) (\s@Entity' {} a -> s {path = a} :: Entity)

instance Data.FromJSON Entity where
  parseJSON =
    Data.withObject
      "Entity"
      (\x -> Entity' Prelude.<$> (x Data..:? "Path"))

instance Prelude.Hashable Entity where
  hashWithSalt _salt Entity' {..} =
    _salt `Prelude.hashWithSalt` path

instance Prelude.NFData Entity where
  rnf Entity' {..} = Prelude.rnf path
