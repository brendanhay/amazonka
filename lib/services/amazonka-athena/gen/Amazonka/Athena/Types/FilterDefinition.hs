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
-- Module      : Amazonka.Athena.Types.FilterDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.FilterDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A string for searching notebook names.
--
-- /See:/ 'newFilterDefinition' smart constructor.
data FilterDefinition = FilterDefinition'
  { -- | The name of the notebook to search for.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'filterDefinition_name' - The name of the notebook to search for.
newFilterDefinition ::
  FilterDefinition
newFilterDefinition =
  FilterDefinition' {name = Prelude.Nothing}

-- | The name of the notebook to search for.
filterDefinition_name :: Lens.Lens' FilterDefinition (Prelude.Maybe Prelude.Text)
filterDefinition_name = Lens.lens (\FilterDefinition' {name} -> name) (\s@FilterDefinition' {} a -> s {name = a} :: FilterDefinition)

instance Prelude.Hashable FilterDefinition where
  hashWithSalt _salt FilterDefinition' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData FilterDefinition where
  rnf FilterDefinition' {..} = Prelude.rnf name

instance Data.ToJSON FilterDefinition where
  toJSON FilterDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Name" Data..=) Prelude.<$> name]
      )
