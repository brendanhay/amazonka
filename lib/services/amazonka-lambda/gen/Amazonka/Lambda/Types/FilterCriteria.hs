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
-- Module      : Amazonka.Lambda.Types.FilterCriteria
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.FilterCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types.Filter
import qualified Amazonka.Prelude as Prelude

-- | An object that contains the filters for an event source.
--
-- /See:/ 'newFilterCriteria' smart constructor.
data FilterCriteria = FilterCriteria'
  { -- | A list of filters.
    filters :: Prelude.Maybe [Filter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'filterCriteria_filters' - A list of filters.
newFilterCriteria ::
  FilterCriteria
newFilterCriteria =
  FilterCriteria' {filters = Prelude.Nothing}

-- | A list of filters.
filterCriteria_filters :: Lens.Lens' FilterCriteria (Prelude.Maybe [Filter])
filterCriteria_filters = Lens.lens (\FilterCriteria' {filters} -> filters) (\s@FilterCriteria' {} a -> s {filters = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON FilterCriteria where
  parseJSON =
    Data.withObject
      "FilterCriteria"
      ( \x ->
          FilterCriteria'
            Prelude.<$> (x Data..:? "Filters" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable FilterCriteria where
  hashWithSalt _salt FilterCriteria' {..} =
    _salt `Prelude.hashWithSalt` filters

instance Prelude.NFData FilterCriteria where
  rnf FilterCriteria' {..} = Prelude.rnf filters

instance Data.ToJSON FilterCriteria where
  toJSON FilterCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Filters" Data..=) Prelude.<$> filters]
      )
