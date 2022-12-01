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
-- Module      : Amazonka.IoTAnalytics.Types.FilterActivity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.FilterActivity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An activity that filters a message based on its attributes.
--
-- /See:/ 'newFilterActivity' smart constructor.
data FilterActivity = FilterActivity'
  { -- | The next activity in the pipeline.
    next :: Prelude.Maybe Prelude.Text,
    -- | The name of the filter activity.
    name :: Prelude.Text,
    -- | An expression that looks like a SQL WHERE clause that must return a
    -- Boolean value. Messages that satisfy the condition are passed to the
    -- next activity.
    filter' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterActivity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'next', 'filterActivity_next' - The next activity in the pipeline.
--
-- 'name', 'filterActivity_name' - The name of the filter activity.
--
-- 'filter'', 'filterActivity_filter' - An expression that looks like a SQL WHERE clause that must return a
-- Boolean value. Messages that satisfy the condition are passed to the
-- next activity.
newFilterActivity ::
  -- | 'name'
  Prelude.Text ->
  -- | 'filter''
  Prelude.Text ->
  FilterActivity
newFilterActivity pName_ pFilter_ =
  FilterActivity'
    { next = Prelude.Nothing,
      name = pName_,
      filter' = pFilter_
    }

-- | The next activity in the pipeline.
filterActivity_next :: Lens.Lens' FilterActivity (Prelude.Maybe Prelude.Text)
filterActivity_next = Lens.lens (\FilterActivity' {next} -> next) (\s@FilterActivity' {} a -> s {next = a} :: FilterActivity)

-- | The name of the filter activity.
filterActivity_name :: Lens.Lens' FilterActivity Prelude.Text
filterActivity_name = Lens.lens (\FilterActivity' {name} -> name) (\s@FilterActivity' {} a -> s {name = a} :: FilterActivity)

-- | An expression that looks like a SQL WHERE clause that must return a
-- Boolean value. Messages that satisfy the condition are passed to the
-- next activity.
filterActivity_filter :: Lens.Lens' FilterActivity Prelude.Text
filterActivity_filter = Lens.lens (\FilterActivity' {filter'} -> filter') (\s@FilterActivity' {} a -> s {filter' = a} :: FilterActivity)

instance Core.FromJSON FilterActivity where
  parseJSON =
    Core.withObject
      "FilterActivity"
      ( \x ->
          FilterActivity'
            Prelude.<$> (x Core..:? "next")
            Prelude.<*> (x Core..: "name")
            Prelude.<*> (x Core..: "filter")
      )

instance Prelude.Hashable FilterActivity where
  hashWithSalt _salt FilterActivity' {..} =
    _salt `Prelude.hashWithSalt` next
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` filter'

instance Prelude.NFData FilterActivity where
  rnf FilterActivity' {..} =
    Prelude.rnf next
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf filter'

instance Core.ToJSON FilterActivity where
  toJSON FilterActivity' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("next" Core..=) Prelude.<$> next,
            Prelude.Just ("name" Core..= name),
            Prelude.Just ("filter" Core..= filter')
          ]
      )
