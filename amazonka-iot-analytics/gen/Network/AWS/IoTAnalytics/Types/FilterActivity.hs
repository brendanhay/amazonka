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
-- Module      : Network.AWS.IoTAnalytics.Types.FilterActivity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.FilterActivity where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An activity that filters a message based on its attributes.
--
-- /See:/ 'newFilterActivity' smart constructor.
data FilterActivity = FilterActivity'
  { -- | The next activity in the pipeline.
    next :: Core.Maybe Core.Text,
    -- | The name of the filter activity.
    name :: Core.Text,
    -- | An expression that looks like a SQL WHERE clause that must return a
    -- Boolean value. Messages that satisfy the condition are passed to the
    -- next activity.
    filter' :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'filter''
  Core.Text ->
  FilterActivity
newFilterActivity pName_ pFilter_ =
  FilterActivity'
    { next = Core.Nothing,
      name = pName_,
      filter' = pFilter_
    }

-- | The next activity in the pipeline.
filterActivity_next :: Lens.Lens' FilterActivity (Core.Maybe Core.Text)
filterActivity_next = Lens.lens (\FilterActivity' {next} -> next) (\s@FilterActivity' {} a -> s {next = a} :: FilterActivity)

-- | The name of the filter activity.
filterActivity_name :: Lens.Lens' FilterActivity Core.Text
filterActivity_name = Lens.lens (\FilterActivity' {name} -> name) (\s@FilterActivity' {} a -> s {name = a} :: FilterActivity)

-- | An expression that looks like a SQL WHERE clause that must return a
-- Boolean value. Messages that satisfy the condition are passed to the
-- next activity.
filterActivity_filter :: Lens.Lens' FilterActivity Core.Text
filterActivity_filter = Lens.lens (\FilterActivity' {filter'} -> filter') (\s@FilterActivity' {} a -> s {filter' = a} :: FilterActivity)

instance Core.FromJSON FilterActivity where
  parseJSON =
    Core.withObject
      "FilterActivity"
      ( \x ->
          FilterActivity'
            Core.<$> (x Core..:? "next")
            Core.<*> (x Core..: "name")
            Core.<*> (x Core..: "filter")
      )

instance Core.Hashable FilterActivity

instance Core.NFData FilterActivity

instance Core.ToJSON FilterActivity where
  toJSON FilterActivity' {..} =
    Core.object
      ( Core.catMaybes
          [ ("next" Core..=) Core.<$> next,
            Core.Just ("name" Core..= name),
            Core.Just ("filter" Core..= filter')
          ]
      )
