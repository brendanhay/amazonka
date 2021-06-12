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
-- Module      : Network.AWS.Glue.Types.Column
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Column where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A column in a @Table@.
--
-- /See:/ 'newColumn' smart constructor.
data Column = Column'
  { -- | A free-form text comment.
    comment :: Core.Maybe Core.Text,
    -- | The data type of the @Column@.
    type' :: Core.Maybe Core.Text,
    -- | These key-value pairs define properties associated with the column.
    parameters :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The name of the @Column@.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Column' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'column_comment' - A free-form text comment.
--
-- 'type'', 'column_type' - The data type of the @Column@.
--
-- 'parameters', 'column_parameters' - These key-value pairs define properties associated with the column.
--
-- 'name', 'column_name' - The name of the @Column@.
newColumn ::
  -- | 'name'
  Core.Text ->
  Column
newColumn pName_ =
  Column'
    { comment = Core.Nothing,
      type' = Core.Nothing,
      parameters = Core.Nothing,
      name = pName_
    }

-- | A free-form text comment.
column_comment :: Lens.Lens' Column (Core.Maybe Core.Text)
column_comment = Lens.lens (\Column' {comment} -> comment) (\s@Column' {} a -> s {comment = a} :: Column)

-- | The data type of the @Column@.
column_type :: Lens.Lens' Column (Core.Maybe Core.Text)
column_type = Lens.lens (\Column' {type'} -> type') (\s@Column' {} a -> s {type' = a} :: Column)

-- | These key-value pairs define properties associated with the column.
column_parameters :: Lens.Lens' Column (Core.Maybe (Core.HashMap Core.Text Core.Text))
column_parameters = Lens.lens (\Column' {parameters} -> parameters) (\s@Column' {} a -> s {parameters = a} :: Column) Core.. Lens.mapping Lens._Coerce

-- | The name of the @Column@.
column_name :: Lens.Lens' Column Core.Text
column_name = Lens.lens (\Column' {name} -> name) (\s@Column' {} a -> s {name = a} :: Column)

instance Core.FromJSON Column where
  parseJSON =
    Core.withObject
      "Column"
      ( \x ->
          Column'
            Core.<$> (x Core..:? "Comment")
            Core.<*> (x Core..:? "Type")
            Core.<*> (x Core..:? "Parameters" Core..!= Core.mempty)
            Core.<*> (x Core..: "Name")
      )

instance Core.Hashable Column

instance Core.NFData Column

instance Core.ToJSON Column where
  toJSON Column' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Comment" Core..=) Core.<$> comment,
            ("Type" Core..=) Core.<$> type',
            ("Parameters" Core..=) Core.<$> parameters,
            Core.Just ("Name" Core..= name)
          ]
      )
