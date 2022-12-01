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
-- Module      : Amazonka.Discovery.Types.ExportFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Discovery.Types.ExportFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Used to select which agent\'s data is to be exported. A single agent ID
-- may be selected for export using the
-- <http://docs.aws.amazon.com/application-discovery/latest/APIReference/API_StartExportTask.html StartExportTask>
-- action.
--
-- /See:/ 'newExportFilter' smart constructor.
data ExportFilter = ExportFilter'
  { -- | A single @ExportFilter@ name. Supported filters: @agentIds@.
    name :: Prelude.Text,
    -- | A single agent ID for a Discovery Agent. An agent ID can be found using
    -- the
    -- <http://docs.aws.amazon.com/application-discovery/latest/APIReference/API_DescribeAgents.html DescribeAgents>
    -- action. Typically an ADS agent ID is in the form @o-0123456789abcdef0@.
    values :: [Prelude.Text],
    -- | Supported condition: @EQUALS@
    condition :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'exportFilter_name' - A single @ExportFilter@ name. Supported filters: @agentIds@.
--
-- 'values', 'exportFilter_values' - A single agent ID for a Discovery Agent. An agent ID can be found using
-- the
-- <http://docs.aws.amazon.com/application-discovery/latest/APIReference/API_DescribeAgents.html DescribeAgents>
-- action. Typically an ADS agent ID is in the form @o-0123456789abcdef0@.
--
-- 'condition', 'exportFilter_condition' - Supported condition: @EQUALS@
newExportFilter ::
  -- | 'name'
  Prelude.Text ->
  -- | 'condition'
  Prelude.Text ->
  ExportFilter
newExportFilter pName_ pCondition_ =
  ExportFilter'
    { name = pName_,
      values = Prelude.mempty,
      condition = pCondition_
    }

-- | A single @ExportFilter@ name. Supported filters: @agentIds@.
exportFilter_name :: Lens.Lens' ExportFilter Prelude.Text
exportFilter_name = Lens.lens (\ExportFilter' {name} -> name) (\s@ExportFilter' {} a -> s {name = a} :: ExportFilter)

-- | A single agent ID for a Discovery Agent. An agent ID can be found using
-- the
-- <http://docs.aws.amazon.com/application-discovery/latest/APIReference/API_DescribeAgents.html DescribeAgents>
-- action. Typically an ADS agent ID is in the form @o-0123456789abcdef0@.
exportFilter_values :: Lens.Lens' ExportFilter [Prelude.Text]
exportFilter_values = Lens.lens (\ExportFilter' {values} -> values) (\s@ExportFilter' {} a -> s {values = a} :: ExportFilter) Prelude.. Lens.coerced

-- | Supported condition: @EQUALS@
exportFilter_condition :: Lens.Lens' ExportFilter Prelude.Text
exportFilter_condition = Lens.lens (\ExportFilter' {condition} -> condition) (\s@ExportFilter' {} a -> s {condition = a} :: ExportFilter)

instance Prelude.Hashable ExportFilter where
  hashWithSalt _salt ExportFilter' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values
      `Prelude.hashWithSalt` condition

instance Prelude.NFData ExportFilter where
  rnf ExportFilter' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf values
      `Prelude.seq` Prelude.rnf condition

instance Core.ToJSON ExportFilter where
  toJSON ExportFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Core..= name),
            Prelude.Just ("values" Core..= values),
            Prelude.Just ("condition" Core..= condition)
          ]
      )
