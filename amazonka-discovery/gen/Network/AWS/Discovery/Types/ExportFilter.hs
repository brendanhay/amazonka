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
-- Module      : Network.AWS.Discovery.Types.ExportFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.ExportFilter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Used to select which agent\'s data is to be exported. A single agent ID
-- may be selected for export using the
-- <http://docs.aws.amazon.com/application-discovery/latest/APIReference/API_StartExportTask.html StartExportTask>
-- action.
--
-- /See:/ 'newExportFilter' smart constructor.
data ExportFilter = ExportFilter'
  { -- | A single @ExportFilter@ name. Supported filters: @agentId@.
    name :: Prelude.Text,
    -- | A single @agentId@ for a Discovery Agent. An @agentId@ can be found
    -- using the
    -- <http://docs.aws.amazon.com/application-discovery/latest/APIReference/API_DescribeExportTasks.html DescribeAgents>
    -- action. Typically an ADS @agentId@ is in the form @o-0123456789abcdef0@.
    values :: [Prelude.Text],
    -- | Supported condition: @EQUALS@
    condition :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ExportFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'exportFilter_name' - A single @ExportFilter@ name. Supported filters: @agentId@.
--
-- 'values', 'exportFilter_values' - A single @agentId@ for a Discovery Agent. An @agentId@ can be found
-- using the
-- <http://docs.aws.amazon.com/application-discovery/latest/APIReference/API_DescribeExportTasks.html DescribeAgents>
-- action. Typically an ADS @agentId@ is in the form @o-0123456789abcdef0@.
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

-- | A single @ExportFilter@ name. Supported filters: @agentId@.
exportFilter_name :: Lens.Lens' ExportFilter Prelude.Text
exportFilter_name = Lens.lens (\ExportFilter' {name} -> name) (\s@ExportFilter' {} a -> s {name = a} :: ExportFilter)

-- | A single @agentId@ for a Discovery Agent. An @agentId@ can be found
-- using the
-- <http://docs.aws.amazon.com/application-discovery/latest/APIReference/API_DescribeExportTasks.html DescribeAgents>
-- action. Typically an ADS @agentId@ is in the form @o-0123456789abcdef0@.
exportFilter_values :: Lens.Lens' ExportFilter [Prelude.Text]
exportFilter_values = Lens.lens (\ExportFilter' {values} -> values) (\s@ExportFilter' {} a -> s {values = a} :: ExportFilter) Prelude.. Prelude._Coerce

-- | Supported condition: @EQUALS@
exportFilter_condition :: Lens.Lens' ExportFilter Prelude.Text
exportFilter_condition = Lens.lens (\ExportFilter' {condition} -> condition) (\s@ExportFilter' {} a -> s {condition = a} :: ExportFilter)

instance Prelude.Hashable ExportFilter

instance Prelude.NFData ExportFilter

instance Prelude.ToJSON ExportFilter where
  toJSON ExportFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Prelude..= name),
            Prelude.Just ("values" Prelude..= values),
            Prelude.Just ("condition" Prelude..= condition)
          ]
      )
