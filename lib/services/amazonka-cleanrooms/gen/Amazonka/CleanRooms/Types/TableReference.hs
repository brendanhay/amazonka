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
-- Module      : Amazonka.CleanRooms.Types.TableReference
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.TableReference where

import Amazonka.CleanRooms.Types.GlueTableReference
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A pointer to the dataset that underlies this table. Currently, this can
-- only be an AWS Glue table.
--
-- /See:/ 'newTableReference' smart constructor.
data TableReference = TableReference'
  { -- | If present, a reference to the AWS Glue table referred to by this table
    -- reference.
    glue :: Prelude.Maybe GlueTableReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'glue', 'tableReference_glue' - If present, a reference to the AWS Glue table referred to by this table
-- reference.
newTableReference ::
  TableReference
newTableReference =
  TableReference' {glue = Prelude.Nothing}

-- | If present, a reference to the AWS Glue table referred to by this table
-- reference.
tableReference_glue :: Lens.Lens' TableReference (Prelude.Maybe GlueTableReference)
tableReference_glue = Lens.lens (\TableReference' {glue} -> glue) (\s@TableReference' {} a -> s {glue = a} :: TableReference)

instance Data.FromJSON TableReference where
  parseJSON =
    Data.withObject
      "TableReference"
      ( \x ->
          TableReference' Prelude.<$> (x Data..:? "glue")
      )

instance Prelude.Hashable TableReference where
  hashWithSalt _salt TableReference' {..} =
    _salt `Prelude.hashWithSalt` glue

instance Prelude.NFData TableReference where
  rnf TableReference' {..} = Prelude.rnf glue

instance Data.ToJSON TableReference where
  toJSON TableReference' {..} =
    Data.object
      ( Prelude.catMaybes
          [("glue" Data..=) Prelude.<$> glue]
      )
