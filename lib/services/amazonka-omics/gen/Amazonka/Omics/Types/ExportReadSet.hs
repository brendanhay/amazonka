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
-- Module      : Amazonka.Omics.Types.ExportReadSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ExportReadSet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A read set.
--
-- /See:/ 'newExportReadSet' smart constructor.
data ExportReadSet = ExportReadSet'
  { -- | The set\'s ID.
    readSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportReadSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'readSetId', 'exportReadSet_readSetId' - The set\'s ID.
newExportReadSet ::
  -- | 'readSetId'
  Prelude.Text ->
  ExportReadSet
newExportReadSet pReadSetId_ =
  ExportReadSet' {readSetId = pReadSetId_}

-- | The set\'s ID.
exportReadSet_readSetId :: Lens.Lens' ExportReadSet Prelude.Text
exportReadSet_readSetId = Lens.lens (\ExportReadSet' {readSetId} -> readSetId) (\s@ExportReadSet' {} a -> s {readSetId = a} :: ExportReadSet)

instance Prelude.Hashable ExportReadSet where
  hashWithSalt _salt ExportReadSet' {..} =
    _salt `Prelude.hashWithSalt` readSetId

instance Prelude.NFData ExportReadSet where
  rnf ExportReadSet' {..} = Prelude.rnf readSetId

instance Data.ToJSON ExportReadSet where
  toJSON ExportReadSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("readSetId" Data..= readSetId)]
      )
