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
-- Module      : Amazonka.HoneyCode.Types.ColumnMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HoneyCode.Types.ColumnMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.HoneyCode.Types.Format
import qualified Amazonka.Prelude as Prelude

-- | Metadata for column in the table.
--
-- /See:/ 'newColumnMetadata' smart constructor.
data ColumnMetadata = ColumnMetadata'
  { -- | The name of the column.
    name :: Data.Sensitive Prelude.Text,
    -- | The format of the column.
    format :: Format
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ColumnMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'columnMetadata_name' - The name of the column.
--
-- 'format', 'columnMetadata_format' - The format of the column.
newColumnMetadata ::
  -- | 'name'
  Prelude.Text ->
  -- | 'format'
  Format ->
  ColumnMetadata
newColumnMetadata pName_ pFormat_ =
  ColumnMetadata'
    { name =
        Data._Sensitive Lens.# pName_,
      format = pFormat_
    }

-- | The name of the column.
columnMetadata_name :: Lens.Lens' ColumnMetadata Prelude.Text
columnMetadata_name = Lens.lens (\ColumnMetadata' {name} -> name) (\s@ColumnMetadata' {} a -> s {name = a} :: ColumnMetadata) Prelude.. Data._Sensitive

-- | The format of the column.
columnMetadata_format :: Lens.Lens' ColumnMetadata Format
columnMetadata_format = Lens.lens (\ColumnMetadata' {format} -> format) (\s@ColumnMetadata' {} a -> s {format = a} :: ColumnMetadata)

instance Data.FromJSON ColumnMetadata where
  parseJSON =
    Data.withObject
      "ColumnMetadata"
      ( \x ->
          ColumnMetadata'
            Prelude.<$> (x Data..: "name") Prelude.<*> (x Data..: "format")
      )

instance Prelude.Hashable ColumnMetadata where
  hashWithSalt _salt ColumnMetadata' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` format

instance Prelude.NFData ColumnMetadata where
  rnf ColumnMetadata' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf format
