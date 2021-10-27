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
-- Module      : Network.AWS.HoneyCode.Types.ColumnMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.HoneyCode.Types.ColumnMetadata where

import qualified Network.AWS.Core as Core
import Network.AWS.HoneyCode.Types.Format
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Metadata for column in the table.
--
-- /See:/ 'newColumnMetadata' smart constructor.
data ColumnMetadata = ColumnMetadata'
  { -- | The name of the column.
    name :: Core.Sensitive Prelude.Text,
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
        Core._Sensitive Lens.# pName_,
      format = pFormat_
    }

-- | The name of the column.
columnMetadata_name :: Lens.Lens' ColumnMetadata Prelude.Text
columnMetadata_name = Lens.lens (\ColumnMetadata' {name} -> name) (\s@ColumnMetadata' {} a -> s {name = a} :: ColumnMetadata) Prelude.. Core._Sensitive

-- | The format of the column.
columnMetadata_format :: Lens.Lens' ColumnMetadata Format
columnMetadata_format = Lens.lens (\ColumnMetadata' {format} -> format) (\s@ColumnMetadata' {} a -> s {format = a} :: ColumnMetadata)

instance Core.FromJSON ColumnMetadata where
  parseJSON =
    Core.withObject
      "ColumnMetadata"
      ( \x ->
          ColumnMetadata'
            Prelude.<$> (x Core..: "name") Prelude.<*> (x Core..: "format")
      )

instance Prelude.Hashable ColumnMetadata

instance Prelude.NFData ColumnMetadata
