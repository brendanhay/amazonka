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
-- Module      : Amazonka.SSM.Types.DocumentFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.DocumentFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.DocumentFilterKey

-- | This data type is deprecated. Instead, use DocumentKeyValuesFilter.
--
-- /See:/ 'newDocumentFilter' smart constructor.
data DocumentFilter = DocumentFilter'
  { -- | The name of the filter.
    key :: DocumentFilterKey,
    -- | The value of the filter.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'documentFilter_key' - The name of the filter.
--
-- 'value', 'documentFilter_value' - The value of the filter.
newDocumentFilter ::
  -- | 'key'
  DocumentFilterKey ->
  -- | 'value'
  Prelude.Text ->
  DocumentFilter
newDocumentFilter pKey_ pValue_ =
  DocumentFilter' {key = pKey_, value = pValue_}

-- | The name of the filter.
documentFilter_key :: Lens.Lens' DocumentFilter DocumentFilterKey
documentFilter_key = Lens.lens (\DocumentFilter' {key} -> key) (\s@DocumentFilter' {} a -> s {key = a} :: DocumentFilter)

-- | The value of the filter.
documentFilter_value :: Lens.Lens' DocumentFilter Prelude.Text
documentFilter_value = Lens.lens (\DocumentFilter' {value} -> value) (\s@DocumentFilter' {} a -> s {value = a} :: DocumentFilter)

instance Prelude.Hashable DocumentFilter where
  hashWithSalt _salt DocumentFilter' {..} =
    _salt
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData DocumentFilter where
  rnf DocumentFilter' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value

instance Data.ToJSON DocumentFilter where
  toJSON DocumentFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("key" Data..= key),
            Prelude.Just ("value" Data..= value)
          ]
      )
