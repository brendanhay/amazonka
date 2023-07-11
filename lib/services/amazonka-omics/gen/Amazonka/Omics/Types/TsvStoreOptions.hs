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
-- Module      : Amazonka.Omics.Types.TsvStoreOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.TsvStoreOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.AnnotationType
import Amazonka.Omics.Types.FormatToHeaderKey
import Amazonka.Omics.Types.SchemaValueType
import qualified Amazonka.Prelude as Prelude

-- | File settings for a TSV store.
--
-- /See:/ 'newTsvStoreOptions' smart constructor.
data TsvStoreOptions = TsvStoreOptions'
  { -- | The store\'s annotation type.
    annotationType :: Prelude.Maybe AnnotationType,
    -- | The store\'s header key to column name mapping.
    formatToHeader :: Prelude.Maybe (Prelude.HashMap FormatToHeaderKey Prelude.Text),
    -- | The store\'s schema.
    schema :: Prelude.Maybe (Prelude.NonEmpty (Prelude.HashMap Prelude.Text SchemaValueType))
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TsvStoreOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'annotationType', 'tsvStoreOptions_annotationType' - The store\'s annotation type.
--
-- 'formatToHeader', 'tsvStoreOptions_formatToHeader' - The store\'s header key to column name mapping.
--
-- 'schema', 'tsvStoreOptions_schema' - The store\'s schema.
newTsvStoreOptions ::
  TsvStoreOptions
newTsvStoreOptions =
  TsvStoreOptions'
    { annotationType = Prelude.Nothing,
      formatToHeader = Prelude.Nothing,
      schema = Prelude.Nothing
    }

-- | The store\'s annotation type.
tsvStoreOptions_annotationType :: Lens.Lens' TsvStoreOptions (Prelude.Maybe AnnotationType)
tsvStoreOptions_annotationType = Lens.lens (\TsvStoreOptions' {annotationType} -> annotationType) (\s@TsvStoreOptions' {} a -> s {annotationType = a} :: TsvStoreOptions)

-- | The store\'s header key to column name mapping.
tsvStoreOptions_formatToHeader :: Lens.Lens' TsvStoreOptions (Prelude.Maybe (Prelude.HashMap FormatToHeaderKey Prelude.Text))
tsvStoreOptions_formatToHeader = Lens.lens (\TsvStoreOptions' {formatToHeader} -> formatToHeader) (\s@TsvStoreOptions' {} a -> s {formatToHeader = a} :: TsvStoreOptions) Prelude.. Lens.mapping Lens.coerced

-- | The store\'s schema.
tsvStoreOptions_schema :: Lens.Lens' TsvStoreOptions (Prelude.Maybe (Prelude.NonEmpty (Prelude.HashMap Prelude.Text SchemaValueType)))
tsvStoreOptions_schema = Lens.lens (\TsvStoreOptions' {schema} -> schema) (\s@TsvStoreOptions' {} a -> s {schema = a} :: TsvStoreOptions) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON TsvStoreOptions where
  parseJSON =
    Data.withObject
      "TsvStoreOptions"
      ( \x ->
          TsvStoreOptions'
            Prelude.<$> (x Data..:? "annotationType")
            Prelude.<*> (x Data..:? "formatToHeader" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "schema")
      )

instance Prelude.Hashable TsvStoreOptions where
  hashWithSalt _salt TsvStoreOptions' {..} =
    _salt
      `Prelude.hashWithSalt` annotationType
      `Prelude.hashWithSalt` formatToHeader
      `Prelude.hashWithSalt` schema

instance Prelude.NFData TsvStoreOptions where
  rnf TsvStoreOptions' {..} =
    Prelude.rnf annotationType
      `Prelude.seq` Prelude.rnf formatToHeader
      `Prelude.seq` Prelude.rnf schema

instance Data.ToJSON TsvStoreOptions where
  toJSON TsvStoreOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("annotationType" Data..=)
              Prelude.<$> annotationType,
            ("formatToHeader" Data..=)
              Prelude.<$> formatToHeader,
            ("schema" Data..=) Prelude.<$> schema
          ]
      )
