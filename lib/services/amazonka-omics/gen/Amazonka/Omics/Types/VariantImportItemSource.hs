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
-- Module      : Amazonka.Omics.Types.VariantImportItemSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.VariantImportItemSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A imported variant item\'s source.
--
-- /See:/ 'newVariantImportItemSource' smart constructor.
data VariantImportItemSource = VariantImportItemSource'
  { -- | The source file\'s location in Amazon S3.
    source :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VariantImportItemSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'source', 'variantImportItemSource_source' - The source file\'s location in Amazon S3.
newVariantImportItemSource ::
  -- | 'source'
  Prelude.Text ->
  VariantImportItemSource
newVariantImportItemSource pSource_ =
  VariantImportItemSource' {source = pSource_}

-- | The source file\'s location in Amazon S3.
variantImportItemSource_source :: Lens.Lens' VariantImportItemSource Prelude.Text
variantImportItemSource_source = Lens.lens (\VariantImportItemSource' {source} -> source) (\s@VariantImportItemSource' {} a -> s {source = a} :: VariantImportItemSource)

instance Prelude.Hashable VariantImportItemSource where
  hashWithSalt _salt VariantImportItemSource' {..} =
    _salt `Prelude.hashWithSalt` source

instance Prelude.NFData VariantImportItemSource where
  rnf VariantImportItemSource' {..} = Prelude.rnf source

instance Data.ToJSON VariantImportItemSource where
  toJSON VariantImportItemSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("source" Data..= source)]
      )
