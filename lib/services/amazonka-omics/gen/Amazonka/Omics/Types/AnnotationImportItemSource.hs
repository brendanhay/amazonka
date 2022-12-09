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
-- Module      : Amazonka.Omics.Types.AnnotationImportItemSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.AnnotationImportItemSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A source for an annotation import job.
--
-- /See:/ 'newAnnotationImportItemSource' smart constructor.
data AnnotationImportItemSource = AnnotationImportItemSource'
  { -- | The source file\'s location in Amazon S3.
    source :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnnotationImportItemSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'source', 'annotationImportItemSource_source' - The source file\'s location in Amazon S3.
newAnnotationImportItemSource ::
  -- | 'source'
  Prelude.Text ->
  AnnotationImportItemSource
newAnnotationImportItemSource pSource_ =
  AnnotationImportItemSource' {source = pSource_}

-- | The source file\'s location in Amazon S3.
annotationImportItemSource_source :: Lens.Lens' AnnotationImportItemSource Prelude.Text
annotationImportItemSource_source = Lens.lens (\AnnotationImportItemSource' {source} -> source) (\s@AnnotationImportItemSource' {} a -> s {source = a} :: AnnotationImportItemSource)

instance Prelude.Hashable AnnotationImportItemSource where
  hashWithSalt _salt AnnotationImportItemSource' {..} =
    _salt `Prelude.hashWithSalt` source

instance Prelude.NFData AnnotationImportItemSource where
  rnf AnnotationImportItemSource' {..} =
    Prelude.rnf source

instance Data.ToJSON AnnotationImportItemSource where
  toJSON AnnotationImportItemSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("source" Data..= source)]
      )
