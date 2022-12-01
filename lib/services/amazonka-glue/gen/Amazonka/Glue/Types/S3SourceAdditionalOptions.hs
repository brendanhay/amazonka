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
-- Module      : Amazonka.Glue.Types.S3SourceAdditionalOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.S3SourceAdditionalOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies additional connection options for the Amazon S3 data store.
--
-- /See:/ 'newS3SourceAdditionalOptions' smart constructor.
data S3SourceAdditionalOptions = S3SourceAdditionalOptions'
  { -- | Sets the upper limit for the target size of the dataset in bytes that
    -- will be processed.
    boundedSize :: Prelude.Maybe Prelude.Integer,
    -- | Sets the upper limit for the target number of files that will be
    -- processed.
    boundedFiles :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3SourceAdditionalOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'boundedSize', 's3SourceAdditionalOptions_boundedSize' - Sets the upper limit for the target size of the dataset in bytes that
-- will be processed.
--
-- 'boundedFiles', 's3SourceAdditionalOptions_boundedFiles' - Sets the upper limit for the target number of files that will be
-- processed.
newS3SourceAdditionalOptions ::
  S3SourceAdditionalOptions
newS3SourceAdditionalOptions =
  S3SourceAdditionalOptions'
    { boundedSize =
        Prelude.Nothing,
      boundedFiles = Prelude.Nothing
    }

-- | Sets the upper limit for the target size of the dataset in bytes that
-- will be processed.
s3SourceAdditionalOptions_boundedSize :: Lens.Lens' S3SourceAdditionalOptions (Prelude.Maybe Prelude.Integer)
s3SourceAdditionalOptions_boundedSize = Lens.lens (\S3SourceAdditionalOptions' {boundedSize} -> boundedSize) (\s@S3SourceAdditionalOptions' {} a -> s {boundedSize = a} :: S3SourceAdditionalOptions)

-- | Sets the upper limit for the target number of files that will be
-- processed.
s3SourceAdditionalOptions_boundedFiles :: Lens.Lens' S3SourceAdditionalOptions (Prelude.Maybe Prelude.Integer)
s3SourceAdditionalOptions_boundedFiles = Lens.lens (\S3SourceAdditionalOptions' {boundedFiles} -> boundedFiles) (\s@S3SourceAdditionalOptions' {} a -> s {boundedFiles = a} :: S3SourceAdditionalOptions)

instance Core.FromJSON S3SourceAdditionalOptions where
  parseJSON =
    Core.withObject
      "S3SourceAdditionalOptions"
      ( \x ->
          S3SourceAdditionalOptions'
            Prelude.<$> (x Core..:? "BoundedSize")
            Prelude.<*> (x Core..:? "BoundedFiles")
      )

instance Prelude.Hashable S3SourceAdditionalOptions where
  hashWithSalt _salt S3SourceAdditionalOptions' {..} =
    _salt `Prelude.hashWithSalt` boundedSize
      `Prelude.hashWithSalt` boundedFiles

instance Prelude.NFData S3SourceAdditionalOptions where
  rnf S3SourceAdditionalOptions' {..} =
    Prelude.rnf boundedSize
      `Prelude.seq` Prelude.rnf boundedFiles

instance Core.ToJSON S3SourceAdditionalOptions where
  toJSON S3SourceAdditionalOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("BoundedSize" Core..=) Prelude.<$> boundedSize,
            ("BoundedFiles" Core..=) Prelude.<$> boundedFiles
          ]
      )
