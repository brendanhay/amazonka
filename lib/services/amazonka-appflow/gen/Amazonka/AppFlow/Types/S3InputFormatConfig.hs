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
-- Module      : Amazonka.AppFlow.Types.S3InputFormatConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.S3InputFormatConfig where

import Amazonka.AppFlow.Types.S3InputFileType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | When you use Amazon S3 as the source, the configuration format that you
-- provide the flow input data.
--
-- /See:/ 'newS3InputFormatConfig' smart constructor.
data S3InputFormatConfig = S3InputFormatConfig'
  { -- | The file type that Amazon AppFlow gets from your Amazon S3 bucket.
    s3InputFileType :: Prelude.Maybe S3InputFileType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3InputFormatConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3InputFileType', 's3InputFormatConfig_s3InputFileType' - The file type that Amazon AppFlow gets from your Amazon S3 bucket.
newS3InputFormatConfig ::
  S3InputFormatConfig
newS3InputFormatConfig =
  S3InputFormatConfig'
    { s3InputFileType =
        Prelude.Nothing
    }

-- | The file type that Amazon AppFlow gets from your Amazon S3 bucket.
s3InputFormatConfig_s3InputFileType :: Lens.Lens' S3InputFormatConfig (Prelude.Maybe S3InputFileType)
s3InputFormatConfig_s3InputFileType = Lens.lens (\S3InputFormatConfig' {s3InputFileType} -> s3InputFileType) (\s@S3InputFormatConfig' {} a -> s {s3InputFileType = a} :: S3InputFormatConfig)

instance Core.FromJSON S3InputFormatConfig where
  parseJSON =
    Core.withObject
      "S3InputFormatConfig"
      ( \x ->
          S3InputFormatConfig'
            Prelude.<$> (x Core..:? "s3InputFileType")
      )

instance Prelude.Hashable S3InputFormatConfig where
  hashWithSalt _salt S3InputFormatConfig' {..} =
    _salt `Prelude.hashWithSalt` s3InputFileType

instance Prelude.NFData S3InputFormatConfig where
  rnf S3InputFormatConfig' {..} =
    Prelude.rnf s3InputFileType

instance Core.ToJSON S3InputFormatConfig where
  toJSON S3InputFormatConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("s3InputFileType" Core..=)
              Prelude.<$> s3InputFileType
          ]
      )
