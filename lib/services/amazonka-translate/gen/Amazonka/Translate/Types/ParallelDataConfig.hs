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
-- Module      : Amazonka.Translate.Types.ParallelDataConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Translate.Types.ParallelDataConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Translate.Types.ParallelDataFormat

-- | Specifies the format and S3 location of the parallel data input file.
--
-- /See:/ 'newParallelDataConfig' smart constructor.
data ParallelDataConfig = ParallelDataConfig'
  { -- | The URI of the Amazon S3 folder that contains the parallel data input
    -- file. The folder must be in the same Region as the API endpoint you are
    -- calling.
    s3Uri :: Prelude.Text,
    -- | The format of the parallel data input file.
    format :: ParallelDataFormat
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParallelDataConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Uri', 'parallelDataConfig_s3Uri' - The URI of the Amazon S3 folder that contains the parallel data input
-- file. The folder must be in the same Region as the API endpoint you are
-- calling.
--
-- 'format', 'parallelDataConfig_format' - The format of the parallel data input file.
newParallelDataConfig ::
  -- | 's3Uri'
  Prelude.Text ->
  -- | 'format'
  ParallelDataFormat ->
  ParallelDataConfig
newParallelDataConfig pS3Uri_ pFormat_ =
  ParallelDataConfig'
    { s3Uri = pS3Uri_,
      format = pFormat_
    }

-- | The URI of the Amazon S3 folder that contains the parallel data input
-- file. The folder must be in the same Region as the API endpoint you are
-- calling.
parallelDataConfig_s3Uri :: Lens.Lens' ParallelDataConfig Prelude.Text
parallelDataConfig_s3Uri = Lens.lens (\ParallelDataConfig' {s3Uri} -> s3Uri) (\s@ParallelDataConfig' {} a -> s {s3Uri = a} :: ParallelDataConfig)

-- | The format of the parallel data input file.
parallelDataConfig_format :: Lens.Lens' ParallelDataConfig ParallelDataFormat
parallelDataConfig_format = Lens.lens (\ParallelDataConfig' {format} -> format) (\s@ParallelDataConfig' {} a -> s {format = a} :: ParallelDataConfig)

instance Data.FromJSON ParallelDataConfig where
  parseJSON =
    Data.withObject
      "ParallelDataConfig"
      ( \x ->
          ParallelDataConfig'
            Prelude.<$> (x Data..: "S3Uri")
            Prelude.<*> (x Data..: "Format")
      )

instance Prelude.Hashable ParallelDataConfig where
  hashWithSalt _salt ParallelDataConfig' {..} =
    _salt
      `Prelude.hashWithSalt` s3Uri
      `Prelude.hashWithSalt` format

instance Prelude.NFData ParallelDataConfig where
  rnf ParallelDataConfig' {..} =
    Prelude.rnf s3Uri `Prelude.seq` Prelude.rnf format

instance Data.ToJSON ParallelDataConfig where
  toJSON ParallelDataConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("S3Uri" Data..= s3Uri),
            Prelude.Just ("Format" Data..= format)
          ]
      )
