{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Translate.Types.ParallelDataConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.ParallelDataConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Translate.Types.ParallelDataFormat

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON ParallelDataConfig where
  parseJSON =
    Prelude.withObject
      "ParallelDataConfig"
      ( \x ->
          ParallelDataConfig'
            Prelude.<$> (x Prelude..: "S3Uri")
            Prelude.<*> (x Prelude..: "Format")
      )

instance Prelude.Hashable ParallelDataConfig

instance Prelude.NFData ParallelDataConfig

instance Prelude.ToJSON ParallelDataConfig where
  toJSON ParallelDataConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("S3Uri" Prelude..= s3Uri),
            Prelude.Just ("Format" Prelude..= format)
          ]
      )
