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
-- Module      : Network.AWS.Translate.Types.OutputDataConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.OutputDataConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The output configuration properties for a batch translation job.
--
-- /See:/ 'newOutputDataConfig' smart constructor.
data OutputDataConfig = OutputDataConfig'
  { -- | The URI of the S3 folder that contains a translation job\'s output file.
    -- The folder must be in the same Region as the API endpoint that you are
    -- calling.
    s3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OutputDataConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Uri', 'outputDataConfig_s3Uri' - The URI of the S3 folder that contains a translation job\'s output file.
-- The folder must be in the same Region as the API endpoint that you are
-- calling.
newOutputDataConfig ::
  -- | 's3Uri'
  Prelude.Text ->
  OutputDataConfig
newOutputDataConfig pS3Uri_ =
  OutputDataConfig' {s3Uri = pS3Uri_}

-- | The URI of the S3 folder that contains a translation job\'s output file.
-- The folder must be in the same Region as the API endpoint that you are
-- calling.
outputDataConfig_s3Uri :: Lens.Lens' OutputDataConfig Prelude.Text
outputDataConfig_s3Uri = Lens.lens (\OutputDataConfig' {s3Uri} -> s3Uri) (\s@OutputDataConfig' {} a -> s {s3Uri = a} :: OutputDataConfig)

instance Prelude.FromJSON OutputDataConfig where
  parseJSON =
    Prelude.withObject
      "OutputDataConfig"
      ( \x ->
          OutputDataConfig' Prelude.<$> (x Prelude..: "S3Uri")
      )

instance Prelude.Hashable OutputDataConfig

instance Prelude.NFData OutputDataConfig

instance Prelude.ToJSON OutputDataConfig where
  toJSON OutputDataConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("S3Uri" Prelude..= s3Uri)]
      )
