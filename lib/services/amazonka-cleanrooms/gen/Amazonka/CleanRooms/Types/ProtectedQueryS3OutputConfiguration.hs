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
-- Module      : Amazonka.CleanRooms.Types.ProtectedQueryS3OutputConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.ProtectedQueryS3OutputConfiguration where

import Amazonka.CleanRooms.Types.ResultFormat
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the configuration to write the query results to S3.
--
-- /See:/ 'newProtectedQueryS3OutputConfiguration' smart constructor.
data ProtectedQueryS3OutputConfiguration = ProtectedQueryS3OutputConfiguration'
  { -- | The S3 prefix to unload the protected query results.
    keyPrefix :: Prelude.Maybe Prelude.Text,
    -- | Intended file format of the result.
    resultFormat :: ResultFormat,
    -- | The S3 bucket to unload the protected query results.
    bucket :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProtectedQueryS3OutputConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPrefix', 'protectedQueryS3OutputConfiguration_keyPrefix' - The S3 prefix to unload the protected query results.
--
-- 'resultFormat', 'protectedQueryS3OutputConfiguration_resultFormat' - Intended file format of the result.
--
-- 'bucket', 'protectedQueryS3OutputConfiguration_bucket' - The S3 bucket to unload the protected query results.
newProtectedQueryS3OutputConfiguration ::
  -- | 'resultFormat'
  ResultFormat ->
  -- | 'bucket'
  Prelude.Text ->
  ProtectedQueryS3OutputConfiguration
newProtectedQueryS3OutputConfiguration
  pResultFormat_
  pBucket_ =
    ProtectedQueryS3OutputConfiguration'
      { keyPrefix =
          Prelude.Nothing,
        resultFormat = pResultFormat_,
        bucket = pBucket_
      }

-- | The S3 prefix to unload the protected query results.
protectedQueryS3OutputConfiguration_keyPrefix :: Lens.Lens' ProtectedQueryS3OutputConfiguration (Prelude.Maybe Prelude.Text)
protectedQueryS3OutputConfiguration_keyPrefix = Lens.lens (\ProtectedQueryS3OutputConfiguration' {keyPrefix} -> keyPrefix) (\s@ProtectedQueryS3OutputConfiguration' {} a -> s {keyPrefix = a} :: ProtectedQueryS3OutputConfiguration)

-- | Intended file format of the result.
protectedQueryS3OutputConfiguration_resultFormat :: Lens.Lens' ProtectedQueryS3OutputConfiguration ResultFormat
protectedQueryS3OutputConfiguration_resultFormat = Lens.lens (\ProtectedQueryS3OutputConfiguration' {resultFormat} -> resultFormat) (\s@ProtectedQueryS3OutputConfiguration' {} a -> s {resultFormat = a} :: ProtectedQueryS3OutputConfiguration)

-- | The S3 bucket to unload the protected query results.
protectedQueryS3OutputConfiguration_bucket :: Lens.Lens' ProtectedQueryS3OutputConfiguration Prelude.Text
protectedQueryS3OutputConfiguration_bucket = Lens.lens (\ProtectedQueryS3OutputConfiguration' {bucket} -> bucket) (\s@ProtectedQueryS3OutputConfiguration' {} a -> s {bucket = a} :: ProtectedQueryS3OutputConfiguration)

instance
  Data.FromJSON
    ProtectedQueryS3OutputConfiguration
  where
  parseJSON =
    Data.withObject
      "ProtectedQueryS3OutputConfiguration"
      ( \x ->
          ProtectedQueryS3OutputConfiguration'
            Prelude.<$> (x Data..:? "keyPrefix")
            Prelude.<*> (x Data..: "resultFormat")
            Prelude.<*> (x Data..: "bucket")
      )

instance
  Prelude.Hashable
    ProtectedQueryS3OutputConfiguration
  where
  hashWithSalt
    _salt
    ProtectedQueryS3OutputConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` keyPrefix
        `Prelude.hashWithSalt` resultFormat
        `Prelude.hashWithSalt` bucket

instance
  Prelude.NFData
    ProtectedQueryS3OutputConfiguration
  where
  rnf ProtectedQueryS3OutputConfiguration' {..} =
    Prelude.rnf keyPrefix
      `Prelude.seq` Prelude.rnf resultFormat
      `Prelude.seq` Prelude.rnf bucket

instance
  Data.ToJSON
    ProtectedQueryS3OutputConfiguration
  where
  toJSON ProtectedQueryS3OutputConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("keyPrefix" Data..=) Prelude.<$> keyPrefix,
            Prelude.Just ("resultFormat" Data..= resultFormat),
            Prelude.Just ("bucket" Data..= bucket)
          ]
      )
