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
-- Module      : Amazonka.Evidently.Types.S3DestinationConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.S3DestinationConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | If the project stores evaluation events in an Amazon S3 bucket, this
-- structure stores the bucket name and bucket prefix.
--
-- /See:/ 'newS3DestinationConfig' smart constructor.
data S3DestinationConfig = S3DestinationConfig'
  { -- | The name of the bucket in which Evidently stores evaluation events.
    bucket :: Prelude.Maybe Prelude.Text,
    -- | The bucket prefix in which Evidently stores evaluation events.
    prefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3DestinationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucket', 's3DestinationConfig_bucket' - The name of the bucket in which Evidently stores evaluation events.
--
-- 'prefix', 's3DestinationConfig_prefix' - The bucket prefix in which Evidently stores evaluation events.
newS3DestinationConfig ::
  S3DestinationConfig
newS3DestinationConfig =
  S3DestinationConfig'
    { bucket = Prelude.Nothing,
      prefix = Prelude.Nothing
    }

-- | The name of the bucket in which Evidently stores evaluation events.
s3DestinationConfig_bucket :: Lens.Lens' S3DestinationConfig (Prelude.Maybe Prelude.Text)
s3DestinationConfig_bucket = Lens.lens (\S3DestinationConfig' {bucket} -> bucket) (\s@S3DestinationConfig' {} a -> s {bucket = a} :: S3DestinationConfig)

-- | The bucket prefix in which Evidently stores evaluation events.
s3DestinationConfig_prefix :: Lens.Lens' S3DestinationConfig (Prelude.Maybe Prelude.Text)
s3DestinationConfig_prefix = Lens.lens (\S3DestinationConfig' {prefix} -> prefix) (\s@S3DestinationConfig' {} a -> s {prefix = a} :: S3DestinationConfig)

instance Prelude.Hashable S3DestinationConfig where
  hashWithSalt _salt S3DestinationConfig' {..} =
    _salt
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` prefix

instance Prelude.NFData S3DestinationConfig where
  rnf S3DestinationConfig' {..} =
    Prelude.rnf bucket `Prelude.seq` Prelude.rnf prefix

instance Data.ToJSON S3DestinationConfig where
  toJSON S3DestinationConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bucket" Data..=) Prelude.<$> bucket,
            ("prefix" Data..=) Prelude.<$> prefix
          ]
      )
