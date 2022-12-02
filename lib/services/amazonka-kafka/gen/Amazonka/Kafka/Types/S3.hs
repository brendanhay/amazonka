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
-- Module      : Amazonka.Kafka.Types.S3
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.S3 where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newS3' smart constructor.
data S3 = S3'
  { bucket :: Prelude.Maybe Prelude.Text,
    prefix :: Prelude.Maybe Prelude.Text,
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucket', 's3_bucket' - Undocumented member.
--
-- 'prefix', 's3_prefix' - Undocumented member.
--
-- 'enabled', 's3_enabled' - Undocumented member.
newS3 ::
  -- | 'enabled'
  Prelude.Bool ->
  S3
newS3 pEnabled_ =
  S3'
    { bucket = Prelude.Nothing,
      prefix = Prelude.Nothing,
      enabled = pEnabled_
    }

-- | Undocumented member.
s3_bucket :: Lens.Lens' S3 (Prelude.Maybe Prelude.Text)
s3_bucket = Lens.lens (\S3' {bucket} -> bucket) (\s@S3' {} a -> s {bucket = a} :: S3)

-- | Undocumented member.
s3_prefix :: Lens.Lens' S3 (Prelude.Maybe Prelude.Text)
s3_prefix = Lens.lens (\S3' {prefix} -> prefix) (\s@S3' {} a -> s {prefix = a} :: S3)

-- | Undocumented member.
s3_enabled :: Lens.Lens' S3 Prelude.Bool
s3_enabled = Lens.lens (\S3' {enabled} -> enabled) (\s@S3' {} a -> s {enabled = a} :: S3)

instance Data.FromJSON S3 where
  parseJSON =
    Data.withObject
      "S3"
      ( \x ->
          S3'
            Prelude.<$> (x Data..:? "bucket")
            Prelude.<*> (x Data..:? "prefix")
            Prelude.<*> (x Data..: "enabled")
      )

instance Prelude.Hashable S3 where
  hashWithSalt _salt S3' {..} =
    _salt `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData S3 where
  rnf S3' {..} =
    Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf enabled

instance Data.ToJSON S3 where
  toJSON S3' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bucket" Data..=) Prelude.<$> bucket,
            ("prefix" Data..=) Prelude.<$> prefix,
            Prelude.Just ("enabled" Data..= enabled)
          ]
      )
