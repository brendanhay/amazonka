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
-- Module      : Network.AWS.Kafka.Types.S3
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kafka.Types.S3 where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | /See:/ 'newS3' smart constructor.
data S3 = S3'
  { prefix :: Prelude.Maybe Prelude.Text,
    bucket :: Prelude.Maybe Prelude.Text,
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
-- 'prefix', 's3_prefix' - Undocumented member.
--
-- 'bucket', 's3_bucket' - Undocumented member.
--
-- 'enabled', 's3_enabled' - Undocumented member.
newS3 ::
  -- | 'enabled'
  Prelude.Bool ->
  S3
newS3 pEnabled_ =
  S3'
    { prefix = Prelude.Nothing,
      bucket = Prelude.Nothing,
      enabled = pEnabled_
    }

-- | Undocumented member.
s3_prefix :: Lens.Lens' S3 (Prelude.Maybe Prelude.Text)
s3_prefix = Lens.lens (\S3' {prefix} -> prefix) (\s@S3' {} a -> s {prefix = a} :: S3)

-- | Undocumented member.
s3_bucket :: Lens.Lens' S3 (Prelude.Maybe Prelude.Text)
s3_bucket = Lens.lens (\S3' {bucket} -> bucket) (\s@S3' {} a -> s {bucket = a} :: S3)

-- | Undocumented member.
s3_enabled :: Lens.Lens' S3 Prelude.Bool
s3_enabled = Lens.lens (\S3' {enabled} -> enabled) (\s@S3' {} a -> s {enabled = a} :: S3)

instance Core.FromJSON S3 where
  parseJSON =
    Core.withObject
      "S3"
      ( \x ->
          S3'
            Prelude.<$> (x Core..:? "prefix")
            Prelude.<*> (x Core..:? "bucket")
            Prelude.<*> (x Core..: "enabled")
      )

instance Prelude.Hashable S3

instance Prelude.NFData S3

instance Core.ToJSON S3 where
  toJSON S3' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("prefix" Core..=) Prelude.<$> prefix,
            ("bucket" Core..=) Prelude.<$> bucket,
            Prelude.Just ("enabled" Core..= enabled)
          ]
      )
