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
-- Module      : Network.AWS.IoT.Types.S3Location
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.S3Location where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The S3 location.
--
-- /See:/ 'newS3Location' smart constructor.
data S3Location = S3Location'
  { -- | The S3 key.
    key :: Core.Maybe Core.Text,
    -- | The S3 bucket version.
    version :: Core.Maybe Core.Text,
    -- | The S3 bucket.
    bucket :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'S3Location' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 's3Location_key' - The S3 key.
--
-- 'version', 's3Location_version' - The S3 bucket version.
--
-- 'bucket', 's3Location_bucket' - The S3 bucket.
newS3Location ::
  S3Location
newS3Location =
  S3Location'
    { key = Core.Nothing,
      version = Core.Nothing,
      bucket = Core.Nothing
    }

-- | The S3 key.
s3Location_key :: Lens.Lens' S3Location (Core.Maybe Core.Text)
s3Location_key = Lens.lens (\S3Location' {key} -> key) (\s@S3Location' {} a -> s {key = a} :: S3Location)

-- | The S3 bucket version.
s3Location_version :: Lens.Lens' S3Location (Core.Maybe Core.Text)
s3Location_version = Lens.lens (\S3Location' {version} -> version) (\s@S3Location' {} a -> s {version = a} :: S3Location)

-- | The S3 bucket.
s3Location_bucket :: Lens.Lens' S3Location (Core.Maybe Core.Text)
s3Location_bucket = Lens.lens (\S3Location' {bucket} -> bucket) (\s@S3Location' {} a -> s {bucket = a} :: S3Location)

instance Core.FromJSON S3Location where
  parseJSON =
    Core.withObject
      "S3Location"
      ( \x ->
          S3Location'
            Core.<$> (x Core..:? "key")
            Core.<*> (x Core..:? "version")
            Core.<*> (x Core..:? "bucket")
      )

instance Core.Hashable S3Location

instance Core.NFData S3Location

instance Core.ToJSON S3Location where
  toJSON S3Location' {..} =
    Core.object
      ( Core.catMaybes
          [ ("key" Core..=) Core.<$> key,
            ("version" Core..=) Core.<$> version,
            ("bucket" Core..=) Core.<$> bucket
          ]
      )
