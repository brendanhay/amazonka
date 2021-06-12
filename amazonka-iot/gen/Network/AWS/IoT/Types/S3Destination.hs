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
-- Module      : Network.AWS.IoT.Types.S3Destination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.S3Destination where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the location of updated firmware in S3.
--
-- /See:/ 'newS3Destination' smart constructor.
data S3Destination = S3Destination'
  { -- | The S3 prefix.
    prefix :: Core.Maybe Core.Text,
    -- | The S3 bucket that contains the updated firmware.
    bucket :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'S3Destination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefix', 's3Destination_prefix' - The S3 prefix.
--
-- 'bucket', 's3Destination_bucket' - The S3 bucket that contains the updated firmware.
newS3Destination ::
  S3Destination
newS3Destination =
  S3Destination'
    { prefix = Core.Nothing,
      bucket = Core.Nothing
    }

-- | The S3 prefix.
s3Destination_prefix :: Lens.Lens' S3Destination (Core.Maybe Core.Text)
s3Destination_prefix = Lens.lens (\S3Destination' {prefix} -> prefix) (\s@S3Destination' {} a -> s {prefix = a} :: S3Destination)

-- | The S3 bucket that contains the updated firmware.
s3Destination_bucket :: Lens.Lens' S3Destination (Core.Maybe Core.Text)
s3Destination_bucket = Lens.lens (\S3Destination' {bucket} -> bucket) (\s@S3Destination' {} a -> s {bucket = a} :: S3Destination)

instance Core.FromJSON S3Destination where
  parseJSON =
    Core.withObject
      "S3Destination"
      ( \x ->
          S3Destination'
            Core.<$> (x Core..:? "prefix") Core.<*> (x Core..:? "bucket")
      )

instance Core.Hashable S3Destination

instance Core.NFData S3Destination

instance Core.ToJSON S3Destination where
  toJSON S3Destination' {..} =
    Core.object
      ( Core.catMaybes
          [ ("prefix" Core..=) Core.<$> prefix,
            ("bucket" Core..=) Core.<$> bucket
          ]
      )
