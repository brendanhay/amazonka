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
-- Module      : Network.AWS.IoT.Types.S3Destination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.S3Destination where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the location of updated firmware in S3.
--
-- /See:/ 'newS3Destination' smart constructor.
data S3Destination = S3Destination'
  { -- | The S3 prefix.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | The S3 bucket that contains the updated firmware.
    bucket :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { prefix = Prelude.Nothing,
      bucket = Prelude.Nothing
    }

-- | The S3 prefix.
s3Destination_prefix :: Lens.Lens' S3Destination (Prelude.Maybe Prelude.Text)
s3Destination_prefix = Lens.lens (\S3Destination' {prefix} -> prefix) (\s@S3Destination' {} a -> s {prefix = a} :: S3Destination)

-- | The S3 bucket that contains the updated firmware.
s3Destination_bucket :: Lens.Lens' S3Destination (Prelude.Maybe Prelude.Text)
s3Destination_bucket = Lens.lens (\S3Destination' {bucket} -> bucket) (\s@S3Destination' {} a -> s {bucket = a} :: S3Destination)

instance Prelude.FromJSON S3Destination where
  parseJSON =
    Prelude.withObject
      "S3Destination"
      ( \x ->
          S3Destination'
            Prelude.<$> (x Prelude..:? "prefix")
            Prelude.<*> (x Prelude..:? "bucket")
      )

instance Prelude.Hashable S3Destination

instance Prelude.NFData S3Destination

instance Prelude.ToJSON S3Destination where
  toJSON S3Destination' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("prefix" Prelude..=) Prelude.<$> prefix,
            ("bucket" Prelude..=) Prelude.<$> bucket
          ]
      )
