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
-- Module      : Amazonka.KafkaConnect.Types.S3LogDeliveryDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.S3LogDeliveryDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The description of the details about delivering logs to Amazon S3.
--
-- /See:/ 'newS3LogDeliveryDescription' smart constructor.
data S3LogDeliveryDescription = S3LogDeliveryDescription'
  { -- | Specifies whether connector logs get sent to the specified Amazon S3
    -- destination.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The S3 prefix that is the destination for log delivery.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | The name of the S3 bucket that is the destination for log delivery.
    bucket :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3LogDeliveryDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 's3LogDeliveryDescription_enabled' - Specifies whether connector logs get sent to the specified Amazon S3
-- destination.
--
-- 'prefix', 's3LogDeliveryDescription_prefix' - The S3 prefix that is the destination for log delivery.
--
-- 'bucket', 's3LogDeliveryDescription_bucket' - The name of the S3 bucket that is the destination for log delivery.
newS3LogDeliveryDescription ::
  S3LogDeliveryDescription
newS3LogDeliveryDescription =
  S3LogDeliveryDescription'
    { enabled =
        Prelude.Nothing,
      prefix = Prelude.Nothing,
      bucket = Prelude.Nothing
    }

-- | Specifies whether connector logs get sent to the specified Amazon S3
-- destination.
s3LogDeliveryDescription_enabled :: Lens.Lens' S3LogDeliveryDescription (Prelude.Maybe Prelude.Bool)
s3LogDeliveryDescription_enabled = Lens.lens (\S3LogDeliveryDescription' {enabled} -> enabled) (\s@S3LogDeliveryDescription' {} a -> s {enabled = a} :: S3LogDeliveryDescription)

-- | The S3 prefix that is the destination for log delivery.
s3LogDeliveryDescription_prefix :: Lens.Lens' S3LogDeliveryDescription (Prelude.Maybe Prelude.Text)
s3LogDeliveryDescription_prefix = Lens.lens (\S3LogDeliveryDescription' {prefix} -> prefix) (\s@S3LogDeliveryDescription' {} a -> s {prefix = a} :: S3LogDeliveryDescription)

-- | The name of the S3 bucket that is the destination for log delivery.
s3LogDeliveryDescription_bucket :: Lens.Lens' S3LogDeliveryDescription (Prelude.Maybe Prelude.Text)
s3LogDeliveryDescription_bucket = Lens.lens (\S3LogDeliveryDescription' {bucket} -> bucket) (\s@S3LogDeliveryDescription' {} a -> s {bucket = a} :: S3LogDeliveryDescription)

instance Core.FromJSON S3LogDeliveryDescription where
  parseJSON =
    Core.withObject
      "S3LogDeliveryDescription"
      ( \x ->
          S3LogDeliveryDescription'
            Prelude.<$> (x Core..:? "enabled")
            Prelude.<*> (x Core..:? "prefix")
            Prelude.<*> (x Core..:? "bucket")
      )

instance Prelude.Hashable S3LogDeliveryDescription where
  hashWithSalt salt' S3LogDeliveryDescription' {..} =
    salt' `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData S3LogDeliveryDescription where
  rnf S3LogDeliveryDescription' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf prefix
