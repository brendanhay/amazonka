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
-- Module      : Network.AWS.KafkaConnect.Types.S3LogDelivery
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KafkaConnect.Types.S3LogDelivery where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details about delivering logs to Amazon S3.
--
-- /See:/ 'newS3LogDelivery' smart constructor.
data S3LogDelivery = S3LogDelivery'
  { -- | The S3 prefix that is the destination for log delivery.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | The name of the S3 bucket that is the destination for log delivery.
    bucket :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether connector logs get sent to the specified Amazon S3
    -- destination.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3LogDelivery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefix', 's3LogDelivery_prefix' - The S3 prefix that is the destination for log delivery.
--
-- 'bucket', 's3LogDelivery_bucket' - The name of the S3 bucket that is the destination for log delivery.
--
-- 'enabled', 's3LogDelivery_enabled' - Specifies whether connector logs get sent to the specified Amazon S3
-- destination.
newS3LogDelivery ::
  -- | 'enabled'
  Prelude.Bool ->
  S3LogDelivery
newS3LogDelivery pEnabled_ =
  S3LogDelivery'
    { prefix = Prelude.Nothing,
      bucket = Prelude.Nothing,
      enabled = pEnabled_
    }

-- | The S3 prefix that is the destination for log delivery.
s3LogDelivery_prefix :: Lens.Lens' S3LogDelivery (Prelude.Maybe Prelude.Text)
s3LogDelivery_prefix = Lens.lens (\S3LogDelivery' {prefix} -> prefix) (\s@S3LogDelivery' {} a -> s {prefix = a} :: S3LogDelivery)

-- | The name of the S3 bucket that is the destination for log delivery.
s3LogDelivery_bucket :: Lens.Lens' S3LogDelivery (Prelude.Maybe Prelude.Text)
s3LogDelivery_bucket = Lens.lens (\S3LogDelivery' {bucket} -> bucket) (\s@S3LogDelivery' {} a -> s {bucket = a} :: S3LogDelivery)

-- | Specifies whether connector logs get sent to the specified Amazon S3
-- destination.
s3LogDelivery_enabled :: Lens.Lens' S3LogDelivery Prelude.Bool
s3LogDelivery_enabled = Lens.lens (\S3LogDelivery' {enabled} -> enabled) (\s@S3LogDelivery' {} a -> s {enabled = a} :: S3LogDelivery)

instance Prelude.Hashable S3LogDelivery

instance Prelude.NFData S3LogDelivery

instance Core.ToJSON S3LogDelivery where
  toJSON S3LogDelivery' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("prefix" Core..=) Prelude.<$> prefix,
            ("bucket" Core..=) Prelude.<$> bucket,
            Prelude.Just ("enabled" Core..= enabled)
          ]
      )
