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
-- Module      : Amazonka.KafkaConnect.Types.S3LogDelivery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.S3LogDelivery where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about delivering logs to Amazon S3.
--
-- /See:/ 'newS3LogDelivery' smart constructor.
data S3LogDelivery = S3LogDelivery'
  { -- | The name of the S3 bucket that is the destination for log delivery.
    bucket :: Prelude.Maybe Prelude.Text,
    -- | The S3 prefix that is the destination for log delivery.
    prefix :: Prelude.Maybe Prelude.Text,
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
-- 'bucket', 's3LogDelivery_bucket' - The name of the S3 bucket that is the destination for log delivery.
--
-- 'prefix', 's3LogDelivery_prefix' - The S3 prefix that is the destination for log delivery.
--
-- 'enabled', 's3LogDelivery_enabled' - Specifies whether connector logs get sent to the specified Amazon S3
-- destination.
newS3LogDelivery ::
  -- | 'enabled'
  Prelude.Bool ->
  S3LogDelivery
newS3LogDelivery pEnabled_ =
  S3LogDelivery'
    { bucket = Prelude.Nothing,
      prefix = Prelude.Nothing,
      enabled = pEnabled_
    }

-- | The name of the S3 bucket that is the destination for log delivery.
s3LogDelivery_bucket :: Lens.Lens' S3LogDelivery (Prelude.Maybe Prelude.Text)
s3LogDelivery_bucket = Lens.lens (\S3LogDelivery' {bucket} -> bucket) (\s@S3LogDelivery' {} a -> s {bucket = a} :: S3LogDelivery)

-- | The S3 prefix that is the destination for log delivery.
s3LogDelivery_prefix :: Lens.Lens' S3LogDelivery (Prelude.Maybe Prelude.Text)
s3LogDelivery_prefix = Lens.lens (\S3LogDelivery' {prefix} -> prefix) (\s@S3LogDelivery' {} a -> s {prefix = a} :: S3LogDelivery)

-- | Specifies whether connector logs get sent to the specified Amazon S3
-- destination.
s3LogDelivery_enabled :: Lens.Lens' S3LogDelivery Prelude.Bool
s3LogDelivery_enabled = Lens.lens (\S3LogDelivery' {enabled} -> enabled) (\s@S3LogDelivery' {} a -> s {enabled = a} :: S3LogDelivery)

instance Prelude.Hashable S3LogDelivery where
  hashWithSalt _salt S3LogDelivery' {..} =
    _salt
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData S3LogDelivery where
  rnf S3LogDelivery' {..} =
    Prelude.rnf bucket `Prelude.seq`
      Prelude.rnf prefix `Prelude.seq`
        Prelude.rnf enabled

instance Data.ToJSON S3LogDelivery where
  toJSON S3LogDelivery' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bucket" Data..=) Prelude.<$> bucket,
            ("prefix" Data..=) Prelude.<$> prefix,
            Prelude.Just ("enabled" Data..= enabled)
          ]
      )
