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
-- Module      : Amazonka.EC2.Types.VerifiedAccessLogS3Destination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VerifiedAccessLogS3Destination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.VerifiedAccessLogDeliveryStatus
import qualified Amazonka.Prelude as Prelude

-- | Options for Amazon S3 as a logging destination.
--
-- /See:/ 'newVerifiedAccessLogS3Destination' smart constructor.
data VerifiedAccessLogS3Destination = VerifiedAccessLogS3Destination'
  { -- | The bucket name.
    bucketName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account number that owns the bucket.
    bucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The delivery status.
    deliveryStatus :: Prelude.Maybe VerifiedAccessLogDeliveryStatus,
    -- | Indicates whether logging is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The bucket prefix.
    prefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifiedAccessLogS3Destination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketName', 'verifiedAccessLogS3Destination_bucketName' - The bucket name.
--
-- 'bucketOwner', 'verifiedAccessLogS3Destination_bucketOwner' - The Amazon Web Services account number that owns the bucket.
--
-- 'deliveryStatus', 'verifiedAccessLogS3Destination_deliveryStatus' - The delivery status.
--
-- 'enabled', 'verifiedAccessLogS3Destination_enabled' - Indicates whether logging is enabled.
--
-- 'prefix', 'verifiedAccessLogS3Destination_prefix' - The bucket prefix.
newVerifiedAccessLogS3Destination ::
  VerifiedAccessLogS3Destination
newVerifiedAccessLogS3Destination =
  VerifiedAccessLogS3Destination'
    { bucketName =
        Prelude.Nothing,
      bucketOwner = Prelude.Nothing,
      deliveryStatus = Prelude.Nothing,
      enabled = Prelude.Nothing,
      prefix = Prelude.Nothing
    }

-- | The bucket name.
verifiedAccessLogS3Destination_bucketName :: Lens.Lens' VerifiedAccessLogS3Destination (Prelude.Maybe Prelude.Text)
verifiedAccessLogS3Destination_bucketName = Lens.lens (\VerifiedAccessLogS3Destination' {bucketName} -> bucketName) (\s@VerifiedAccessLogS3Destination' {} a -> s {bucketName = a} :: VerifiedAccessLogS3Destination)

-- | The Amazon Web Services account number that owns the bucket.
verifiedAccessLogS3Destination_bucketOwner :: Lens.Lens' VerifiedAccessLogS3Destination (Prelude.Maybe Prelude.Text)
verifiedAccessLogS3Destination_bucketOwner = Lens.lens (\VerifiedAccessLogS3Destination' {bucketOwner} -> bucketOwner) (\s@VerifiedAccessLogS3Destination' {} a -> s {bucketOwner = a} :: VerifiedAccessLogS3Destination)

-- | The delivery status.
verifiedAccessLogS3Destination_deliveryStatus :: Lens.Lens' VerifiedAccessLogS3Destination (Prelude.Maybe VerifiedAccessLogDeliveryStatus)
verifiedAccessLogS3Destination_deliveryStatus = Lens.lens (\VerifiedAccessLogS3Destination' {deliveryStatus} -> deliveryStatus) (\s@VerifiedAccessLogS3Destination' {} a -> s {deliveryStatus = a} :: VerifiedAccessLogS3Destination)

-- | Indicates whether logging is enabled.
verifiedAccessLogS3Destination_enabled :: Lens.Lens' VerifiedAccessLogS3Destination (Prelude.Maybe Prelude.Bool)
verifiedAccessLogS3Destination_enabled = Lens.lens (\VerifiedAccessLogS3Destination' {enabled} -> enabled) (\s@VerifiedAccessLogS3Destination' {} a -> s {enabled = a} :: VerifiedAccessLogS3Destination)

-- | The bucket prefix.
verifiedAccessLogS3Destination_prefix :: Lens.Lens' VerifiedAccessLogS3Destination (Prelude.Maybe Prelude.Text)
verifiedAccessLogS3Destination_prefix = Lens.lens (\VerifiedAccessLogS3Destination' {prefix} -> prefix) (\s@VerifiedAccessLogS3Destination' {} a -> s {prefix = a} :: VerifiedAccessLogS3Destination)

instance Data.FromXML VerifiedAccessLogS3Destination where
  parseXML x =
    VerifiedAccessLogS3Destination'
      Prelude.<$> (x Data..@? "bucketName")
      Prelude.<*> (x Data..@? "bucketOwner")
      Prelude.<*> (x Data..@? "deliveryStatus")
      Prelude.<*> (x Data..@? "enabled")
      Prelude.<*> (x Data..@? "prefix")

instance
  Prelude.Hashable
    VerifiedAccessLogS3Destination
  where
  hashWithSalt
    _salt
    VerifiedAccessLogS3Destination' {..} =
      _salt
        `Prelude.hashWithSalt` bucketName
        `Prelude.hashWithSalt` bucketOwner
        `Prelude.hashWithSalt` deliveryStatus
        `Prelude.hashWithSalt` enabled
        `Prelude.hashWithSalt` prefix

instance
  Prelude.NFData
    VerifiedAccessLogS3Destination
  where
  rnf VerifiedAccessLogS3Destination' {..} =
    Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf bucketOwner
      `Prelude.seq` Prelude.rnf deliveryStatus
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf prefix
