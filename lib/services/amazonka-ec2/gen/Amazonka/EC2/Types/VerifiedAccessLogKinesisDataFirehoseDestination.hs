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
-- Module      : Amazonka.EC2.Types.VerifiedAccessLogKinesisDataFirehoseDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VerifiedAccessLogKinesisDataFirehoseDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.VerifiedAccessLogDeliveryStatus
import qualified Amazonka.Prelude as Prelude

-- | Options for Kinesis as a logging destination.
--
-- /See:/ 'newVerifiedAccessLogKinesisDataFirehoseDestination' smart constructor.
data VerifiedAccessLogKinesisDataFirehoseDestination = VerifiedAccessLogKinesisDataFirehoseDestination'
  { -- | The delivery status.
    deliveryStatus :: Prelude.Maybe VerifiedAccessLogDeliveryStatus,
    -- | The ID of the delivery stream.
    deliveryStream :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether logging is enabled.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifiedAccessLogKinesisDataFirehoseDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deliveryStatus', 'verifiedAccessLogKinesisDataFirehoseDestination_deliveryStatus' - The delivery status.
--
-- 'deliveryStream', 'verifiedAccessLogKinesisDataFirehoseDestination_deliveryStream' - The ID of the delivery stream.
--
-- 'enabled', 'verifiedAccessLogKinesisDataFirehoseDestination_enabled' - Indicates whether logging is enabled.
newVerifiedAccessLogKinesisDataFirehoseDestination ::
  VerifiedAccessLogKinesisDataFirehoseDestination
newVerifiedAccessLogKinesisDataFirehoseDestination =
  VerifiedAccessLogKinesisDataFirehoseDestination'
    { deliveryStatus =
        Prelude.Nothing,
      deliveryStream =
        Prelude.Nothing,
      enabled = Prelude.Nothing
    }

-- | The delivery status.
verifiedAccessLogKinesisDataFirehoseDestination_deliveryStatus :: Lens.Lens' VerifiedAccessLogKinesisDataFirehoseDestination (Prelude.Maybe VerifiedAccessLogDeliveryStatus)
verifiedAccessLogKinesisDataFirehoseDestination_deliveryStatus = Lens.lens (\VerifiedAccessLogKinesisDataFirehoseDestination' {deliveryStatus} -> deliveryStatus) (\s@VerifiedAccessLogKinesisDataFirehoseDestination' {} a -> s {deliveryStatus = a} :: VerifiedAccessLogKinesisDataFirehoseDestination)

-- | The ID of the delivery stream.
verifiedAccessLogKinesisDataFirehoseDestination_deliveryStream :: Lens.Lens' VerifiedAccessLogKinesisDataFirehoseDestination (Prelude.Maybe Prelude.Text)
verifiedAccessLogKinesisDataFirehoseDestination_deliveryStream = Lens.lens (\VerifiedAccessLogKinesisDataFirehoseDestination' {deliveryStream} -> deliveryStream) (\s@VerifiedAccessLogKinesisDataFirehoseDestination' {} a -> s {deliveryStream = a} :: VerifiedAccessLogKinesisDataFirehoseDestination)

-- | Indicates whether logging is enabled.
verifiedAccessLogKinesisDataFirehoseDestination_enabled :: Lens.Lens' VerifiedAccessLogKinesisDataFirehoseDestination (Prelude.Maybe Prelude.Bool)
verifiedAccessLogKinesisDataFirehoseDestination_enabled = Lens.lens (\VerifiedAccessLogKinesisDataFirehoseDestination' {enabled} -> enabled) (\s@VerifiedAccessLogKinesisDataFirehoseDestination' {} a -> s {enabled = a} :: VerifiedAccessLogKinesisDataFirehoseDestination)

instance
  Data.FromXML
    VerifiedAccessLogKinesisDataFirehoseDestination
  where
  parseXML x =
    VerifiedAccessLogKinesisDataFirehoseDestination'
      Prelude.<$> (x Data..@? "deliveryStatus")
        Prelude.<*> (x Data..@? "deliveryStream")
        Prelude.<*> (x Data..@? "enabled")

instance
  Prelude.Hashable
    VerifiedAccessLogKinesisDataFirehoseDestination
  where
  hashWithSalt
    _salt
    VerifiedAccessLogKinesisDataFirehoseDestination' {..} =
      _salt `Prelude.hashWithSalt` deliveryStatus
        `Prelude.hashWithSalt` deliveryStream
        `Prelude.hashWithSalt` enabled

instance
  Prelude.NFData
    VerifiedAccessLogKinesisDataFirehoseDestination
  where
  rnf
    VerifiedAccessLogKinesisDataFirehoseDestination' {..} =
      Prelude.rnf deliveryStatus
        `Prelude.seq` Prelude.rnf deliveryStream
        `Prelude.seq` Prelude.rnf enabled
