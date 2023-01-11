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
-- Module      : Amazonka.EC2.Types.VerifiedAccessLogKinesisDataFirehoseDestinationOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VerifiedAccessLogKinesisDataFirehoseDestinationOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes Amazon Kinesis Data Firehose logging options.
--
-- /See:/ 'newVerifiedAccessLogKinesisDataFirehoseDestinationOptions' smart constructor.
data VerifiedAccessLogKinesisDataFirehoseDestinationOptions = VerifiedAccessLogKinesisDataFirehoseDestinationOptions'
  { -- | The ID of the delivery stream.
    deliveryStream :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether logging is enabled.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifiedAccessLogKinesisDataFirehoseDestinationOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deliveryStream', 'verifiedAccessLogKinesisDataFirehoseDestinationOptions_deliveryStream' - The ID of the delivery stream.
--
-- 'enabled', 'verifiedAccessLogKinesisDataFirehoseDestinationOptions_enabled' - Indicates whether logging is enabled.
newVerifiedAccessLogKinesisDataFirehoseDestinationOptions ::
  -- | 'enabled'
  Prelude.Bool ->
  VerifiedAccessLogKinesisDataFirehoseDestinationOptions
newVerifiedAccessLogKinesisDataFirehoseDestinationOptions
  pEnabled_ =
    VerifiedAccessLogKinesisDataFirehoseDestinationOptions'
      { deliveryStream =
          Prelude.Nothing,
        enabled = pEnabled_
      }

-- | The ID of the delivery stream.
verifiedAccessLogKinesisDataFirehoseDestinationOptions_deliveryStream :: Lens.Lens' VerifiedAccessLogKinesisDataFirehoseDestinationOptions (Prelude.Maybe Prelude.Text)
verifiedAccessLogKinesisDataFirehoseDestinationOptions_deliveryStream = Lens.lens (\VerifiedAccessLogKinesisDataFirehoseDestinationOptions' {deliveryStream} -> deliveryStream) (\s@VerifiedAccessLogKinesisDataFirehoseDestinationOptions' {} a -> s {deliveryStream = a} :: VerifiedAccessLogKinesisDataFirehoseDestinationOptions)

-- | Indicates whether logging is enabled.
verifiedAccessLogKinesisDataFirehoseDestinationOptions_enabled :: Lens.Lens' VerifiedAccessLogKinesisDataFirehoseDestinationOptions Prelude.Bool
verifiedAccessLogKinesisDataFirehoseDestinationOptions_enabled = Lens.lens (\VerifiedAccessLogKinesisDataFirehoseDestinationOptions' {enabled} -> enabled) (\s@VerifiedAccessLogKinesisDataFirehoseDestinationOptions' {} a -> s {enabled = a} :: VerifiedAccessLogKinesisDataFirehoseDestinationOptions)

instance
  Prelude.Hashable
    VerifiedAccessLogKinesisDataFirehoseDestinationOptions
  where
  hashWithSalt
    _salt
    VerifiedAccessLogKinesisDataFirehoseDestinationOptions' {..} =
      _salt `Prelude.hashWithSalt` deliveryStream
        `Prelude.hashWithSalt` enabled

instance
  Prelude.NFData
    VerifiedAccessLogKinesisDataFirehoseDestinationOptions
  where
  rnf
    VerifiedAccessLogKinesisDataFirehoseDestinationOptions' {..} =
      Prelude.rnf deliveryStream
        `Prelude.seq` Prelude.rnf enabled

instance
  Data.ToQuery
    VerifiedAccessLogKinesisDataFirehoseDestinationOptions
  where
  toQuery
    VerifiedAccessLogKinesisDataFirehoseDestinationOptions' {..} =
      Prelude.mconcat
        [ "DeliveryStream" Data.=: deliveryStream,
          "Enabled" Data.=: enabled
        ]
