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
-- Module      : Amazonka.ElastiCache.Types.KinesisFirehoseDestinationDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.KinesisFirehoseDestinationDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The configuration details of the Kinesis Data Firehose destination.
--
-- /See:/ 'newKinesisFirehoseDestinationDetails' smart constructor.
data KinesisFirehoseDestinationDetails = KinesisFirehoseDestinationDetails'
  { -- | The name of the Kinesis Data Firehose delivery stream.
    deliveryStream :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KinesisFirehoseDestinationDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deliveryStream', 'kinesisFirehoseDestinationDetails_deliveryStream' - The name of the Kinesis Data Firehose delivery stream.
newKinesisFirehoseDestinationDetails ::
  KinesisFirehoseDestinationDetails
newKinesisFirehoseDestinationDetails =
  KinesisFirehoseDestinationDetails'
    { deliveryStream =
        Prelude.Nothing
    }

-- | The name of the Kinesis Data Firehose delivery stream.
kinesisFirehoseDestinationDetails_deliveryStream :: Lens.Lens' KinesisFirehoseDestinationDetails (Prelude.Maybe Prelude.Text)
kinesisFirehoseDestinationDetails_deliveryStream = Lens.lens (\KinesisFirehoseDestinationDetails' {deliveryStream} -> deliveryStream) (\s@KinesisFirehoseDestinationDetails' {} a -> s {deliveryStream = a} :: KinesisFirehoseDestinationDetails)

instance
  Core.FromXML
    KinesisFirehoseDestinationDetails
  where
  parseXML x =
    KinesisFirehoseDestinationDetails'
      Prelude.<$> (x Core..@? "DeliveryStream")

instance
  Prelude.Hashable
    KinesisFirehoseDestinationDetails
  where
  hashWithSalt
    _salt
    KinesisFirehoseDestinationDetails' {..} =
      _salt `Prelude.hashWithSalt` deliveryStream

instance
  Prelude.NFData
    KinesisFirehoseDestinationDetails
  where
  rnf KinesisFirehoseDestinationDetails' {..} =
    Prelude.rnf deliveryStream

instance
  Core.ToQuery
    KinesisFirehoseDestinationDetails
  where
  toQuery KinesisFirehoseDestinationDetails' {..} =
    Prelude.mconcat
      ["DeliveryStream" Core.=: deliveryStream]
