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
-- Module      : Amazonka.IVSChat.Types.FirehoseDestinationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVSChat.Types.FirehoseDestinationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a Kinesis Firehose location where chat logs will be stored.
--
-- /See:/ 'newFirehoseDestinationConfiguration' smart constructor.
data FirehoseDestinationConfiguration = FirehoseDestinationConfiguration'
  { -- | Name of the Amazon Kinesis Firehose delivery stream where chat activity
    -- will be logged.
    deliveryStreamName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FirehoseDestinationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deliveryStreamName', 'firehoseDestinationConfiguration_deliveryStreamName' - Name of the Amazon Kinesis Firehose delivery stream where chat activity
-- will be logged.
newFirehoseDestinationConfiguration ::
  -- | 'deliveryStreamName'
  Prelude.Text ->
  FirehoseDestinationConfiguration
newFirehoseDestinationConfiguration
  pDeliveryStreamName_ =
    FirehoseDestinationConfiguration'
      { deliveryStreamName =
          pDeliveryStreamName_
      }

-- | Name of the Amazon Kinesis Firehose delivery stream where chat activity
-- will be logged.
firehoseDestinationConfiguration_deliveryStreamName :: Lens.Lens' FirehoseDestinationConfiguration Prelude.Text
firehoseDestinationConfiguration_deliveryStreamName = Lens.lens (\FirehoseDestinationConfiguration' {deliveryStreamName} -> deliveryStreamName) (\s@FirehoseDestinationConfiguration' {} a -> s {deliveryStreamName = a} :: FirehoseDestinationConfiguration)

instance
  Data.FromJSON
    FirehoseDestinationConfiguration
  where
  parseJSON =
    Data.withObject
      "FirehoseDestinationConfiguration"
      ( \x ->
          FirehoseDestinationConfiguration'
            Prelude.<$> (x Data..: "deliveryStreamName")
      )

instance
  Prelude.Hashable
    FirehoseDestinationConfiguration
  where
  hashWithSalt
    _salt
    FirehoseDestinationConfiguration' {..} =
      _salt `Prelude.hashWithSalt` deliveryStreamName

instance
  Prelude.NFData
    FirehoseDestinationConfiguration
  where
  rnf FirehoseDestinationConfiguration' {..} =
    Prelude.rnf deliveryStreamName

instance Data.ToJSON FirehoseDestinationConfiguration where
  toJSON FirehoseDestinationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("deliveryStreamName" Data..= deliveryStreamName)
          ]
      )
