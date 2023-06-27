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
-- Module      : Amazonka.CustomerProfiles.Types.DestinationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.DestinationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types.EventStreamDestinationStatus
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary information about the Kinesis data stream
--
-- /See:/ 'newDestinationSummary' smart constructor.
data DestinationSummary = DestinationSummary'
  { -- | The timestamp when the status last changed to @UNHEALHY@.
    unhealthySince :: Prelude.Maybe Data.POSIX,
    -- | The StreamARN of the destination to deliver profile events to. For
    -- example, arn:aws:kinesis:region:account-id:stream\/stream-name.
    uri :: Prelude.Text,
    -- | The status of enabling the Kinesis stream as a destination for export.
    status :: EventStreamDestinationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DestinationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unhealthySince', 'destinationSummary_unhealthySince' - The timestamp when the status last changed to @UNHEALHY@.
--
-- 'uri', 'destinationSummary_uri' - The StreamARN of the destination to deliver profile events to. For
-- example, arn:aws:kinesis:region:account-id:stream\/stream-name.
--
-- 'status', 'destinationSummary_status' - The status of enabling the Kinesis stream as a destination for export.
newDestinationSummary ::
  -- | 'uri'
  Prelude.Text ->
  -- | 'status'
  EventStreamDestinationStatus ->
  DestinationSummary
newDestinationSummary pUri_ pStatus_ =
  DestinationSummary'
    { unhealthySince =
        Prelude.Nothing,
      uri = pUri_,
      status = pStatus_
    }

-- | The timestamp when the status last changed to @UNHEALHY@.
destinationSummary_unhealthySince :: Lens.Lens' DestinationSummary (Prelude.Maybe Prelude.UTCTime)
destinationSummary_unhealthySince = Lens.lens (\DestinationSummary' {unhealthySince} -> unhealthySince) (\s@DestinationSummary' {} a -> s {unhealthySince = a} :: DestinationSummary) Prelude.. Lens.mapping Data._Time

-- | The StreamARN of the destination to deliver profile events to. For
-- example, arn:aws:kinesis:region:account-id:stream\/stream-name.
destinationSummary_uri :: Lens.Lens' DestinationSummary Prelude.Text
destinationSummary_uri = Lens.lens (\DestinationSummary' {uri} -> uri) (\s@DestinationSummary' {} a -> s {uri = a} :: DestinationSummary)

-- | The status of enabling the Kinesis stream as a destination for export.
destinationSummary_status :: Lens.Lens' DestinationSummary EventStreamDestinationStatus
destinationSummary_status = Lens.lens (\DestinationSummary' {status} -> status) (\s@DestinationSummary' {} a -> s {status = a} :: DestinationSummary)

instance Data.FromJSON DestinationSummary where
  parseJSON =
    Data.withObject
      "DestinationSummary"
      ( \x ->
          DestinationSummary'
            Prelude.<$> (x Data..:? "UnhealthySince")
            Prelude.<*> (x Data..: "Uri")
            Prelude.<*> (x Data..: "Status")
      )

instance Prelude.Hashable DestinationSummary where
  hashWithSalt _salt DestinationSummary' {..} =
    _salt
      `Prelude.hashWithSalt` unhealthySince
      `Prelude.hashWithSalt` uri
      `Prelude.hashWithSalt` status

instance Prelude.NFData DestinationSummary where
  rnf DestinationSummary' {..} =
    Prelude.rnf unhealthySince
      `Prelude.seq` Prelude.rnf uri
      `Prelude.seq` Prelude.rnf status
