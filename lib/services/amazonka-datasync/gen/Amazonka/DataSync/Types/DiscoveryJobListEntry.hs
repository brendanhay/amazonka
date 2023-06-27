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
-- Module      : Amazonka.DataSync.Types.DiscoveryJobListEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.DiscoveryJobListEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types.DiscoveryJobStatus
import qualified Amazonka.Prelude as Prelude

-- | The details about a specific DataSync discovery job.
--
-- /See:/ 'newDiscoveryJobListEntry' smart constructor.
data DiscoveryJobListEntry = DiscoveryJobListEntry'
  { -- | The Amazon Resource Name (ARN) of a discovery job.
    discoveryJobArn :: Prelude.Maybe Prelude.Text,
    -- | The status of a discovery job. For more information, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-job-statuses.html#discovery-job-statuses-table Discovery job statuses>.
    status :: Prelude.Maybe DiscoveryJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DiscoveryJobListEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'discoveryJobArn', 'discoveryJobListEntry_discoveryJobArn' - The Amazon Resource Name (ARN) of a discovery job.
--
-- 'status', 'discoveryJobListEntry_status' - The status of a discovery job. For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-job-statuses.html#discovery-job-statuses-table Discovery job statuses>.
newDiscoveryJobListEntry ::
  DiscoveryJobListEntry
newDiscoveryJobListEntry =
  DiscoveryJobListEntry'
    { discoveryJobArn =
        Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of a discovery job.
discoveryJobListEntry_discoveryJobArn :: Lens.Lens' DiscoveryJobListEntry (Prelude.Maybe Prelude.Text)
discoveryJobListEntry_discoveryJobArn = Lens.lens (\DiscoveryJobListEntry' {discoveryJobArn} -> discoveryJobArn) (\s@DiscoveryJobListEntry' {} a -> s {discoveryJobArn = a} :: DiscoveryJobListEntry)

-- | The status of a discovery job. For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-job-statuses.html#discovery-job-statuses-table Discovery job statuses>.
discoveryJobListEntry_status :: Lens.Lens' DiscoveryJobListEntry (Prelude.Maybe DiscoveryJobStatus)
discoveryJobListEntry_status = Lens.lens (\DiscoveryJobListEntry' {status} -> status) (\s@DiscoveryJobListEntry' {} a -> s {status = a} :: DiscoveryJobListEntry)

instance Data.FromJSON DiscoveryJobListEntry where
  parseJSON =
    Data.withObject
      "DiscoveryJobListEntry"
      ( \x ->
          DiscoveryJobListEntry'
            Prelude.<$> (x Data..:? "DiscoveryJobArn")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable DiscoveryJobListEntry where
  hashWithSalt _salt DiscoveryJobListEntry' {..} =
    _salt
      `Prelude.hashWithSalt` discoveryJobArn
      `Prelude.hashWithSalt` status

instance Prelude.NFData DiscoveryJobListEntry where
  rnf DiscoveryJobListEntry' {..} =
    Prelude.rnf discoveryJobArn
      `Prelude.seq` Prelude.rnf status
