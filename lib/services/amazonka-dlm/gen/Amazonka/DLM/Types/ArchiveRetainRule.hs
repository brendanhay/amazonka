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
-- Module      : Amazonka.DLM.Types.ArchiveRetainRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DLM.Types.ArchiveRetainRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DLM.Types.RetentionArchiveTier
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | __[Snapshot policies only]__ Specifies information about the archive
-- storage tier retention period.
--
-- /See:/ 'newArchiveRetainRule' smart constructor.
data ArchiveRetainRule = ArchiveRetainRule'
  { -- | Information about retention period in the Amazon EBS Snapshots Archive.
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/snapshot-archive.html Archive Amazon EBS snapshots>.
    retentionArchiveTier :: RetentionArchiveTier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ArchiveRetainRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'retentionArchiveTier', 'archiveRetainRule_retentionArchiveTier' - Information about retention period in the Amazon EBS Snapshots Archive.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/snapshot-archive.html Archive Amazon EBS snapshots>.
newArchiveRetainRule ::
  -- | 'retentionArchiveTier'
  RetentionArchiveTier ->
  ArchiveRetainRule
newArchiveRetainRule pRetentionArchiveTier_ =
  ArchiveRetainRule'
    { retentionArchiveTier =
        pRetentionArchiveTier_
    }

-- | Information about retention period in the Amazon EBS Snapshots Archive.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/snapshot-archive.html Archive Amazon EBS snapshots>.
archiveRetainRule_retentionArchiveTier :: Lens.Lens' ArchiveRetainRule RetentionArchiveTier
archiveRetainRule_retentionArchiveTier = Lens.lens (\ArchiveRetainRule' {retentionArchiveTier} -> retentionArchiveTier) (\s@ArchiveRetainRule' {} a -> s {retentionArchiveTier = a} :: ArchiveRetainRule)

instance Data.FromJSON ArchiveRetainRule where
  parseJSON =
    Data.withObject
      "ArchiveRetainRule"
      ( \x ->
          ArchiveRetainRule'
            Prelude.<$> (x Data..: "RetentionArchiveTier")
      )

instance Prelude.Hashable ArchiveRetainRule where
  hashWithSalt _salt ArchiveRetainRule' {..} =
    _salt `Prelude.hashWithSalt` retentionArchiveTier

instance Prelude.NFData ArchiveRetainRule where
  rnf ArchiveRetainRule' {..} =
    Prelude.rnf retentionArchiveTier

instance Data.ToJSON ArchiveRetainRule where
  toJSON ArchiveRetainRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "RetentionArchiveTier"
                  Data..= retentionArchiveTier
              )
          ]
      )
