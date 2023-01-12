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
-- Module      : Amazonka.DLM.Types.ArchiveRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DLM.Types.ArchiveRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DLM.Types.ArchiveRetainRule
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | __[Snapshot policies only]__ Specifies a snapshot archiving rule for a
-- schedule.
--
-- /See:/ 'newArchiveRule' smart constructor.
data ArchiveRule = ArchiveRule'
  { -- | Information about the retention period for the snapshot archiving rule.
    retainRule :: ArchiveRetainRule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ArchiveRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'retainRule', 'archiveRule_retainRule' - Information about the retention period for the snapshot archiving rule.
newArchiveRule ::
  -- | 'retainRule'
  ArchiveRetainRule ->
  ArchiveRule
newArchiveRule pRetainRule_ =
  ArchiveRule' {retainRule = pRetainRule_}

-- | Information about the retention period for the snapshot archiving rule.
archiveRule_retainRule :: Lens.Lens' ArchiveRule ArchiveRetainRule
archiveRule_retainRule = Lens.lens (\ArchiveRule' {retainRule} -> retainRule) (\s@ArchiveRule' {} a -> s {retainRule = a} :: ArchiveRule)

instance Data.FromJSON ArchiveRule where
  parseJSON =
    Data.withObject
      "ArchiveRule"
      ( \x ->
          ArchiveRule' Prelude.<$> (x Data..: "RetainRule")
      )

instance Prelude.Hashable ArchiveRule where
  hashWithSalt _salt ArchiveRule' {..} =
    _salt `Prelude.hashWithSalt` retainRule

instance Prelude.NFData ArchiveRule where
  rnf ArchiveRule' {..} = Prelude.rnf retainRule

instance Data.ToJSON ArchiveRule where
  toJSON ArchiveRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("RetainRule" Data..= retainRule)]
      )
