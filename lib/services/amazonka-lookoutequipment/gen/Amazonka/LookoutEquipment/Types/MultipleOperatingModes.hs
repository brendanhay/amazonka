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
-- Module      : Amazonka.LookoutEquipment.Types.MultipleOperatingModes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.MultipleOperatingModes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LookoutEquipment.Types.StatisticalIssueStatus
import qualified Amazonka.Prelude as Prelude

-- | Entity that comprises information on operating modes in data.
--
-- /See:/ 'newMultipleOperatingModes' smart constructor.
data MultipleOperatingModes = MultipleOperatingModes'
  { -- | Indicates whether there is a potential data issue related to having
    -- multiple operating modes.
    status :: StatisticalIssueStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MultipleOperatingModes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'multipleOperatingModes_status' - Indicates whether there is a potential data issue related to having
-- multiple operating modes.
newMultipleOperatingModes ::
  -- | 'status'
  StatisticalIssueStatus ->
  MultipleOperatingModes
newMultipleOperatingModes pStatus_ =
  MultipleOperatingModes' {status = pStatus_}

-- | Indicates whether there is a potential data issue related to having
-- multiple operating modes.
multipleOperatingModes_status :: Lens.Lens' MultipleOperatingModes StatisticalIssueStatus
multipleOperatingModes_status = Lens.lens (\MultipleOperatingModes' {status} -> status) (\s@MultipleOperatingModes' {} a -> s {status = a} :: MultipleOperatingModes)

instance Core.FromJSON MultipleOperatingModes where
  parseJSON =
    Core.withObject
      "MultipleOperatingModes"
      ( \x ->
          MultipleOperatingModes'
            Prelude.<$> (x Core..: "Status")
      )

instance Prelude.Hashable MultipleOperatingModes where
  hashWithSalt _salt MultipleOperatingModes' {..} =
    _salt `Prelude.hashWithSalt` status

instance Prelude.NFData MultipleOperatingModes where
  rnf MultipleOperatingModes' {..} = Prelude.rnf status
