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
-- Module      : Amazonka.CostExplorer.Types.CostAllocationTagStatusEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.CostAllocationTagStatusEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.CostAllocationTagStatus
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The cost allocation tag status. The status of a key can either be active
-- or inactive.
--
-- /See:/ 'newCostAllocationTagStatusEntry' smart constructor.
data CostAllocationTagStatusEntry = CostAllocationTagStatusEntry'
  { -- | The key for the cost allocation tag.
    tagKey :: Prelude.Text,
    -- | The status of a cost allocation tag.
    status :: CostAllocationTagStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CostAllocationTagStatusEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagKey', 'costAllocationTagStatusEntry_tagKey' - The key for the cost allocation tag.
--
-- 'status', 'costAllocationTagStatusEntry_status' - The status of a cost allocation tag.
newCostAllocationTagStatusEntry ::
  -- | 'tagKey'
  Prelude.Text ->
  -- | 'status'
  CostAllocationTagStatus ->
  CostAllocationTagStatusEntry
newCostAllocationTagStatusEntry pTagKey_ pStatus_ =
  CostAllocationTagStatusEntry'
    { tagKey = pTagKey_,
      status = pStatus_
    }

-- | The key for the cost allocation tag.
costAllocationTagStatusEntry_tagKey :: Lens.Lens' CostAllocationTagStatusEntry Prelude.Text
costAllocationTagStatusEntry_tagKey = Lens.lens (\CostAllocationTagStatusEntry' {tagKey} -> tagKey) (\s@CostAllocationTagStatusEntry' {} a -> s {tagKey = a} :: CostAllocationTagStatusEntry)

-- | The status of a cost allocation tag.
costAllocationTagStatusEntry_status :: Lens.Lens' CostAllocationTagStatusEntry CostAllocationTagStatus
costAllocationTagStatusEntry_status = Lens.lens (\CostAllocationTagStatusEntry' {status} -> status) (\s@CostAllocationTagStatusEntry' {} a -> s {status = a} :: CostAllocationTagStatusEntry)

instance
  Prelude.Hashable
    CostAllocationTagStatusEntry
  where
  hashWithSalt _salt CostAllocationTagStatusEntry' {..} =
    _salt `Prelude.hashWithSalt` tagKey
      `Prelude.hashWithSalt` status

instance Prelude.NFData CostAllocationTagStatusEntry where
  rnf CostAllocationTagStatusEntry' {..} =
    Prelude.rnf tagKey `Prelude.seq` Prelude.rnf status

instance Data.ToJSON CostAllocationTagStatusEntry where
  toJSON CostAllocationTagStatusEntry' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("TagKey" Data..= tagKey),
            Prelude.Just ("Status" Data..= status)
          ]
      )
