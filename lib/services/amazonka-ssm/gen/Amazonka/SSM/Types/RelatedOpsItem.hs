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
-- Module      : Amazonka.SSM.Types.RelatedOpsItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.RelatedOpsItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An OpsItems that shares something in common with the current OpsItem.
-- For example, related OpsItems can include OpsItems with similar error
-- messages, impacted resources, or statuses for the impacted resource.
--
-- /See:/ 'newRelatedOpsItem' smart constructor.
data RelatedOpsItem = RelatedOpsItem'
  { -- | The ID of an OpsItem related to the current OpsItem.
    opsItemId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RelatedOpsItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'opsItemId', 'relatedOpsItem_opsItemId' - The ID of an OpsItem related to the current OpsItem.
newRelatedOpsItem ::
  -- | 'opsItemId'
  Prelude.Text ->
  RelatedOpsItem
newRelatedOpsItem pOpsItemId_ =
  RelatedOpsItem' {opsItemId = pOpsItemId_}

-- | The ID of an OpsItem related to the current OpsItem.
relatedOpsItem_opsItemId :: Lens.Lens' RelatedOpsItem Prelude.Text
relatedOpsItem_opsItemId = Lens.lens (\RelatedOpsItem' {opsItemId} -> opsItemId) (\s@RelatedOpsItem' {} a -> s {opsItemId = a} :: RelatedOpsItem)

instance Data.FromJSON RelatedOpsItem where
  parseJSON =
    Data.withObject
      "RelatedOpsItem"
      ( \x ->
          RelatedOpsItem' Prelude.<$> (x Data..: "OpsItemId")
      )

instance Prelude.Hashable RelatedOpsItem where
  hashWithSalt _salt RelatedOpsItem' {..} =
    _salt `Prelude.hashWithSalt` opsItemId

instance Prelude.NFData RelatedOpsItem where
  rnf RelatedOpsItem' {..} = Prelude.rnf opsItemId

instance Data.ToJSON RelatedOpsItem where
  toJSON RelatedOpsItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("OpsItemId" Data..= opsItemId)]
      )
