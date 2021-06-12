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
-- Module      : Network.AWS.SSM.Types.RelatedOpsItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.RelatedOpsItem where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An OpsItems that shares something in common with the current OpsItem.
-- For example, related OpsItems can include OpsItems with similar error
-- messages, impacted resources, or statuses for the impacted resource.
--
-- /See:/ 'newRelatedOpsItem' smart constructor.
data RelatedOpsItem = RelatedOpsItem'
  { -- | The ID of an OpsItem related to the current OpsItem.
    opsItemId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  RelatedOpsItem
newRelatedOpsItem pOpsItemId_ =
  RelatedOpsItem' {opsItemId = pOpsItemId_}

-- | The ID of an OpsItem related to the current OpsItem.
relatedOpsItem_opsItemId :: Lens.Lens' RelatedOpsItem Core.Text
relatedOpsItem_opsItemId = Lens.lens (\RelatedOpsItem' {opsItemId} -> opsItemId) (\s@RelatedOpsItem' {} a -> s {opsItemId = a} :: RelatedOpsItem)

instance Core.FromJSON RelatedOpsItem where
  parseJSON =
    Core.withObject
      "RelatedOpsItem"
      ( \x ->
          RelatedOpsItem' Core.<$> (x Core..: "OpsItemId")
      )

instance Core.Hashable RelatedOpsItem

instance Core.NFData RelatedOpsItem

instance Core.ToJSON RelatedOpsItem where
  toJSON RelatedOpsItem' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("OpsItemId" Core..= opsItemId)]
      )
