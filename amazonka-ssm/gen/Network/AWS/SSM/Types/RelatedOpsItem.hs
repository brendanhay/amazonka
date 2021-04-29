{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An OpsItems that shares something in common with the current OpsItem.
-- For example, related OpsItems can include OpsItems with similar error
-- messages, impacted resources, or statuses for the impacted resource.
--
-- /See:/ 'newRelatedOpsItem' smart constructor.
data RelatedOpsItem = RelatedOpsItem'
  { -- | The ID of an OpsItem related to the current OpsItem.
    opsItemId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON RelatedOpsItem where
  parseJSON =
    Prelude.withObject
      "RelatedOpsItem"
      ( \x ->
          RelatedOpsItem'
            Prelude.<$> (x Prelude..: "OpsItemId")
      )

instance Prelude.Hashable RelatedOpsItem

instance Prelude.NFData RelatedOpsItem

instance Prelude.ToJSON RelatedOpsItem where
  toJSON RelatedOpsItem' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("OpsItemId" Prelude..= opsItemId)]
      )
