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
-- Module      : Amazonka.IotTwinMaker.Types.ParentEntityUpdateRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.ParentEntityUpdateRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types.ParentEntityUpdateType
import qualified Amazonka.Prelude as Prelude

-- | The parent entity update request.
--
-- /See:/ 'newParentEntityUpdateRequest' smart constructor.
data ParentEntityUpdateRequest = ParentEntityUpdateRequest'
  { -- | The ID of the parent entity.
    parentEntityId :: Prelude.Maybe Prelude.Text,
    -- | The type of the update.
    updateType :: ParentEntityUpdateType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParentEntityUpdateRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parentEntityId', 'parentEntityUpdateRequest_parentEntityId' - The ID of the parent entity.
--
-- 'updateType', 'parentEntityUpdateRequest_updateType' - The type of the update.
newParentEntityUpdateRequest ::
  -- | 'updateType'
  ParentEntityUpdateType ->
  ParentEntityUpdateRequest
newParentEntityUpdateRequest pUpdateType_ =
  ParentEntityUpdateRequest'
    { parentEntityId =
        Prelude.Nothing,
      updateType = pUpdateType_
    }

-- | The ID of the parent entity.
parentEntityUpdateRequest_parentEntityId :: Lens.Lens' ParentEntityUpdateRequest (Prelude.Maybe Prelude.Text)
parentEntityUpdateRequest_parentEntityId = Lens.lens (\ParentEntityUpdateRequest' {parentEntityId} -> parentEntityId) (\s@ParentEntityUpdateRequest' {} a -> s {parentEntityId = a} :: ParentEntityUpdateRequest)

-- | The type of the update.
parentEntityUpdateRequest_updateType :: Lens.Lens' ParentEntityUpdateRequest ParentEntityUpdateType
parentEntityUpdateRequest_updateType = Lens.lens (\ParentEntityUpdateRequest' {updateType} -> updateType) (\s@ParentEntityUpdateRequest' {} a -> s {updateType = a} :: ParentEntityUpdateRequest)

instance Prelude.Hashable ParentEntityUpdateRequest where
  hashWithSalt _salt ParentEntityUpdateRequest' {..} =
    _salt
      `Prelude.hashWithSalt` parentEntityId
      `Prelude.hashWithSalt` updateType

instance Prelude.NFData ParentEntityUpdateRequest where
  rnf ParentEntityUpdateRequest' {..} =
    Prelude.rnf parentEntityId
      `Prelude.seq` Prelude.rnf updateType

instance Data.ToJSON ParentEntityUpdateRequest where
  toJSON ParentEntityUpdateRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("parentEntityId" Data..=)
              Prelude.<$> parentEntityId,
            Prelude.Just ("updateType" Data..= updateType)
          ]
      )
