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
-- Module      : Amazonka.CloudDirectory.Types.BatchUpdateLinkAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchUpdateLinkAttributes where

import Amazonka.CloudDirectory.Types.LinkAttributeUpdate
import Amazonka.CloudDirectory.Types.TypedLinkSpecifier
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Updates a given typed link’s attributes inside a BatchRead operation.
-- Attributes to be updated must not contribute to the typed link’s
-- identity, as defined by its @IdentityAttributeOrder@. For more
-- information, see UpdateLinkAttributes and BatchReadRequest$Operations.
--
-- /See:/ 'newBatchUpdateLinkAttributes' smart constructor.
data BatchUpdateLinkAttributes = BatchUpdateLinkAttributes'
  { -- | Allows a typed link specifier to be accepted as input.
    typedLinkSpecifier :: TypedLinkSpecifier,
    -- | The attributes update structure.
    attributeUpdates :: [LinkAttributeUpdate]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchUpdateLinkAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'typedLinkSpecifier', 'batchUpdateLinkAttributes_typedLinkSpecifier' - Allows a typed link specifier to be accepted as input.
--
-- 'attributeUpdates', 'batchUpdateLinkAttributes_attributeUpdates' - The attributes update structure.
newBatchUpdateLinkAttributes ::
  -- | 'typedLinkSpecifier'
  TypedLinkSpecifier ->
  BatchUpdateLinkAttributes
newBatchUpdateLinkAttributes pTypedLinkSpecifier_ =
  BatchUpdateLinkAttributes'
    { typedLinkSpecifier =
        pTypedLinkSpecifier_,
      attributeUpdates = Prelude.mempty
    }

-- | Allows a typed link specifier to be accepted as input.
batchUpdateLinkAttributes_typedLinkSpecifier :: Lens.Lens' BatchUpdateLinkAttributes TypedLinkSpecifier
batchUpdateLinkAttributes_typedLinkSpecifier = Lens.lens (\BatchUpdateLinkAttributes' {typedLinkSpecifier} -> typedLinkSpecifier) (\s@BatchUpdateLinkAttributes' {} a -> s {typedLinkSpecifier = a} :: BatchUpdateLinkAttributes)

-- | The attributes update structure.
batchUpdateLinkAttributes_attributeUpdates :: Lens.Lens' BatchUpdateLinkAttributes [LinkAttributeUpdate]
batchUpdateLinkAttributes_attributeUpdates = Lens.lens (\BatchUpdateLinkAttributes' {attributeUpdates} -> attributeUpdates) (\s@BatchUpdateLinkAttributes' {} a -> s {attributeUpdates = a} :: BatchUpdateLinkAttributes) Prelude.. Lens.coerced

instance Prelude.Hashable BatchUpdateLinkAttributes where
  hashWithSalt _salt BatchUpdateLinkAttributes' {..} =
    _salt `Prelude.hashWithSalt` typedLinkSpecifier
      `Prelude.hashWithSalt` attributeUpdates

instance Prelude.NFData BatchUpdateLinkAttributes where
  rnf BatchUpdateLinkAttributes' {..} =
    Prelude.rnf typedLinkSpecifier
      `Prelude.seq` Prelude.rnf attributeUpdates

instance Data.ToJSON BatchUpdateLinkAttributes where
  toJSON BatchUpdateLinkAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("TypedLinkSpecifier" Data..= typedLinkSpecifier),
            Prelude.Just
              ("AttributeUpdates" Data..= attributeUpdates)
          ]
      )
