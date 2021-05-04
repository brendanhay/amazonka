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
-- Module      : Network.AWS.CloudDirectory.Types.BatchUpdateLinkAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchUpdateLinkAttributes where

import Network.AWS.CloudDirectory.Types.LinkAttributeUpdate
import Network.AWS.CloudDirectory.Types.TypedLinkSpecifier
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
batchUpdateLinkAttributes_attributeUpdates = Lens.lens (\BatchUpdateLinkAttributes' {attributeUpdates} -> attributeUpdates) (\s@BatchUpdateLinkAttributes' {} a -> s {attributeUpdates = a} :: BatchUpdateLinkAttributes) Prelude.. Prelude._Coerce

instance Prelude.Hashable BatchUpdateLinkAttributes

instance Prelude.NFData BatchUpdateLinkAttributes

instance Prelude.ToJSON BatchUpdateLinkAttributes where
  toJSON BatchUpdateLinkAttributes' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("TypedLinkSpecifier" Prelude..= typedLinkSpecifier),
            Prelude.Just
              ("AttributeUpdates" Prelude..= attributeUpdates)
          ]
      )
