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
-- Module      : Amazonka.CloudDirectory.Types.BatchGetLinkAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchGetLinkAttributes where

import Amazonka.CloudDirectory.Types.TypedLinkSpecifier
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Retrieves attributes that are associated with a typed link inside a
-- BatchRead operation. For more information, see GetLinkAttributes and
-- BatchReadRequest$Operations.
--
-- /See:/ 'newBatchGetLinkAttributes' smart constructor.
data BatchGetLinkAttributes = BatchGetLinkAttributes'
  { -- | Allows a typed link specifier to be accepted as input.
    typedLinkSpecifier :: TypedLinkSpecifier,
    -- | A list of attribute names whose values will be retrieved.
    attributeNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetLinkAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'typedLinkSpecifier', 'batchGetLinkAttributes_typedLinkSpecifier' - Allows a typed link specifier to be accepted as input.
--
-- 'attributeNames', 'batchGetLinkAttributes_attributeNames' - A list of attribute names whose values will be retrieved.
newBatchGetLinkAttributes ::
  -- | 'typedLinkSpecifier'
  TypedLinkSpecifier ->
  BatchGetLinkAttributes
newBatchGetLinkAttributes pTypedLinkSpecifier_ =
  BatchGetLinkAttributes'
    { typedLinkSpecifier =
        pTypedLinkSpecifier_,
      attributeNames = Prelude.mempty
    }

-- | Allows a typed link specifier to be accepted as input.
batchGetLinkAttributes_typedLinkSpecifier :: Lens.Lens' BatchGetLinkAttributes TypedLinkSpecifier
batchGetLinkAttributes_typedLinkSpecifier = Lens.lens (\BatchGetLinkAttributes' {typedLinkSpecifier} -> typedLinkSpecifier) (\s@BatchGetLinkAttributes' {} a -> s {typedLinkSpecifier = a} :: BatchGetLinkAttributes)

-- | A list of attribute names whose values will be retrieved.
batchGetLinkAttributes_attributeNames :: Lens.Lens' BatchGetLinkAttributes [Prelude.Text]
batchGetLinkAttributes_attributeNames = Lens.lens (\BatchGetLinkAttributes' {attributeNames} -> attributeNames) (\s@BatchGetLinkAttributes' {} a -> s {attributeNames = a} :: BatchGetLinkAttributes) Prelude.. Lens.coerced

instance Prelude.Hashable BatchGetLinkAttributes where
  hashWithSalt _salt BatchGetLinkAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` typedLinkSpecifier
      `Prelude.hashWithSalt` attributeNames

instance Prelude.NFData BatchGetLinkAttributes where
  rnf BatchGetLinkAttributes' {..} =
    Prelude.rnf typedLinkSpecifier
      `Prelude.seq` Prelude.rnf attributeNames

instance Data.ToJSON BatchGetLinkAttributes where
  toJSON BatchGetLinkAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("TypedLinkSpecifier" Data..= typedLinkSpecifier),
            Prelude.Just
              ("AttributeNames" Data..= attributeNames)
          ]
      )
