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
-- Module      : Amazonka.CloudDirectory.Types.BatchDetachTypedLink
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchDetachTypedLink where

import Amazonka.CloudDirectory.Types.TypedLinkSpecifier
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Detaches a typed link from a specified source and target object inside a
-- BatchRead operation. For more information, see DetachTypedLink and
-- BatchReadRequest$Operations.
--
-- /See:/ 'newBatchDetachTypedLink' smart constructor.
data BatchDetachTypedLink = BatchDetachTypedLink'
  { -- | Used to accept a typed link specifier as input.
    typedLinkSpecifier :: TypedLinkSpecifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDetachTypedLink' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'typedLinkSpecifier', 'batchDetachTypedLink_typedLinkSpecifier' - Used to accept a typed link specifier as input.
newBatchDetachTypedLink ::
  -- | 'typedLinkSpecifier'
  TypedLinkSpecifier ->
  BatchDetachTypedLink
newBatchDetachTypedLink pTypedLinkSpecifier_ =
  BatchDetachTypedLink'
    { typedLinkSpecifier =
        pTypedLinkSpecifier_
    }

-- | Used to accept a typed link specifier as input.
batchDetachTypedLink_typedLinkSpecifier :: Lens.Lens' BatchDetachTypedLink TypedLinkSpecifier
batchDetachTypedLink_typedLinkSpecifier = Lens.lens (\BatchDetachTypedLink' {typedLinkSpecifier} -> typedLinkSpecifier) (\s@BatchDetachTypedLink' {} a -> s {typedLinkSpecifier = a} :: BatchDetachTypedLink)

instance Prelude.Hashable BatchDetachTypedLink where
  hashWithSalt _salt BatchDetachTypedLink' {..} =
    _salt `Prelude.hashWithSalt` typedLinkSpecifier

instance Prelude.NFData BatchDetachTypedLink where
  rnf BatchDetachTypedLink' {..} =
    Prelude.rnf typedLinkSpecifier

instance Data.ToJSON BatchDetachTypedLink where
  toJSON BatchDetachTypedLink' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("TypedLinkSpecifier" Data..= typedLinkSpecifier)
          ]
      )
