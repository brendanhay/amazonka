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
-- Module      : Amazonka.IdentityStore.Types.AlternateIdentifier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IdentityStore.Types.AlternateIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IdentityStore.Types.ExternalId
import Amazonka.IdentityStore.Types.UniqueAttribute
import qualified Amazonka.Prelude as Prelude

-- | A unique identifier for a user or group that is not the primary
-- identifier. This value can be an identifier from an external identity
-- provider (IdP) that is associated with the user, the group, or a unique
-- attribute. For example, a unique @GroupDisplayName@.
--
-- /See:/ 'newAlternateIdentifier' smart constructor.
data AlternateIdentifier = AlternateIdentifier'
  { -- | The identifier issued to this resource by an external identity provider.
    externalId :: Prelude.Maybe ExternalId,
    -- | An entity attribute that\'s unique to a specific entity.
    uniqueAttribute :: Prelude.Maybe UniqueAttribute
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AlternateIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'externalId', 'alternateIdentifier_externalId' - The identifier issued to this resource by an external identity provider.
--
-- 'uniqueAttribute', 'alternateIdentifier_uniqueAttribute' - An entity attribute that\'s unique to a specific entity.
newAlternateIdentifier ::
  AlternateIdentifier
newAlternateIdentifier =
  AlternateIdentifier'
    { externalId = Prelude.Nothing,
      uniqueAttribute = Prelude.Nothing
    }

-- | The identifier issued to this resource by an external identity provider.
alternateIdentifier_externalId :: Lens.Lens' AlternateIdentifier (Prelude.Maybe ExternalId)
alternateIdentifier_externalId = Lens.lens (\AlternateIdentifier' {externalId} -> externalId) (\s@AlternateIdentifier' {} a -> s {externalId = a} :: AlternateIdentifier)

-- | An entity attribute that\'s unique to a specific entity.
alternateIdentifier_uniqueAttribute :: Lens.Lens' AlternateIdentifier (Prelude.Maybe UniqueAttribute)
alternateIdentifier_uniqueAttribute = Lens.lens (\AlternateIdentifier' {uniqueAttribute} -> uniqueAttribute) (\s@AlternateIdentifier' {} a -> s {uniqueAttribute = a} :: AlternateIdentifier)

instance Prelude.Hashable AlternateIdentifier where
  hashWithSalt _salt AlternateIdentifier' {..} =
    _salt
      `Prelude.hashWithSalt` externalId
      `Prelude.hashWithSalt` uniqueAttribute

instance Prelude.NFData AlternateIdentifier where
  rnf AlternateIdentifier' {..} =
    Prelude.rnf externalId
      `Prelude.seq` Prelude.rnf uniqueAttribute

instance Data.ToJSON AlternateIdentifier where
  toJSON AlternateIdentifier' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExternalId" Data..=) Prelude.<$> externalId,
            ("UniqueAttribute" Data..=)
              Prelude.<$> uniqueAttribute
          ]
      )
