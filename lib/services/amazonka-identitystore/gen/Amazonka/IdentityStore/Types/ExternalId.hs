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
-- Module      : Amazonka.IdentityStore.Types.ExternalId
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IdentityStore.Types.ExternalId where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The identifier issued to this resource by an external identity provider.
--
-- /See:/ 'newExternalId' smart constructor.
data ExternalId = ExternalId'
  { -- | The issuer for an external identifier.
    issuer :: Core.Sensitive Prelude.Text,
    -- | The identifier issued to this resource by an external identity provider.
    id :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExternalId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'issuer', 'externalId_issuer' - The issuer for an external identifier.
--
-- 'id', 'externalId_id' - The identifier issued to this resource by an external identity provider.
newExternalId ::
  -- | 'issuer'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  ExternalId
newExternalId pIssuer_ pId_ =
  ExternalId'
    { issuer =
        Core._Sensitive Lens.# pIssuer_,
      id = Core._Sensitive Lens.# pId_
    }

-- | The issuer for an external identifier.
externalId_issuer :: Lens.Lens' ExternalId Prelude.Text
externalId_issuer = Lens.lens (\ExternalId' {issuer} -> issuer) (\s@ExternalId' {} a -> s {issuer = a} :: ExternalId) Prelude.. Core._Sensitive

-- | The identifier issued to this resource by an external identity provider.
externalId_id :: Lens.Lens' ExternalId Prelude.Text
externalId_id = Lens.lens (\ExternalId' {id} -> id) (\s@ExternalId' {} a -> s {id = a} :: ExternalId) Prelude.. Core._Sensitive

instance Core.FromJSON ExternalId where
  parseJSON =
    Core.withObject
      "ExternalId"
      ( \x ->
          ExternalId'
            Prelude.<$> (x Core..: "Issuer") Prelude.<*> (x Core..: "Id")
      )

instance Prelude.Hashable ExternalId where
  hashWithSalt _salt ExternalId' {..} =
    _salt `Prelude.hashWithSalt` issuer
      `Prelude.hashWithSalt` id

instance Prelude.NFData ExternalId where
  rnf ExternalId' {..} =
    Prelude.rnf issuer `Prelude.seq` Prelude.rnf id

instance Core.ToJSON ExternalId where
  toJSON ExternalId' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Issuer" Core..= issuer),
            Prelude.Just ("Id" Core..= id)
          ]
      )
