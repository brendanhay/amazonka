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
-- Module      : Amazonka.WorkSpacesWeb.Types.IdentityProviderSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpacesWeb.Types.IdentityProviderSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkSpacesWeb.Types.IdentityProviderType

-- | The summary of the identity provider.
--
-- /See:/ 'newIdentityProviderSummary' smart constructor.
data IdentityProviderSummary = IdentityProviderSummary'
  { -- | The ARN of the identity provider.
    identityProviderArn :: Prelude.Maybe Prelude.Text,
    -- | The identity provider name.
    identityProviderName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The identity provider type.
    identityProviderType :: Prelude.Maybe IdentityProviderType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IdentityProviderSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityProviderArn', 'identityProviderSummary_identityProviderArn' - The ARN of the identity provider.
--
-- 'identityProviderName', 'identityProviderSummary_identityProviderName' - The identity provider name.
--
-- 'identityProviderType', 'identityProviderSummary_identityProviderType' - The identity provider type.
newIdentityProviderSummary ::
  IdentityProviderSummary
newIdentityProviderSummary =
  IdentityProviderSummary'
    { identityProviderArn =
        Prelude.Nothing,
      identityProviderName = Prelude.Nothing,
      identityProviderType = Prelude.Nothing
    }

-- | The ARN of the identity provider.
identityProviderSummary_identityProviderArn :: Lens.Lens' IdentityProviderSummary (Prelude.Maybe Prelude.Text)
identityProviderSummary_identityProviderArn = Lens.lens (\IdentityProviderSummary' {identityProviderArn} -> identityProviderArn) (\s@IdentityProviderSummary' {} a -> s {identityProviderArn = a} :: IdentityProviderSummary)

-- | The identity provider name.
identityProviderSummary_identityProviderName :: Lens.Lens' IdentityProviderSummary (Prelude.Maybe Prelude.Text)
identityProviderSummary_identityProviderName = Lens.lens (\IdentityProviderSummary' {identityProviderName} -> identityProviderName) (\s@IdentityProviderSummary' {} a -> s {identityProviderName = a} :: IdentityProviderSummary) Prelude.. Lens.mapping Data._Sensitive

-- | The identity provider type.
identityProviderSummary_identityProviderType :: Lens.Lens' IdentityProviderSummary (Prelude.Maybe IdentityProviderType)
identityProviderSummary_identityProviderType = Lens.lens (\IdentityProviderSummary' {identityProviderType} -> identityProviderType) (\s@IdentityProviderSummary' {} a -> s {identityProviderType = a} :: IdentityProviderSummary)

instance Data.FromJSON IdentityProviderSummary where
  parseJSON =
    Data.withObject
      "IdentityProviderSummary"
      ( \x ->
          IdentityProviderSummary'
            Prelude.<$> (x Data..:? "identityProviderArn")
            Prelude.<*> (x Data..:? "identityProviderName")
            Prelude.<*> (x Data..:? "identityProviderType")
      )

instance Prelude.Hashable IdentityProviderSummary where
  hashWithSalt _salt IdentityProviderSummary' {..} =
    _salt
      `Prelude.hashWithSalt` identityProviderArn
      `Prelude.hashWithSalt` identityProviderName
      `Prelude.hashWithSalt` identityProviderType

instance Prelude.NFData IdentityProviderSummary where
  rnf IdentityProviderSummary' {..} =
    Prelude.rnf identityProviderArn
      `Prelude.seq` Prelude.rnf identityProviderName
      `Prelude.seq` Prelude.rnf identityProviderType
