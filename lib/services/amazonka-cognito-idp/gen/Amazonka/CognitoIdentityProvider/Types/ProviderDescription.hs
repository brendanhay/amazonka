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
-- Module      : Amazonka.CognitoIdentityProvider.Types.ProviderDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.ProviderDescription where

import Amazonka.CognitoIdentityProvider.Types.IdentityProviderTypeType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A container for identity provider details.
--
-- /See:/ 'newProviderDescription' smart constructor.
data ProviderDescription = ProviderDescription'
  { -- | The date the provider was last modified.
    lastModifiedDate :: Prelude.Maybe Core.POSIX,
    -- | The identity provider type.
    providerType :: Prelude.Maybe IdentityProviderTypeType,
    -- | The date the provider was added to the user pool.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The identity provider name.
    providerName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProviderDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'providerDescription_lastModifiedDate' - The date the provider was last modified.
--
-- 'providerType', 'providerDescription_providerType' - The identity provider type.
--
-- 'creationDate', 'providerDescription_creationDate' - The date the provider was added to the user pool.
--
-- 'providerName', 'providerDescription_providerName' - The identity provider name.
newProviderDescription ::
  ProviderDescription
newProviderDescription =
  ProviderDescription'
    { lastModifiedDate =
        Prelude.Nothing,
      providerType = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      providerName = Prelude.Nothing
    }

-- | The date the provider was last modified.
providerDescription_lastModifiedDate :: Lens.Lens' ProviderDescription (Prelude.Maybe Prelude.UTCTime)
providerDescription_lastModifiedDate = Lens.lens (\ProviderDescription' {lastModifiedDate} -> lastModifiedDate) (\s@ProviderDescription' {} a -> s {lastModifiedDate = a} :: ProviderDescription) Prelude.. Lens.mapping Core._Time

-- | The identity provider type.
providerDescription_providerType :: Lens.Lens' ProviderDescription (Prelude.Maybe IdentityProviderTypeType)
providerDescription_providerType = Lens.lens (\ProviderDescription' {providerType} -> providerType) (\s@ProviderDescription' {} a -> s {providerType = a} :: ProviderDescription)

-- | The date the provider was added to the user pool.
providerDescription_creationDate :: Lens.Lens' ProviderDescription (Prelude.Maybe Prelude.UTCTime)
providerDescription_creationDate = Lens.lens (\ProviderDescription' {creationDate} -> creationDate) (\s@ProviderDescription' {} a -> s {creationDate = a} :: ProviderDescription) Prelude.. Lens.mapping Core._Time

-- | The identity provider name.
providerDescription_providerName :: Lens.Lens' ProviderDescription (Prelude.Maybe Prelude.Text)
providerDescription_providerName = Lens.lens (\ProviderDescription' {providerName} -> providerName) (\s@ProviderDescription' {} a -> s {providerName = a} :: ProviderDescription)

instance Core.FromJSON ProviderDescription where
  parseJSON =
    Core.withObject
      "ProviderDescription"
      ( \x ->
          ProviderDescription'
            Prelude.<$> (x Core..:? "LastModifiedDate")
            Prelude.<*> (x Core..:? "ProviderType")
            Prelude.<*> (x Core..:? "CreationDate")
            Prelude.<*> (x Core..:? "ProviderName")
      )

instance Prelude.Hashable ProviderDescription where
  hashWithSalt _salt ProviderDescription' {..} =
    _salt `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` providerType
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` providerName

instance Prelude.NFData ProviderDescription where
  rnf ProviderDescription' {..} =
    Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf providerType
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf providerName
