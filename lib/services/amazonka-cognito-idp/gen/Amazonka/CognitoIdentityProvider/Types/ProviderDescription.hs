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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.ProviderDescription where

import Amazonka.CognitoIdentityProvider.Types.IdentityProviderTypeType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A container for IdP details.
--
-- /See:/ 'newProviderDescription' smart constructor.
data ProviderDescription = ProviderDescription'
  { -- | The date the provider was added to the user pool.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The date the provider was last modified.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | The IdP name.
    providerName :: Prelude.Maybe Prelude.Text,
    -- | The IdP type.
    providerType :: Prelude.Maybe IdentityProviderTypeType
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
-- 'creationDate', 'providerDescription_creationDate' - The date the provider was added to the user pool.
--
-- 'lastModifiedDate', 'providerDescription_lastModifiedDate' - The date the provider was last modified.
--
-- 'providerName', 'providerDescription_providerName' - The IdP name.
--
-- 'providerType', 'providerDescription_providerType' - The IdP type.
newProviderDescription ::
  ProviderDescription
newProviderDescription =
  ProviderDescription'
    { creationDate =
        Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      providerName = Prelude.Nothing,
      providerType = Prelude.Nothing
    }

-- | The date the provider was added to the user pool.
providerDescription_creationDate :: Lens.Lens' ProviderDescription (Prelude.Maybe Prelude.UTCTime)
providerDescription_creationDate = Lens.lens (\ProviderDescription' {creationDate} -> creationDate) (\s@ProviderDescription' {} a -> s {creationDate = a} :: ProviderDescription) Prelude.. Lens.mapping Data._Time

-- | The date the provider was last modified.
providerDescription_lastModifiedDate :: Lens.Lens' ProviderDescription (Prelude.Maybe Prelude.UTCTime)
providerDescription_lastModifiedDate = Lens.lens (\ProviderDescription' {lastModifiedDate} -> lastModifiedDate) (\s@ProviderDescription' {} a -> s {lastModifiedDate = a} :: ProviderDescription) Prelude.. Lens.mapping Data._Time

-- | The IdP name.
providerDescription_providerName :: Lens.Lens' ProviderDescription (Prelude.Maybe Prelude.Text)
providerDescription_providerName = Lens.lens (\ProviderDescription' {providerName} -> providerName) (\s@ProviderDescription' {} a -> s {providerName = a} :: ProviderDescription)

-- | The IdP type.
providerDescription_providerType :: Lens.Lens' ProviderDescription (Prelude.Maybe IdentityProviderTypeType)
providerDescription_providerType = Lens.lens (\ProviderDescription' {providerType} -> providerType) (\s@ProviderDescription' {} a -> s {providerType = a} :: ProviderDescription)

instance Data.FromJSON ProviderDescription where
  parseJSON =
    Data.withObject
      "ProviderDescription"
      ( \x ->
          ProviderDescription'
            Prelude.<$> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "LastModifiedDate")
            Prelude.<*> (x Data..:? "ProviderName")
            Prelude.<*> (x Data..:? "ProviderType")
      )

instance Prelude.Hashable ProviderDescription where
  hashWithSalt _salt ProviderDescription' {..} =
    _salt
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` providerName
      `Prelude.hashWithSalt` providerType

instance Prelude.NFData ProviderDescription where
  rnf ProviderDescription' {..} =
    Prelude.rnf creationDate `Prelude.seq`
      Prelude.rnf lastModifiedDate `Prelude.seq`
        Prelude.rnf providerName `Prelude.seq`
          Prelude.rnf providerType
