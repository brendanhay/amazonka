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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.ProviderDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.ProviderDescription where

import Network.AWS.CognitoIdentityProvider.Types.IdentityProviderTypeType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A container for identity provider details.
--
-- /See:/ 'newProviderDescription' smart constructor.
data ProviderDescription = ProviderDescription'
  { -- | The date the provider was last modified.
    lastModifiedDate :: Core.Maybe Core.POSIX,
    -- | The identity provider type.
    providerType :: Core.Maybe IdentityProviderTypeType,
    -- | The identity provider name.
    providerName :: Core.Maybe Core.Text,
    -- | The date the provider was added to the user pool.
    creationDate :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'providerName', 'providerDescription_providerName' - The identity provider name.
--
-- 'creationDate', 'providerDescription_creationDate' - The date the provider was added to the user pool.
newProviderDescription ::
  ProviderDescription
newProviderDescription =
  ProviderDescription'
    { lastModifiedDate =
        Core.Nothing,
      providerType = Core.Nothing,
      providerName = Core.Nothing,
      creationDate = Core.Nothing
    }

-- | The date the provider was last modified.
providerDescription_lastModifiedDate :: Lens.Lens' ProviderDescription (Core.Maybe Core.UTCTime)
providerDescription_lastModifiedDate = Lens.lens (\ProviderDescription' {lastModifiedDate} -> lastModifiedDate) (\s@ProviderDescription' {} a -> s {lastModifiedDate = a} :: ProviderDescription) Core.. Lens.mapping Core._Time

-- | The identity provider type.
providerDescription_providerType :: Lens.Lens' ProviderDescription (Core.Maybe IdentityProviderTypeType)
providerDescription_providerType = Lens.lens (\ProviderDescription' {providerType} -> providerType) (\s@ProviderDescription' {} a -> s {providerType = a} :: ProviderDescription)

-- | The identity provider name.
providerDescription_providerName :: Lens.Lens' ProviderDescription (Core.Maybe Core.Text)
providerDescription_providerName = Lens.lens (\ProviderDescription' {providerName} -> providerName) (\s@ProviderDescription' {} a -> s {providerName = a} :: ProviderDescription)

-- | The date the provider was added to the user pool.
providerDescription_creationDate :: Lens.Lens' ProviderDescription (Core.Maybe Core.UTCTime)
providerDescription_creationDate = Lens.lens (\ProviderDescription' {creationDate} -> creationDate) (\s@ProviderDescription' {} a -> s {creationDate = a} :: ProviderDescription) Core.. Lens.mapping Core._Time

instance Core.FromJSON ProviderDescription where
  parseJSON =
    Core.withObject
      "ProviderDescription"
      ( \x ->
          ProviderDescription'
            Core.<$> (x Core..:? "LastModifiedDate")
            Core.<*> (x Core..:? "ProviderType")
            Core.<*> (x Core..:? "ProviderName")
            Core.<*> (x Core..:? "CreationDate")
      )

instance Core.Hashable ProviderDescription

instance Core.NFData ProviderDescription
