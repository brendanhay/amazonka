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
-- Module      : Amazonka.WorkMail.Types.AvailabilityConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMail.Types.AvailabilityConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkMail.Types.AvailabilityProviderType
import Amazonka.WorkMail.Types.LambdaAvailabilityProvider
import Amazonka.WorkMail.Types.RedactedEwsAvailabilityProvider

-- | List all the @AvailabilityConfiguration@\'s for the given WorkMail
-- organization.
--
-- /See:/ 'newAvailabilityConfiguration' smart constructor.
data AvailabilityConfiguration = AvailabilityConfiguration'
  { -- | If @ProviderType@ is @EWS@, then this field contains
    -- @RedactedEwsAvailabilityProvider@. Otherwise, it is not required.
    ewsProvider :: Prelude.Maybe RedactedEwsAvailabilityProvider,
    -- | Displays the domain to which the provider applies.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | Displays the provider type that applies to this domain.
    providerType :: Prelude.Maybe AvailabilityProviderType,
    -- | If ProviderType is @LAMBDA@ then this field contains
    -- @LambdaAvailabilityProvider@. Otherwise, it is not required.
    lambdaProvider :: Prelude.Maybe LambdaAvailabilityProvider,
    -- | The date and time at which the availability configuration was created.
    dateCreated :: Prelude.Maybe Core.POSIX,
    -- | The date and time at which the availability configuration was last
    -- modified.
    dateModified :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AvailabilityConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ewsProvider', 'availabilityConfiguration_ewsProvider' - If @ProviderType@ is @EWS@, then this field contains
-- @RedactedEwsAvailabilityProvider@. Otherwise, it is not required.
--
-- 'domainName', 'availabilityConfiguration_domainName' - Displays the domain to which the provider applies.
--
-- 'providerType', 'availabilityConfiguration_providerType' - Displays the provider type that applies to this domain.
--
-- 'lambdaProvider', 'availabilityConfiguration_lambdaProvider' - If ProviderType is @LAMBDA@ then this field contains
-- @LambdaAvailabilityProvider@. Otherwise, it is not required.
--
-- 'dateCreated', 'availabilityConfiguration_dateCreated' - The date and time at which the availability configuration was created.
--
-- 'dateModified', 'availabilityConfiguration_dateModified' - The date and time at which the availability configuration was last
-- modified.
newAvailabilityConfiguration ::
  AvailabilityConfiguration
newAvailabilityConfiguration =
  AvailabilityConfiguration'
    { ewsProvider =
        Prelude.Nothing,
      domainName = Prelude.Nothing,
      providerType = Prelude.Nothing,
      lambdaProvider = Prelude.Nothing,
      dateCreated = Prelude.Nothing,
      dateModified = Prelude.Nothing
    }

-- | If @ProviderType@ is @EWS@, then this field contains
-- @RedactedEwsAvailabilityProvider@. Otherwise, it is not required.
availabilityConfiguration_ewsProvider :: Lens.Lens' AvailabilityConfiguration (Prelude.Maybe RedactedEwsAvailabilityProvider)
availabilityConfiguration_ewsProvider = Lens.lens (\AvailabilityConfiguration' {ewsProvider} -> ewsProvider) (\s@AvailabilityConfiguration' {} a -> s {ewsProvider = a} :: AvailabilityConfiguration)

-- | Displays the domain to which the provider applies.
availabilityConfiguration_domainName :: Lens.Lens' AvailabilityConfiguration (Prelude.Maybe Prelude.Text)
availabilityConfiguration_domainName = Lens.lens (\AvailabilityConfiguration' {domainName} -> domainName) (\s@AvailabilityConfiguration' {} a -> s {domainName = a} :: AvailabilityConfiguration)

-- | Displays the provider type that applies to this domain.
availabilityConfiguration_providerType :: Lens.Lens' AvailabilityConfiguration (Prelude.Maybe AvailabilityProviderType)
availabilityConfiguration_providerType = Lens.lens (\AvailabilityConfiguration' {providerType} -> providerType) (\s@AvailabilityConfiguration' {} a -> s {providerType = a} :: AvailabilityConfiguration)

-- | If ProviderType is @LAMBDA@ then this field contains
-- @LambdaAvailabilityProvider@. Otherwise, it is not required.
availabilityConfiguration_lambdaProvider :: Lens.Lens' AvailabilityConfiguration (Prelude.Maybe LambdaAvailabilityProvider)
availabilityConfiguration_lambdaProvider = Lens.lens (\AvailabilityConfiguration' {lambdaProvider} -> lambdaProvider) (\s@AvailabilityConfiguration' {} a -> s {lambdaProvider = a} :: AvailabilityConfiguration)

-- | The date and time at which the availability configuration was created.
availabilityConfiguration_dateCreated :: Lens.Lens' AvailabilityConfiguration (Prelude.Maybe Prelude.UTCTime)
availabilityConfiguration_dateCreated = Lens.lens (\AvailabilityConfiguration' {dateCreated} -> dateCreated) (\s@AvailabilityConfiguration' {} a -> s {dateCreated = a} :: AvailabilityConfiguration) Prelude.. Lens.mapping Core._Time

-- | The date and time at which the availability configuration was last
-- modified.
availabilityConfiguration_dateModified :: Lens.Lens' AvailabilityConfiguration (Prelude.Maybe Prelude.UTCTime)
availabilityConfiguration_dateModified = Lens.lens (\AvailabilityConfiguration' {dateModified} -> dateModified) (\s@AvailabilityConfiguration' {} a -> s {dateModified = a} :: AvailabilityConfiguration) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON AvailabilityConfiguration where
  parseJSON =
    Core.withObject
      "AvailabilityConfiguration"
      ( \x ->
          AvailabilityConfiguration'
            Prelude.<$> (x Core..:? "EwsProvider")
            Prelude.<*> (x Core..:? "DomainName")
            Prelude.<*> (x Core..:? "ProviderType")
            Prelude.<*> (x Core..:? "LambdaProvider")
            Prelude.<*> (x Core..:? "DateCreated")
            Prelude.<*> (x Core..:? "DateModified")
      )

instance Prelude.Hashable AvailabilityConfiguration where
  hashWithSalt _salt AvailabilityConfiguration' {..} =
    _salt `Prelude.hashWithSalt` ewsProvider
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` providerType
      `Prelude.hashWithSalt` lambdaProvider
      `Prelude.hashWithSalt` dateCreated
      `Prelude.hashWithSalt` dateModified

instance Prelude.NFData AvailabilityConfiguration where
  rnf AvailabilityConfiguration' {..} =
    Prelude.rnf ewsProvider
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf providerType
      `Prelude.seq` Prelude.rnf lambdaProvider
      `Prelude.seq` Prelude.rnf dateCreated
      `Prelude.seq` Prelude.rnf dateModified
