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
-- Module      : Amazonka.Organizations.Types.DelegatedService
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Organizations.Types.DelegatedService where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the Amazon Web Services service for which the
-- account is a delegated administrator.
--
-- /See:/ 'newDelegatedService' smart constructor.
data DelegatedService = DelegatedService'
  { -- | The name of an Amazon Web Services service that can request an operation
    -- for the specified service. This is typically in the form of a URL, such
    -- as: @ servicename.amazonaws.com@.
    servicePrincipal :: Prelude.Maybe Prelude.Text,
    -- | The date that the account became a delegated administrator for this
    -- service.
    delegationEnabledDate :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DelegatedService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'servicePrincipal', 'delegatedService_servicePrincipal' - The name of an Amazon Web Services service that can request an operation
-- for the specified service. This is typically in the form of a URL, such
-- as: @ servicename.amazonaws.com@.
--
-- 'delegationEnabledDate', 'delegatedService_delegationEnabledDate' - The date that the account became a delegated administrator for this
-- service.
newDelegatedService ::
  DelegatedService
newDelegatedService =
  DelegatedService'
    { servicePrincipal =
        Prelude.Nothing,
      delegationEnabledDate = Prelude.Nothing
    }

-- | The name of an Amazon Web Services service that can request an operation
-- for the specified service. This is typically in the form of a URL, such
-- as: @ servicename.amazonaws.com@.
delegatedService_servicePrincipal :: Lens.Lens' DelegatedService (Prelude.Maybe Prelude.Text)
delegatedService_servicePrincipal = Lens.lens (\DelegatedService' {servicePrincipal} -> servicePrincipal) (\s@DelegatedService' {} a -> s {servicePrincipal = a} :: DelegatedService)

-- | The date that the account became a delegated administrator for this
-- service.
delegatedService_delegationEnabledDate :: Lens.Lens' DelegatedService (Prelude.Maybe Prelude.UTCTime)
delegatedService_delegationEnabledDate = Lens.lens (\DelegatedService' {delegationEnabledDate} -> delegationEnabledDate) (\s@DelegatedService' {} a -> s {delegationEnabledDate = a} :: DelegatedService) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON DelegatedService where
  parseJSON =
    Core.withObject
      "DelegatedService"
      ( \x ->
          DelegatedService'
            Prelude.<$> (x Core..:? "ServicePrincipal")
            Prelude.<*> (x Core..:? "DelegationEnabledDate")
      )

instance Prelude.Hashable DelegatedService where
  hashWithSalt _salt DelegatedService' {..} =
    _salt `Prelude.hashWithSalt` servicePrincipal
      `Prelude.hashWithSalt` delegationEnabledDate

instance Prelude.NFData DelegatedService where
  rnf DelegatedService' {..} =
    Prelude.rnf servicePrincipal
      `Prelude.seq` Prelude.rnf delegationEnabledDate
