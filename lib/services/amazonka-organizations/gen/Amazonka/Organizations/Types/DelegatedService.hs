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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Organizations.Types.DelegatedService where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the Amazon Web Services service for which the
-- account is a delegated administrator.
--
-- /See:/ 'newDelegatedService' smart constructor.
data DelegatedService = DelegatedService'
  { -- | The date that the account became a delegated administrator for this
    -- service.
    delegationEnabledDate :: Prelude.Maybe Data.POSIX,
    -- | The name of an Amazon Web Services service that can request an operation
    -- for the specified service. This is typically in the form of a URL, such
    -- as: @ @/@servicename@/@.amazonaws.com@.
    servicePrincipal :: Prelude.Maybe Prelude.Text
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
-- 'delegationEnabledDate', 'delegatedService_delegationEnabledDate' - The date that the account became a delegated administrator for this
-- service.
--
-- 'servicePrincipal', 'delegatedService_servicePrincipal' - The name of an Amazon Web Services service that can request an operation
-- for the specified service. This is typically in the form of a URL, such
-- as: @ @/@servicename@/@.amazonaws.com@.
newDelegatedService ::
  DelegatedService
newDelegatedService =
  DelegatedService'
    { delegationEnabledDate =
        Prelude.Nothing,
      servicePrincipal = Prelude.Nothing
    }

-- | The date that the account became a delegated administrator for this
-- service.
delegatedService_delegationEnabledDate :: Lens.Lens' DelegatedService (Prelude.Maybe Prelude.UTCTime)
delegatedService_delegationEnabledDate = Lens.lens (\DelegatedService' {delegationEnabledDate} -> delegationEnabledDate) (\s@DelegatedService' {} a -> s {delegationEnabledDate = a} :: DelegatedService) Prelude.. Lens.mapping Data._Time

-- | The name of an Amazon Web Services service that can request an operation
-- for the specified service. This is typically in the form of a URL, such
-- as: @ @/@servicename@/@.amazonaws.com@.
delegatedService_servicePrincipal :: Lens.Lens' DelegatedService (Prelude.Maybe Prelude.Text)
delegatedService_servicePrincipal = Lens.lens (\DelegatedService' {servicePrincipal} -> servicePrincipal) (\s@DelegatedService' {} a -> s {servicePrincipal = a} :: DelegatedService)

instance Data.FromJSON DelegatedService where
  parseJSON =
    Data.withObject
      "DelegatedService"
      ( \x ->
          DelegatedService'
            Prelude.<$> (x Data..:? "DelegationEnabledDate")
            Prelude.<*> (x Data..:? "ServicePrincipal")
      )

instance Prelude.Hashable DelegatedService where
  hashWithSalt _salt DelegatedService' {..} =
    _salt
      `Prelude.hashWithSalt` delegationEnabledDate
      `Prelude.hashWithSalt` servicePrincipal

instance Prelude.NFData DelegatedService where
  rnf DelegatedService' {..} =
    Prelude.rnf delegationEnabledDate `Prelude.seq`
      Prelude.rnf servicePrincipal
