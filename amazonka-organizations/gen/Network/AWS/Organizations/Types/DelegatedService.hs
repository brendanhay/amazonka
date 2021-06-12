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
-- Module      : Network.AWS.Organizations.Types.DelegatedService
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.DelegatedService where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about the AWS service for which the account is a
-- delegated administrator.
--
-- /See:/ 'newDelegatedService' smart constructor.
data DelegatedService = DelegatedService'
  { -- | The name of a service that can request an operation for the specified
    -- service. This is typically in the form of a URL, such as:
    -- @ servicename.amazonaws.com@.
    servicePrincipal :: Core.Maybe Core.Text,
    -- | The date that the account became a delegated administrator for this
    -- service.
    delegationEnabledDate :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DelegatedService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'servicePrincipal', 'delegatedService_servicePrincipal' - The name of a service that can request an operation for the specified
-- service. This is typically in the form of a URL, such as:
-- @ servicename.amazonaws.com@.
--
-- 'delegationEnabledDate', 'delegatedService_delegationEnabledDate' - The date that the account became a delegated administrator for this
-- service.
newDelegatedService ::
  DelegatedService
newDelegatedService =
  DelegatedService'
    { servicePrincipal = Core.Nothing,
      delegationEnabledDate = Core.Nothing
    }

-- | The name of a service that can request an operation for the specified
-- service. This is typically in the form of a URL, such as:
-- @ servicename.amazonaws.com@.
delegatedService_servicePrincipal :: Lens.Lens' DelegatedService (Core.Maybe Core.Text)
delegatedService_servicePrincipal = Lens.lens (\DelegatedService' {servicePrincipal} -> servicePrincipal) (\s@DelegatedService' {} a -> s {servicePrincipal = a} :: DelegatedService)

-- | The date that the account became a delegated administrator for this
-- service.
delegatedService_delegationEnabledDate :: Lens.Lens' DelegatedService (Core.Maybe Core.UTCTime)
delegatedService_delegationEnabledDate = Lens.lens (\DelegatedService' {delegationEnabledDate} -> delegationEnabledDate) (\s@DelegatedService' {} a -> s {delegationEnabledDate = a} :: DelegatedService) Core.. Lens.mapping Core._Time

instance Core.FromJSON DelegatedService where
  parseJSON =
    Core.withObject
      "DelegatedService"
      ( \x ->
          DelegatedService'
            Core.<$> (x Core..:? "ServicePrincipal")
            Core.<*> (x Core..:? "DelegationEnabledDate")
      )

instance Core.Hashable DelegatedService

instance Core.NFData DelegatedService
