{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.UpgradeElasticsearchDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows you to either upgrade your domain or perform an Upgrade
-- eligibility check to a compatible Elasticsearch version.
module Network.AWS.ElasticSearch.UpgradeElasticsearchDomain
  ( -- * Creating a Request
    UpgradeElasticsearchDomain (..),
    newUpgradeElasticsearchDomain,

    -- * Request Lenses
    upgradeElasticsearchDomain_performCheckOnly,
    upgradeElasticsearchDomain_domainName,
    upgradeElasticsearchDomain_targetVersion,

    -- * Destructuring the Response
    UpgradeElasticsearchDomainResponse (..),
    newUpgradeElasticsearchDomainResponse,

    -- * Response Lenses
    upgradeElasticsearchDomainResponse_targetVersion,
    upgradeElasticsearchDomainResponse_domainName,
    upgradeElasticsearchDomainResponse_performCheckOnly,
    upgradeElasticsearchDomainResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for request parameters to @ UpgradeElasticsearchDomain @
-- operation.
--
-- /See:/ 'newUpgradeElasticsearchDomain' smart constructor.
data UpgradeElasticsearchDomain = UpgradeElasticsearchDomain'
  { -- | This flag, when set to True, indicates that an Upgrade Eligibility Check
    -- needs to be performed. This will not actually perform the Upgrade.
    performCheckOnly :: Core.Maybe Core.Bool,
    domainName :: Core.Text,
    -- | The version of Elasticsearch that you intend to upgrade the domain to.
    targetVersion :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpgradeElasticsearchDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'performCheckOnly', 'upgradeElasticsearchDomain_performCheckOnly' - This flag, when set to True, indicates that an Upgrade Eligibility Check
-- needs to be performed. This will not actually perform the Upgrade.
--
-- 'domainName', 'upgradeElasticsearchDomain_domainName' - Undocumented member.
--
-- 'targetVersion', 'upgradeElasticsearchDomain_targetVersion' - The version of Elasticsearch that you intend to upgrade the domain to.
newUpgradeElasticsearchDomain ::
  -- | 'domainName'
  Core.Text ->
  -- | 'targetVersion'
  Core.Text ->
  UpgradeElasticsearchDomain
newUpgradeElasticsearchDomain
  pDomainName_
  pTargetVersion_ =
    UpgradeElasticsearchDomain'
      { performCheckOnly =
          Core.Nothing,
        domainName = pDomainName_,
        targetVersion = pTargetVersion_
      }

-- | This flag, when set to True, indicates that an Upgrade Eligibility Check
-- needs to be performed. This will not actually perform the Upgrade.
upgradeElasticsearchDomain_performCheckOnly :: Lens.Lens' UpgradeElasticsearchDomain (Core.Maybe Core.Bool)
upgradeElasticsearchDomain_performCheckOnly = Lens.lens (\UpgradeElasticsearchDomain' {performCheckOnly} -> performCheckOnly) (\s@UpgradeElasticsearchDomain' {} a -> s {performCheckOnly = a} :: UpgradeElasticsearchDomain)

-- | Undocumented member.
upgradeElasticsearchDomain_domainName :: Lens.Lens' UpgradeElasticsearchDomain Core.Text
upgradeElasticsearchDomain_domainName = Lens.lens (\UpgradeElasticsearchDomain' {domainName} -> domainName) (\s@UpgradeElasticsearchDomain' {} a -> s {domainName = a} :: UpgradeElasticsearchDomain)

-- | The version of Elasticsearch that you intend to upgrade the domain to.
upgradeElasticsearchDomain_targetVersion :: Lens.Lens' UpgradeElasticsearchDomain Core.Text
upgradeElasticsearchDomain_targetVersion = Lens.lens (\UpgradeElasticsearchDomain' {targetVersion} -> targetVersion) (\s@UpgradeElasticsearchDomain' {} a -> s {targetVersion = a} :: UpgradeElasticsearchDomain)

instance Core.AWSRequest UpgradeElasticsearchDomain where
  type
    AWSResponse UpgradeElasticsearchDomain =
      UpgradeElasticsearchDomainResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpgradeElasticsearchDomainResponse'
            Core.<$> (x Core..?> "TargetVersion")
            Core.<*> (x Core..?> "DomainName")
            Core.<*> (x Core..?> "PerformCheckOnly")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpgradeElasticsearchDomain

instance Core.NFData UpgradeElasticsearchDomain

instance Core.ToHeaders UpgradeElasticsearchDomain where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON UpgradeElasticsearchDomain where
  toJSON UpgradeElasticsearchDomain' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PerformCheckOnly" Core..=)
              Core.<$> performCheckOnly,
            Core.Just ("DomainName" Core..= domainName),
            Core.Just ("TargetVersion" Core..= targetVersion)
          ]
      )

instance Core.ToPath UpgradeElasticsearchDomain where
  toPath = Core.const "/2015-01-01/es/upgradeDomain"

instance Core.ToQuery UpgradeElasticsearchDomain where
  toQuery = Core.const Core.mempty

-- | Container for response returned by @ UpgradeElasticsearchDomain @
-- operation.
--
-- /See:/ 'newUpgradeElasticsearchDomainResponse' smart constructor.
data UpgradeElasticsearchDomainResponse = UpgradeElasticsearchDomainResponse'
  { -- | The version of Elasticsearch that you intend to upgrade the domain to.
    targetVersion :: Core.Maybe Core.Text,
    domainName :: Core.Maybe Core.Text,
    -- | This flag, when set to True, indicates that an Upgrade Eligibility Check
    -- needs to be performed. This will not actually perform the Upgrade.
    performCheckOnly :: Core.Maybe Core.Bool,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpgradeElasticsearchDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetVersion', 'upgradeElasticsearchDomainResponse_targetVersion' - The version of Elasticsearch that you intend to upgrade the domain to.
--
-- 'domainName', 'upgradeElasticsearchDomainResponse_domainName' - Undocumented member.
--
-- 'performCheckOnly', 'upgradeElasticsearchDomainResponse_performCheckOnly' - This flag, when set to True, indicates that an Upgrade Eligibility Check
-- needs to be performed. This will not actually perform the Upgrade.
--
-- 'httpStatus', 'upgradeElasticsearchDomainResponse_httpStatus' - The response's http status code.
newUpgradeElasticsearchDomainResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpgradeElasticsearchDomainResponse
newUpgradeElasticsearchDomainResponse pHttpStatus_ =
  UpgradeElasticsearchDomainResponse'
    { targetVersion =
        Core.Nothing,
      domainName = Core.Nothing,
      performCheckOnly = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The version of Elasticsearch that you intend to upgrade the domain to.
upgradeElasticsearchDomainResponse_targetVersion :: Lens.Lens' UpgradeElasticsearchDomainResponse (Core.Maybe Core.Text)
upgradeElasticsearchDomainResponse_targetVersion = Lens.lens (\UpgradeElasticsearchDomainResponse' {targetVersion} -> targetVersion) (\s@UpgradeElasticsearchDomainResponse' {} a -> s {targetVersion = a} :: UpgradeElasticsearchDomainResponse)

-- | Undocumented member.
upgradeElasticsearchDomainResponse_domainName :: Lens.Lens' UpgradeElasticsearchDomainResponse (Core.Maybe Core.Text)
upgradeElasticsearchDomainResponse_domainName = Lens.lens (\UpgradeElasticsearchDomainResponse' {domainName} -> domainName) (\s@UpgradeElasticsearchDomainResponse' {} a -> s {domainName = a} :: UpgradeElasticsearchDomainResponse)

-- | This flag, when set to True, indicates that an Upgrade Eligibility Check
-- needs to be performed. This will not actually perform the Upgrade.
upgradeElasticsearchDomainResponse_performCheckOnly :: Lens.Lens' UpgradeElasticsearchDomainResponse (Core.Maybe Core.Bool)
upgradeElasticsearchDomainResponse_performCheckOnly = Lens.lens (\UpgradeElasticsearchDomainResponse' {performCheckOnly} -> performCheckOnly) (\s@UpgradeElasticsearchDomainResponse' {} a -> s {performCheckOnly = a} :: UpgradeElasticsearchDomainResponse)

-- | The response's http status code.
upgradeElasticsearchDomainResponse_httpStatus :: Lens.Lens' UpgradeElasticsearchDomainResponse Core.Int
upgradeElasticsearchDomainResponse_httpStatus = Lens.lens (\UpgradeElasticsearchDomainResponse' {httpStatus} -> httpStatus) (\s@UpgradeElasticsearchDomainResponse' {} a -> s {httpStatus = a} :: UpgradeElasticsearchDomainResponse)

instance
  Core.NFData
    UpgradeElasticsearchDomainResponse
