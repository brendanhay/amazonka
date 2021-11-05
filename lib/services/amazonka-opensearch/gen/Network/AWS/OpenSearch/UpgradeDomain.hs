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
-- Module      : Network.AWS.OpenSearch.UpgradeDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows you to either upgrade your domain or perform an upgrade
-- eligibility check to a compatible version of OpenSearch or
-- Elasticsearch.
module Network.AWS.OpenSearch.UpgradeDomain
  ( -- * Creating a Request
    UpgradeDomain (..),
    newUpgradeDomain,

    -- * Request Lenses
    upgradeDomain_performCheckOnly,
    upgradeDomain_advancedOptions,
    upgradeDomain_domainName,
    upgradeDomain_targetVersion,

    -- * Destructuring the Response
    UpgradeDomainResponse (..),
    newUpgradeDomainResponse,

    -- * Response Lenses
    upgradeDomainResponse_domainName,
    upgradeDomainResponse_upgradeId,
    upgradeDomainResponse_performCheckOnly,
    upgradeDomainResponse_targetVersion,
    upgradeDomainResponse_advancedOptions,
    upgradeDomainResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpenSearch.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the request parameters to @ UpgradeDomain @ operation.
--
-- /See:/ 'newUpgradeDomain' smart constructor.
data UpgradeDomain = UpgradeDomain'
  { -- | When true, indicates that an upgrade eligibility check needs to be
    -- performed. Does not actually perform the upgrade.
    performCheckOnly :: Prelude.Maybe Prelude.Bool,
    advancedOptions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    domainName :: Prelude.Text,
    -- | The version of OpenSearch you intend to upgrade the domain to.
    targetVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpgradeDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'performCheckOnly', 'upgradeDomain_performCheckOnly' - When true, indicates that an upgrade eligibility check needs to be
-- performed. Does not actually perform the upgrade.
--
-- 'advancedOptions', 'upgradeDomain_advancedOptions' - Undocumented member.
--
-- 'domainName', 'upgradeDomain_domainName' - Undocumented member.
--
-- 'targetVersion', 'upgradeDomain_targetVersion' - The version of OpenSearch you intend to upgrade the domain to.
newUpgradeDomain ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'targetVersion'
  Prelude.Text ->
  UpgradeDomain
newUpgradeDomain pDomainName_ pTargetVersion_ =
  UpgradeDomain'
    { performCheckOnly = Prelude.Nothing,
      advancedOptions = Prelude.Nothing,
      domainName = pDomainName_,
      targetVersion = pTargetVersion_
    }

-- | When true, indicates that an upgrade eligibility check needs to be
-- performed. Does not actually perform the upgrade.
upgradeDomain_performCheckOnly :: Lens.Lens' UpgradeDomain (Prelude.Maybe Prelude.Bool)
upgradeDomain_performCheckOnly = Lens.lens (\UpgradeDomain' {performCheckOnly} -> performCheckOnly) (\s@UpgradeDomain' {} a -> s {performCheckOnly = a} :: UpgradeDomain)

-- | Undocumented member.
upgradeDomain_advancedOptions :: Lens.Lens' UpgradeDomain (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
upgradeDomain_advancedOptions = Lens.lens (\UpgradeDomain' {advancedOptions} -> advancedOptions) (\s@UpgradeDomain' {} a -> s {advancedOptions = a} :: UpgradeDomain) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
upgradeDomain_domainName :: Lens.Lens' UpgradeDomain Prelude.Text
upgradeDomain_domainName = Lens.lens (\UpgradeDomain' {domainName} -> domainName) (\s@UpgradeDomain' {} a -> s {domainName = a} :: UpgradeDomain)

-- | The version of OpenSearch you intend to upgrade the domain to.
upgradeDomain_targetVersion :: Lens.Lens' UpgradeDomain Prelude.Text
upgradeDomain_targetVersion = Lens.lens (\UpgradeDomain' {targetVersion} -> targetVersion) (\s@UpgradeDomain' {} a -> s {targetVersion = a} :: UpgradeDomain)

instance Core.AWSRequest UpgradeDomain where
  type
    AWSResponse UpgradeDomain =
      UpgradeDomainResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpgradeDomainResponse'
            Prelude.<$> (x Core..?> "DomainName")
            Prelude.<*> (x Core..?> "UpgradeId")
            Prelude.<*> (x Core..?> "PerformCheckOnly")
            Prelude.<*> (x Core..?> "TargetVersion")
            Prelude.<*> ( x Core..?> "AdvancedOptions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpgradeDomain

instance Prelude.NFData UpgradeDomain

instance Core.ToHeaders UpgradeDomain where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON UpgradeDomain where
  toJSON UpgradeDomain' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PerformCheckOnly" Core..=)
              Prelude.<$> performCheckOnly,
            ("AdvancedOptions" Core..=)
              Prelude.<$> advancedOptions,
            Prelude.Just ("DomainName" Core..= domainName),
            Prelude.Just
              ("TargetVersion" Core..= targetVersion)
          ]
      )

instance Core.ToPath UpgradeDomain where
  toPath =
    Prelude.const
      "/2021-01-01/opensearch/upgradeDomain"

instance Core.ToQuery UpgradeDomain where
  toQuery = Prelude.const Prelude.mempty

-- | Container for response returned by @ UpgradeDomain @ operation.
--
-- /See:/ 'newUpgradeDomainResponse' smart constructor.
data UpgradeDomainResponse = UpgradeDomainResponse'
  { domainName :: Prelude.Maybe Prelude.Text,
    upgradeId :: Prelude.Maybe Prelude.Text,
    -- | When true, indicates that an upgrade eligibility check needs to be
    -- performed. Does not actually perform the upgrade.
    performCheckOnly :: Prelude.Maybe Prelude.Bool,
    -- | The version of OpenSearch that you intend to upgrade the domain to.
    targetVersion :: Prelude.Maybe Prelude.Text,
    advancedOptions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpgradeDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'upgradeDomainResponse_domainName' - Undocumented member.
--
-- 'upgradeId', 'upgradeDomainResponse_upgradeId' - Undocumented member.
--
-- 'performCheckOnly', 'upgradeDomainResponse_performCheckOnly' - When true, indicates that an upgrade eligibility check needs to be
-- performed. Does not actually perform the upgrade.
--
-- 'targetVersion', 'upgradeDomainResponse_targetVersion' - The version of OpenSearch that you intend to upgrade the domain to.
--
-- 'advancedOptions', 'upgradeDomainResponse_advancedOptions' - Undocumented member.
--
-- 'httpStatus', 'upgradeDomainResponse_httpStatus' - The response's http status code.
newUpgradeDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpgradeDomainResponse
newUpgradeDomainResponse pHttpStatus_ =
  UpgradeDomainResponse'
    { domainName =
        Prelude.Nothing,
      upgradeId = Prelude.Nothing,
      performCheckOnly = Prelude.Nothing,
      targetVersion = Prelude.Nothing,
      advancedOptions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
upgradeDomainResponse_domainName :: Lens.Lens' UpgradeDomainResponse (Prelude.Maybe Prelude.Text)
upgradeDomainResponse_domainName = Lens.lens (\UpgradeDomainResponse' {domainName} -> domainName) (\s@UpgradeDomainResponse' {} a -> s {domainName = a} :: UpgradeDomainResponse)

-- | Undocumented member.
upgradeDomainResponse_upgradeId :: Lens.Lens' UpgradeDomainResponse (Prelude.Maybe Prelude.Text)
upgradeDomainResponse_upgradeId = Lens.lens (\UpgradeDomainResponse' {upgradeId} -> upgradeId) (\s@UpgradeDomainResponse' {} a -> s {upgradeId = a} :: UpgradeDomainResponse)

-- | When true, indicates that an upgrade eligibility check needs to be
-- performed. Does not actually perform the upgrade.
upgradeDomainResponse_performCheckOnly :: Lens.Lens' UpgradeDomainResponse (Prelude.Maybe Prelude.Bool)
upgradeDomainResponse_performCheckOnly = Lens.lens (\UpgradeDomainResponse' {performCheckOnly} -> performCheckOnly) (\s@UpgradeDomainResponse' {} a -> s {performCheckOnly = a} :: UpgradeDomainResponse)

-- | The version of OpenSearch that you intend to upgrade the domain to.
upgradeDomainResponse_targetVersion :: Lens.Lens' UpgradeDomainResponse (Prelude.Maybe Prelude.Text)
upgradeDomainResponse_targetVersion = Lens.lens (\UpgradeDomainResponse' {targetVersion} -> targetVersion) (\s@UpgradeDomainResponse' {} a -> s {targetVersion = a} :: UpgradeDomainResponse)

-- | Undocumented member.
upgradeDomainResponse_advancedOptions :: Lens.Lens' UpgradeDomainResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
upgradeDomainResponse_advancedOptions = Lens.lens (\UpgradeDomainResponse' {advancedOptions} -> advancedOptions) (\s@UpgradeDomainResponse' {} a -> s {advancedOptions = a} :: UpgradeDomainResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
upgradeDomainResponse_httpStatus :: Lens.Lens' UpgradeDomainResponse Prelude.Int
upgradeDomainResponse_httpStatus = Lens.lens (\UpgradeDomainResponse' {httpStatus} -> httpStatus) (\s@UpgradeDomainResponse' {} a -> s {httpStatus = a} :: UpgradeDomainResponse)

instance Prelude.NFData UpgradeDomainResponse
