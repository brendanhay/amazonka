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
-- Module      : Amazonka.ElasticSearch.UpgradeElasticsearchDomain
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows you to either upgrade your domain or perform an Upgrade
-- eligibility check to a compatible Elasticsearch version.
module Amazonka.ElasticSearch.UpgradeElasticsearchDomain
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
    upgradeElasticsearchDomainResponse_changeProgressDetails,
    upgradeElasticsearchDomainResponse_domainName,
    upgradeElasticsearchDomainResponse_targetVersion,
    upgradeElasticsearchDomainResponse_performCheckOnly,
    upgradeElasticsearchDomainResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for request parameters to @ UpgradeElasticsearchDomain @
-- operation.
--
-- /See:/ 'newUpgradeElasticsearchDomain' smart constructor.
data UpgradeElasticsearchDomain = UpgradeElasticsearchDomain'
  { -- | This flag, when set to True, indicates that an Upgrade Eligibility Check
    -- needs to be performed. This will not actually perform the Upgrade.
    performCheckOnly :: Prelude.Maybe Prelude.Bool,
    domainName :: Prelude.Text,
    -- | The version of Elasticsearch that you intend to upgrade the domain to.
    targetVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'targetVersion'
  Prelude.Text ->
  UpgradeElasticsearchDomain
newUpgradeElasticsearchDomain
  pDomainName_
  pTargetVersion_ =
    UpgradeElasticsearchDomain'
      { performCheckOnly =
          Prelude.Nothing,
        domainName = pDomainName_,
        targetVersion = pTargetVersion_
      }

-- | This flag, when set to True, indicates that an Upgrade Eligibility Check
-- needs to be performed. This will not actually perform the Upgrade.
upgradeElasticsearchDomain_performCheckOnly :: Lens.Lens' UpgradeElasticsearchDomain (Prelude.Maybe Prelude.Bool)
upgradeElasticsearchDomain_performCheckOnly = Lens.lens (\UpgradeElasticsearchDomain' {performCheckOnly} -> performCheckOnly) (\s@UpgradeElasticsearchDomain' {} a -> s {performCheckOnly = a} :: UpgradeElasticsearchDomain)

-- | Undocumented member.
upgradeElasticsearchDomain_domainName :: Lens.Lens' UpgradeElasticsearchDomain Prelude.Text
upgradeElasticsearchDomain_domainName = Lens.lens (\UpgradeElasticsearchDomain' {domainName} -> domainName) (\s@UpgradeElasticsearchDomain' {} a -> s {domainName = a} :: UpgradeElasticsearchDomain)

-- | The version of Elasticsearch that you intend to upgrade the domain to.
upgradeElasticsearchDomain_targetVersion :: Lens.Lens' UpgradeElasticsearchDomain Prelude.Text
upgradeElasticsearchDomain_targetVersion = Lens.lens (\UpgradeElasticsearchDomain' {targetVersion} -> targetVersion) (\s@UpgradeElasticsearchDomain' {} a -> s {targetVersion = a} :: UpgradeElasticsearchDomain)

instance Core.AWSRequest UpgradeElasticsearchDomain where
  type
    AWSResponse UpgradeElasticsearchDomain =
      UpgradeElasticsearchDomainResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpgradeElasticsearchDomainResponse'
            Prelude.<$> (x Data..?> "ChangeProgressDetails")
            Prelude.<*> (x Data..?> "DomainName")
            Prelude.<*> (x Data..?> "TargetVersion")
            Prelude.<*> (x Data..?> "PerformCheckOnly")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpgradeElasticsearchDomain where
  hashWithSalt _salt UpgradeElasticsearchDomain' {..} =
    _salt `Prelude.hashWithSalt` performCheckOnly
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` targetVersion

instance Prelude.NFData UpgradeElasticsearchDomain where
  rnf UpgradeElasticsearchDomain' {..} =
    Prelude.rnf performCheckOnly
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf targetVersion

instance Data.ToHeaders UpgradeElasticsearchDomain where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpgradeElasticsearchDomain where
  toJSON UpgradeElasticsearchDomain' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PerformCheckOnly" Data..=)
              Prelude.<$> performCheckOnly,
            Prelude.Just ("DomainName" Data..= domainName),
            Prelude.Just
              ("TargetVersion" Data..= targetVersion)
          ]
      )

instance Data.ToPath UpgradeElasticsearchDomain where
  toPath = Prelude.const "/2015-01-01/es/upgradeDomain"

instance Data.ToQuery UpgradeElasticsearchDomain where
  toQuery = Prelude.const Prelude.mempty

-- | Container for response returned by @ UpgradeElasticsearchDomain @
-- operation.
--
-- /See:/ 'newUpgradeElasticsearchDomainResponse' smart constructor.
data UpgradeElasticsearchDomainResponse = UpgradeElasticsearchDomainResponse'
  { changeProgressDetails :: Prelude.Maybe ChangeProgressDetails,
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The version of Elasticsearch that you intend to upgrade the domain to.
    targetVersion :: Prelude.Maybe Prelude.Text,
    -- | This flag, when set to True, indicates that an Upgrade Eligibility Check
    -- needs to be performed. This will not actually perform the Upgrade.
    performCheckOnly :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpgradeElasticsearchDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeProgressDetails', 'upgradeElasticsearchDomainResponse_changeProgressDetails' - Undocumented member.
--
-- 'domainName', 'upgradeElasticsearchDomainResponse_domainName' - Undocumented member.
--
-- 'targetVersion', 'upgradeElasticsearchDomainResponse_targetVersion' - The version of Elasticsearch that you intend to upgrade the domain to.
--
-- 'performCheckOnly', 'upgradeElasticsearchDomainResponse_performCheckOnly' - This flag, when set to True, indicates that an Upgrade Eligibility Check
-- needs to be performed. This will not actually perform the Upgrade.
--
-- 'httpStatus', 'upgradeElasticsearchDomainResponse_httpStatus' - The response's http status code.
newUpgradeElasticsearchDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpgradeElasticsearchDomainResponse
newUpgradeElasticsearchDomainResponse pHttpStatus_ =
  UpgradeElasticsearchDomainResponse'
    { changeProgressDetails =
        Prelude.Nothing,
      domainName = Prelude.Nothing,
      targetVersion = Prelude.Nothing,
      performCheckOnly = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
upgradeElasticsearchDomainResponse_changeProgressDetails :: Lens.Lens' UpgradeElasticsearchDomainResponse (Prelude.Maybe ChangeProgressDetails)
upgradeElasticsearchDomainResponse_changeProgressDetails = Lens.lens (\UpgradeElasticsearchDomainResponse' {changeProgressDetails} -> changeProgressDetails) (\s@UpgradeElasticsearchDomainResponse' {} a -> s {changeProgressDetails = a} :: UpgradeElasticsearchDomainResponse)

-- | Undocumented member.
upgradeElasticsearchDomainResponse_domainName :: Lens.Lens' UpgradeElasticsearchDomainResponse (Prelude.Maybe Prelude.Text)
upgradeElasticsearchDomainResponse_domainName = Lens.lens (\UpgradeElasticsearchDomainResponse' {domainName} -> domainName) (\s@UpgradeElasticsearchDomainResponse' {} a -> s {domainName = a} :: UpgradeElasticsearchDomainResponse)

-- | The version of Elasticsearch that you intend to upgrade the domain to.
upgradeElasticsearchDomainResponse_targetVersion :: Lens.Lens' UpgradeElasticsearchDomainResponse (Prelude.Maybe Prelude.Text)
upgradeElasticsearchDomainResponse_targetVersion = Lens.lens (\UpgradeElasticsearchDomainResponse' {targetVersion} -> targetVersion) (\s@UpgradeElasticsearchDomainResponse' {} a -> s {targetVersion = a} :: UpgradeElasticsearchDomainResponse)

-- | This flag, when set to True, indicates that an Upgrade Eligibility Check
-- needs to be performed. This will not actually perform the Upgrade.
upgradeElasticsearchDomainResponse_performCheckOnly :: Lens.Lens' UpgradeElasticsearchDomainResponse (Prelude.Maybe Prelude.Bool)
upgradeElasticsearchDomainResponse_performCheckOnly = Lens.lens (\UpgradeElasticsearchDomainResponse' {performCheckOnly} -> performCheckOnly) (\s@UpgradeElasticsearchDomainResponse' {} a -> s {performCheckOnly = a} :: UpgradeElasticsearchDomainResponse)

-- | The response's http status code.
upgradeElasticsearchDomainResponse_httpStatus :: Lens.Lens' UpgradeElasticsearchDomainResponse Prelude.Int
upgradeElasticsearchDomainResponse_httpStatus = Lens.lens (\UpgradeElasticsearchDomainResponse' {httpStatus} -> httpStatus) (\s@UpgradeElasticsearchDomainResponse' {} a -> s {httpStatus = a} :: UpgradeElasticsearchDomainResponse)

instance
  Prelude.NFData
    UpgradeElasticsearchDomainResponse
  where
  rnf UpgradeElasticsearchDomainResponse' {..} =
    Prelude.rnf changeProgressDetails
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf targetVersion
      `Prelude.seq` Prelude.rnf performCheckOnly
      `Prelude.seq` Prelude.rnf httpStatus
