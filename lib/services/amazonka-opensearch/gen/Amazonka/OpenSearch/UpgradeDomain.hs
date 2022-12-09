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
-- Module      : Amazonka.OpenSearch.UpgradeDomain
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows you to either upgrade your Amazon OpenSearch Service domain or
-- perform an upgrade eligibility check to a compatible version of
-- OpenSearch or Elasticsearch.
module Amazonka.OpenSearch.UpgradeDomain
  ( -- * Creating a Request
    UpgradeDomain (..),
    newUpgradeDomain,

    -- * Request Lenses
    upgradeDomain_advancedOptions,
    upgradeDomain_performCheckOnly,
    upgradeDomain_domainName,
    upgradeDomain_targetVersion,

    -- * Destructuring the Response
    UpgradeDomainResponse (..),
    newUpgradeDomainResponse,

    -- * Response Lenses
    upgradeDomainResponse_advancedOptions,
    upgradeDomainResponse_changeProgressDetails,
    upgradeDomainResponse_domainName,
    upgradeDomainResponse_performCheckOnly,
    upgradeDomainResponse_targetVersion,
    upgradeDomainResponse_upgradeId,
    upgradeDomainResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the request parameters to the @UpgradeDomain@ operation.
--
-- /See:/ 'newUpgradeDomain' smart constructor.
data UpgradeDomain = UpgradeDomain'
  { -- | Only supports the @override_main_response_version@ parameter and not
    -- other advanced options. You can only include this option when upgrading
    -- to an OpenSearch version. Specifies whether the domain reports its
    -- version as 7.10 so that it continues to work with Elasticsearch OSS
    -- clients and plugins.
    advancedOptions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | When true, indicates that an upgrade eligibility check needs to be
    -- performed. Does not actually perform the upgrade.
    performCheckOnly :: Prelude.Maybe Prelude.Bool,
    -- | Name of the OpenSearch Service domain that you want to upgrade.
    domainName :: Prelude.Text,
    -- | OpenSearch or Elasticsearch version to which you want to upgrade, in the
    -- format Opensearch_X.Y or Elasticsearch_X.Y.
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
-- 'advancedOptions', 'upgradeDomain_advancedOptions' - Only supports the @override_main_response_version@ parameter and not
-- other advanced options. You can only include this option when upgrading
-- to an OpenSearch version. Specifies whether the domain reports its
-- version as 7.10 so that it continues to work with Elasticsearch OSS
-- clients and plugins.
--
-- 'performCheckOnly', 'upgradeDomain_performCheckOnly' - When true, indicates that an upgrade eligibility check needs to be
-- performed. Does not actually perform the upgrade.
--
-- 'domainName', 'upgradeDomain_domainName' - Name of the OpenSearch Service domain that you want to upgrade.
--
-- 'targetVersion', 'upgradeDomain_targetVersion' - OpenSearch or Elasticsearch version to which you want to upgrade, in the
-- format Opensearch_X.Y or Elasticsearch_X.Y.
newUpgradeDomain ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'targetVersion'
  Prelude.Text ->
  UpgradeDomain
newUpgradeDomain pDomainName_ pTargetVersion_ =
  UpgradeDomain'
    { advancedOptions = Prelude.Nothing,
      performCheckOnly = Prelude.Nothing,
      domainName = pDomainName_,
      targetVersion = pTargetVersion_
    }

-- | Only supports the @override_main_response_version@ parameter and not
-- other advanced options. You can only include this option when upgrading
-- to an OpenSearch version. Specifies whether the domain reports its
-- version as 7.10 so that it continues to work with Elasticsearch OSS
-- clients and plugins.
upgradeDomain_advancedOptions :: Lens.Lens' UpgradeDomain (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
upgradeDomain_advancedOptions = Lens.lens (\UpgradeDomain' {advancedOptions} -> advancedOptions) (\s@UpgradeDomain' {} a -> s {advancedOptions = a} :: UpgradeDomain) Prelude.. Lens.mapping Lens.coerced

-- | When true, indicates that an upgrade eligibility check needs to be
-- performed. Does not actually perform the upgrade.
upgradeDomain_performCheckOnly :: Lens.Lens' UpgradeDomain (Prelude.Maybe Prelude.Bool)
upgradeDomain_performCheckOnly = Lens.lens (\UpgradeDomain' {performCheckOnly} -> performCheckOnly) (\s@UpgradeDomain' {} a -> s {performCheckOnly = a} :: UpgradeDomain)

-- | Name of the OpenSearch Service domain that you want to upgrade.
upgradeDomain_domainName :: Lens.Lens' UpgradeDomain Prelude.Text
upgradeDomain_domainName = Lens.lens (\UpgradeDomain' {domainName} -> domainName) (\s@UpgradeDomain' {} a -> s {domainName = a} :: UpgradeDomain)

-- | OpenSearch or Elasticsearch version to which you want to upgrade, in the
-- format Opensearch_X.Y or Elasticsearch_X.Y.
upgradeDomain_targetVersion :: Lens.Lens' UpgradeDomain Prelude.Text
upgradeDomain_targetVersion = Lens.lens (\UpgradeDomain' {targetVersion} -> targetVersion) (\s@UpgradeDomain' {} a -> s {targetVersion = a} :: UpgradeDomain)

instance Core.AWSRequest UpgradeDomain where
  type
    AWSResponse UpgradeDomain =
      UpgradeDomainResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpgradeDomainResponse'
            Prelude.<$> ( x Data..?> "AdvancedOptions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "ChangeProgressDetails")
            Prelude.<*> (x Data..?> "DomainName")
            Prelude.<*> (x Data..?> "PerformCheckOnly")
            Prelude.<*> (x Data..?> "TargetVersion")
            Prelude.<*> (x Data..?> "UpgradeId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpgradeDomain where
  hashWithSalt _salt UpgradeDomain' {..} =
    _salt `Prelude.hashWithSalt` advancedOptions
      `Prelude.hashWithSalt` performCheckOnly
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` targetVersion

instance Prelude.NFData UpgradeDomain where
  rnf UpgradeDomain' {..} =
    Prelude.rnf advancedOptions
      `Prelude.seq` Prelude.rnf performCheckOnly
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf targetVersion

instance Data.ToHeaders UpgradeDomain where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpgradeDomain where
  toJSON UpgradeDomain' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdvancedOptions" Data..=)
              Prelude.<$> advancedOptions,
            ("PerformCheckOnly" Data..=)
              Prelude.<$> performCheckOnly,
            Prelude.Just ("DomainName" Data..= domainName),
            Prelude.Just
              ("TargetVersion" Data..= targetVersion)
          ]
      )

instance Data.ToPath UpgradeDomain where
  toPath =
    Prelude.const
      "/2021-01-01/opensearch/upgradeDomain"

instance Data.ToQuery UpgradeDomain where
  toQuery = Prelude.const Prelude.mempty

-- | Container for the response returned by @UpgradeDomain@ operation.
--
-- /See:/ 'newUpgradeDomainResponse' smart constructor.
data UpgradeDomainResponse = UpgradeDomainResponse'
  { -- | The advanced options configuration for the domain.
    advancedOptions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Container for information about a configuration change happening on a
    -- domain.
    changeProgressDetails :: Prelude.Maybe ChangeProgressDetails,
    -- | The name of the domain that was upgraded.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | When true, indicates that an upgrade eligibility check was performed.
    performCheckOnly :: Prelude.Maybe Prelude.Bool,
    -- | OpenSearch or Elasticsearch version that the domain was upgraded to.
    targetVersion :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the domain upgrade.
    upgradeId :: Prelude.Maybe Prelude.Text,
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
-- 'advancedOptions', 'upgradeDomainResponse_advancedOptions' - The advanced options configuration for the domain.
--
-- 'changeProgressDetails', 'upgradeDomainResponse_changeProgressDetails' - Container for information about a configuration change happening on a
-- domain.
--
-- 'domainName', 'upgradeDomainResponse_domainName' - The name of the domain that was upgraded.
--
-- 'performCheckOnly', 'upgradeDomainResponse_performCheckOnly' - When true, indicates that an upgrade eligibility check was performed.
--
-- 'targetVersion', 'upgradeDomainResponse_targetVersion' - OpenSearch or Elasticsearch version that the domain was upgraded to.
--
-- 'upgradeId', 'upgradeDomainResponse_upgradeId' - The unique identifier of the domain upgrade.
--
-- 'httpStatus', 'upgradeDomainResponse_httpStatus' - The response's http status code.
newUpgradeDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpgradeDomainResponse
newUpgradeDomainResponse pHttpStatus_ =
  UpgradeDomainResponse'
    { advancedOptions =
        Prelude.Nothing,
      changeProgressDetails = Prelude.Nothing,
      domainName = Prelude.Nothing,
      performCheckOnly = Prelude.Nothing,
      targetVersion = Prelude.Nothing,
      upgradeId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The advanced options configuration for the domain.
upgradeDomainResponse_advancedOptions :: Lens.Lens' UpgradeDomainResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
upgradeDomainResponse_advancedOptions = Lens.lens (\UpgradeDomainResponse' {advancedOptions} -> advancedOptions) (\s@UpgradeDomainResponse' {} a -> s {advancedOptions = a} :: UpgradeDomainResponse) Prelude.. Lens.mapping Lens.coerced

-- | Container for information about a configuration change happening on a
-- domain.
upgradeDomainResponse_changeProgressDetails :: Lens.Lens' UpgradeDomainResponse (Prelude.Maybe ChangeProgressDetails)
upgradeDomainResponse_changeProgressDetails = Lens.lens (\UpgradeDomainResponse' {changeProgressDetails} -> changeProgressDetails) (\s@UpgradeDomainResponse' {} a -> s {changeProgressDetails = a} :: UpgradeDomainResponse)

-- | The name of the domain that was upgraded.
upgradeDomainResponse_domainName :: Lens.Lens' UpgradeDomainResponse (Prelude.Maybe Prelude.Text)
upgradeDomainResponse_domainName = Lens.lens (\UpgradeDomainResponse' {domainName} -> domainName) (\s@UpgradeDomainResponse' {} a -> s {domainName = a} :: UpgradeDomainResponse)

-- | When true, indicates that an upgrade eligibility check was performed.
upgradeDomainResponse_performCheckOnly :: Lens.Lens' UpgradeDomainResponse (Prelude.Maybe Prelude.Bool)
upgradeDomainResponse_performCheckOnly = Lens.lens (\UpgradeDomainResponse' {performCheckOnly} -> performCheckOnly) (\s@UpgradeDomainResponse' {} a -> s {performCheckOnly = a} :: UpgradeDomainResponse)

-- | OpenSearch or Elasticsearch version that the domain was upgraded to.
upgradeDomainResponse_targetVersion :: Lens.Lens' UpgradeDomainResponse (Prelude.Maybe Prelude.Text)
upgradeDomainResponse_targetVersion = Lens.lens (\UpgradeDomainResponse' {targetVersion} -> targetVersion) (\s@UpgradeDomainResponse' {} a -> s {targetVersion = a} :: UpgradeDomainResponse)

-- | The unique identifier of the domain upgrade.
upgradeDomainResponse_upgradeId :: Lens.Lens' UpgradeDomainResponse (Prelude.Maybe Prelude.Text)
upgradeDomainResponse_upgradeId = Lens.lens (\UpgradeDomainResponse' {upgradeId} -> upgradeId) (\s@UpgradeDomainResponse' {} a -> s {upgradeId = a} :: UpgradeDomainResponse)

-- | The response's http status code.
upgradeDomainResponse_httpStatus :: Lens.Lens' UpgradeDomainResponse Prelude.Int
upgradeDomainResponse_httpStatus = Lens.lens (\UpgradeDomainResponse' {httpStatus} -> httpStatus) (\s@UpgradeDomainResponse' {} a -> s {httpStatus = a} :: UpgradeDomainResponse)

instance Prelude.NFData UpgradeDomainResponse where
  rnf UpgradeDomainResponse' {..} =
    Prelude.rnf advancedOptions
      `Prelude.seq` Prelude.rnf changeProgressDetails
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf performCheckOnly
      `Prelude.seq` Prelude.rnf targetVersion
      `Prelude.seq` Prelude.rnf upgradeId
      `Prelude.seq` Prelude.rnf httpStatus
