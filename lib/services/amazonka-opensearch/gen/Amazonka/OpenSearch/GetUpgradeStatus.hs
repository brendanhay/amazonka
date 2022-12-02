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
-- Module      : Amazonka.OpenSearch.GetUpgradeStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the most recent status of the last upgrade or upgrade
-- eligibility check performed on an Amazon OpenSearch Service domain.
module Amazonka.OpenSearch.GetUpgradeStatus
  ( -- * Creating a Request
    GetUpgradeStatus (..),
    newGetUpgradeStatus,

    -- * Request Lenses
    getUpgradeStatus_domainName,

    -- * Destructuring the Response
    GetUpgradeStatusResponse (..),
    newGetUpgradeStatusResponse,

    -- * Response Lenses
    getUpgradeStatusResponse_upgradeStep,
    getUpgradeStatusResponse_upgradeName,
    getUpgradeStatusResponse_stepStatus,
    getUpgradeStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the request parameters to the @GetUpgradeStatus@
-- operation.
--
-- /See:/ 'newGetUpgradeStatus' smart constructor.
data GetUpgradeStatus = GetUpgradeStatus'
  { -- | The domain of the domain to get upgrade status information for.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUpgradeStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'getUpgradeStatus_domainName' - The domain of the domain to get upgrade status information for.
newGetUpgradeStatus ::
  -- | 'domainName'
  Prelude.Text ->
  GetUpgradeStatus
newGetUpgradeStatus pDomainName_ =
  GetUpgradeStatus' {domainName = pDomainName_}

-- | The domain of the domain to get upgrade status information for.
getUpgradeStatus_domainName :: Lens.Lens' GetUpgradeStatus Prelude.Text
getUpgradeStatus_domainName = Lens.lens (\GetUpgradeStatus' {domainName} -> domainName) (\s@GetUpgradeStatus' {} a -> s {domainName = a} :: GetUpgradeStatus)

instance Core.AWSRequest GetUpgradeStatus where
  type
    AWSResponse GetUpgradeStatus =
      GetUpgradeStatusResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUpgradeStatusResponse'
            Prelude.<$> (x Data..?> "UpgradeStep")
            Prelude.<*> (x Data..?> "UpgradeName")
            Prelude.<*> (x Data..?> "StepStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetUpgradeStatus where
  hashWithSalt _salt GetUpgradeStatus' {..} =
    _salt `Prelude.hashWithSalt` domainName

instance Prelude.NFData GetUpgradeStatus where
  rnf GetUpgradeStatus' {..} = Prelude.rnf domainName

instance Data.ToHeaders GetUpgradeStatus where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetUpgradeStatus where
  toPath GetUpgradeStatus' {..} =
    Prelude.mconcat
      [ "/2021-01-01/opensearch/upgradeDomain/",
        Data.toBS domainName,
        "/status"
      ]

instance Data.ToQuery GetUpgradeStatus where
  toQuery = Prelude.const Prelude.mempty

-- | Container for the response returned by the @GetUpgradeStatus@ operation.
--
-- /See:/ 'newGetUpgradeStatusResponse' smart constructor.
data GetUpgradeStatusResponse = GetUpgradeStatusResponse'
  { -- | One of three steps that an upgrade or upgrade eligibility check goes
    -- through.
    upgradeStep :: Prelude.Maybe UpgradeStep,
    -- | A string that describes the update.
    upgradeName :: Prelude.Maybe Prelude.Text,
    -- | The status of the current step that an upgrade is on.
    stepStatus :: Prelude.Maybe UpgradeStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUpgradeStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'upgradeStep', 'getUpgradeStatusResponse_upgradeStep' - One of three steps that an upgrade or upgrade eligibility check goes
-- through.
--
-- 'upgradeName', 'getUpgradeStatusResponse_upgradeName' - A string that describes the update.
--
-- 'stepStatus', 'getUpgradeStatusResponse_stepStatus' - The status of the current step that an upgrade is on.
--
-- 'httpStatus', 'getUpgradeStatusResponse_httpStatus' - The response's http status code.
newGetUpgradeStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetUpgradeStatusResponse
newGetUpgradeStatusResponse pHttpStatus_ =
  GetUpgradeStatusResponse'
    { upgradeStep =
        Prelude.Nothing,
      upgradeName = Prelude.Nothing,
      stepStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | One of three steps that an upgrade or upgrade eligibility check goes
-- through.
getUpgradeStatusResponse_upgradeStep :: Lens.Lens' GetUpgradeStatusResponse (Prelude.Maybe UpgradeStep)
getUpgradeStatusResponse_upgradeStep = Lens.lens (\GetUpgradeStatusResponse' {upgradeStep} -> upgradeStep) (\s@GetUpgradeStatusResponse' {} a -> s {upgradeStep = a} :: GetUpgradeStatusResponse)

-- | A string that describes the update.
getUpgradeStatusResponse_upgradeName :: Lens.Lens' GetUpgradeStatusResponse (Prelude.Maybe Prelude.Text)
getUpgradeStatusResponse_upgradeName = Lens.lens (\GetUpgradeStatusResponse' {upgradeName} -> upgradeName) (\s@GetUpgradeStatusResponse' {} a -> s {upgradeName = a} :: GetUpgradeStatusResponse)

-- | The status of the current step that an upgrade is on.
getUpgradeStatusResponse_stepStatus :: Lens.Lens' GetUpgradeStatusResponse (Prelude.Maybe UpgradeStatus)
getUpgradeStatusResponse_stepStatus = Lens.lens (\GetUpgradeStatusResponse' {stepStatus} -> stepStatus) (\s@GetUpgradeStatusResponse' {} a -> s {stepStatus = a} :: GetUpgradeStatusResponse)

-- | The response's http status code.
getUpgradeStatusResponse_httpStatus :: Lens.Lens' GetUpgradeStatusResponse Prelude.Int
getUpgradeStatusResponse_httpStatus = Lens.lens (\GetUpgradeStatusResponse' {httpStatus} -> httpStatus) (\s@GetUpgradeStatusResponse' {} a -> s {httpStatus = a} :: GetUpgradeStatusResponse)

instance Prelude.NFData GetUpgradeStatusResponse where
  rnf GetUpgradeStatusResponse' {..} =
    Prelude.rnf upgradeStep
      `Prelude.seq` Prelude.rnf upgradeName
      `Prelude.seq` Prelude.rnf stepStatus
      `Prelude.seq` Prelude.rnf httpStatus
