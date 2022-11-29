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
-- Module      : Amazonka.ElasticSearch.GetUpgradeStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the latest status of the last upgrade or upgrade eligibility
-- check that was performed on the domain.
module Amazonka.ElasticSearch.GetUpgradeStatus
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
import Amazonka.ElasticSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for request parameters to @ GetUpgradeStatus @ operation.
--
-- /See:/ 'newGetUpgradeStatus' smart constructor.
data GetUpgradeStatus = GetUpgradeStatus'
  { domainName :: Prelude.Text
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
-- 'domainName', 'getUpgradeStatus_domainName' - Undocumented member.
newGetUpgradeStatus ::
  -- | 'domainName'
  Prelude.Text ->
  GetUpgradeStatus
newGetUpgradeStatus pDomainName_ =
  GetUpgradeStatus' {domainName = pDomainName_}

-- | Undocumented member.
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
            Prelude.<$> (x Core..?> "UpgradeStep")
            Prelude.<*> (x Core..?> "UpgradeName")
            Prelude.<*> (x Core..?> "StepStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetUpgradeStatus where
  hashWithSalt _salt GetUpgradeStatus' {..} =
    _salt `Prelude.hashWithSalt` domainName

instance Prelude.NFData GetUpgradeStatus where
  rnf GetUpgradeStatus' {..} = Prelude.rnf domainName

instance Core.ToHeaders GetUpgradeStatus where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetUpgradeStatus where
  toPath GetUpgradeStatus' {..} =
    Prelude.mconcat
      [ "/2015-01-01/es/upgradeDomain/",
        Core.toBS domainName,
        "/status"
      ]

instance Core.ToQuery GetUpgradeStatus where
  toQuery = Prelude.const Prelude.mempty

-- | Container for response returned by @ GetUpgradeStatus @ operation.
--
-- /See:/ 'newGetUpgradeStatusResponse' smart constructor.
data GetUpgradeStatusResponse = GetUpgradeStatusResponse'
  { -- | Represents one of 3 steps that an Upgrade or Upgrade Eligibility Check
    -- does through:
    --
    -- -   PreUpgradeCheck
    -- -   Snapshot
    -- -   Upgrade
    upgradeStep :: Prelude.Maybe UpgradeStep,
    -- | A string that describes the update briefly
    upgradeName :: Prelude.Maybe Prelude.Text,
    -- | One of 4 statuses that a step can go through returned as part of the
    -- @ GetUpgradeStatusResponse @ object. The status can take one of the
    -- following values:
    --
    -- -   In Progress
    -- -   Succeeded
    -- -   Succeeded with Issues
    -- -   Failed
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
-- 'upgradeStep', 'getUpgradeStatusResponse_upgradeStep' - Represents one of 3 steps that an Upgrade or Upgrade Eligibility Check
-- does through:
--
-- -   PreUpgradeCheck
-- -   Snapshot
-- -   Upgrade
--
-- 'upgradeName', 'getUpgradeStatusResponse_upgradeName' - A string that describes the update briefly
--
-- 'stepStatus', 'getUpgradeStatusResponse_stepStatus' - One of 4 statuses that a step can go through returned as part of the
-- @ GetUpgradeStatusResponse @ object. The status can take one of the
-- following values:
--
-- -   In Progress
-- -   Succeeded
-- -   Succeeded with Issues
-- -   Failed
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

-- | Represents one of 3 steps that an Upgrade or Upgrade Eligibility Check
-- does through:
--
-- -   PreUpgradeCheck
-- -   Snapshot
-- -   Upgrade
getUpgradeStatusResponse_upgradeStep :: Lens.Lens' GetUpgradeStatusResponse (Prelude.Maybe UpgradeStep)
getUpgradeStatusResponse_upgradeStep = Lens.lens (\GetUpgradeStatusResponse' {upgradeStep} -> upgradeStep) (\s@GetUpgradeStatusResponse' {} a -> s {upgradeStep = a} :: GetUpgradeStatusResponse)

-- | A string that describes the update briefly
getUpgradeStatusResponse_upgradeName :: Lens.Lens' GetUpgradeStatusResponse (Prelude.Maybe Prelude.Text)
getUpgradeStatusResponse_upgradeName = Lens.lens (\GetUpgradeStatusResponse' {upgradeName} -> upgradeName) (\s@GetUpgradeStatusResponse' {} a -> s {upgradeName = a} :: GetUpgradeStatusResponse)

-- | One of 4 statuses that a step can go through returned as part of the
-- @ GetUpgradeStatusResponse @ object. The status can take one of the
-- following values:
--
-- -   In Progress
-- -   Succeeded
-- -   Succeeded with Issues
-- -   Failed
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
