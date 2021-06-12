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
-- Module      : Network.AWS.ElasticSearch.GetUpgradeStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the latest status of the last upgrade or upgrade eligibility
-- check that was performed on the domain.
module Network.AWS.ElasticSearch.GetUpgradeStatus
  ( -- * Creating a Request
    GetUpgradeStatus (..),
    newGetUpgradeStatus,

    -- * Request Lenses
    getUpgradeStatus_domainName,

    -- * Destructuring the Response
    GetUpgradeStatusResponse (..),
    newGetUpgradeStatusResponse,

    -- * Response Lenses
    getUpgradeStatusResponse_upgradeName,
    getUpgradeStatusResponse_upgradeStep,
    getUpgradeStatusResponse_stepStatus,
    getUpgradeStatusResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for request parameters to @ GetUpgradeStatus @ operation.
--
-- /See:/ 'newGetUpgradeStatus' smart constructor.
data GetUpgradeStatus = GetUpgradeStatus'
  { domainName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetUpgradeStatus
newGetUpgradeStatus pDomainName_ =
  GetUpgradeStatus' {domainName = pDomainName_}

-- | Undocumented member.
getUpgradeStatus_domainName :: Lens.Lens' GetUpgradeStatus Core.Text
getUpgradeStatus_domainName = Lens.lens (\GetUpgradeStatus' {domainName} -> domainName) (\s@GetUpgradeStatus' {} a -> s {domainName = a} :: GetUpgradeStatus)

instance Core.AWSRequest GetUpgradeStatus where
  type
    AWSResponse GetUpgradeStatus =
      GetUpgradeStatusResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUpgradeStatusResponse'
            Core.<$> (x Core..?> "UpgradeName")
            Core.<*> (x Core..?> "UpgradeStep")
            Core.<*> (x Core..?> "StepStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetUpgradeStatus

instance Core.NFData GetUpgradeStatus

instance Core.ToHeaders GetUpgradeStatus where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetUpgradeStatus where
  toPath GetUpgradeStatus' {..} =
    Core.mconcat
      [ "/2015-01-01/es/upgradeDomain/",
        Core.toBS domainName,
        "/status"
      ]

instance Core.ToQuery GetUpgradeStatus where
  toQuery = Core.const Core.mempty

-- | Container for response returned by @ GetUpgradeStatus @ operation.
--
-- /See:/ 'newGetUpgradeStatusResponse' smart constructor.
data GetUpgradeStatusResponse = GetUpgradeStatusResponse'
  { -- | A string that describes the update briefly
    upgradeName :: Core.Maybe Core.Text,
    -- | Represents one of 3 steps that an Upgrade or Upgrade Eligibility Check
    -- does through:
    --
    -- -   PreUpgradeCheck
    -- -   Snapshot
    -- -   Upgrade
    upgradeStep :: Core.Maybe UpgradeStep,
    -- | One of 4 statuses that a step can go through returned as part of the
    -- @ GetUpgradeStatusResponse @ object. The status can take one of the
    -- following values:
    --
    -- -   In Progress
    -- -   Succeeded
    -- -   Succeeded with Issues
    -- -   Failed
    stepStatus :: Core.Maybe UpgradeStatus,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetUpgradeStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'upgradeName', 'getUpgradeStatusResponse_upgradeName' - A string that describes the update briefly
--
-- 'upgradeStep', 'getUpgradeStatusResponse_upgradeStep' - Represents one of 3 steps that an Upgrade or Upgrade Eligibility Check
-- does through:
--
-- -   PreUpgradeCheck
-- -   Snapshot
-- -   Upgrade
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
  Core.Int ->
  GetUpgradeStatusResponse
newGetUpgradeStatusResponse pHttpStatus_ =
  GetUpgradeStatusResponse'
    { upgradeName =
        Core.Nothing,
      upgradeStep = Core.Nothing,
      stepStatus = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A string that describes the update briefly
getUpgradeStatusResponse_upgradeName :: Lens.Lens' GetUpgradeStatusResponse (Core.Maybe Core.Text)
getUpgradeStatusResponse_upgradeName = Lens.lens (\GetUpgradeStatusResponse' {upgradeName} -> upgradeName) (\s@GetUpgradeStatusResponse' {} a -> s {upgradeName = a} :: GetUpgradeStatusResponse)

-- | Represents one of 3 steps that an Upgrade or Upgrade Eligibility Check
-- does through:
--
-- -   PreUpgradeCheck
-- -   Snapshot
-- -   Upgrade
getUpgradeStatusResponse_upgradeStep :: Lens.Lens' GetUpgradeStatusResponse (Core.Maybe UpgradeStep)
getUpgradeStatusResponse_upgradeStep = Lens.lens (\GetUpgradeStatusResponse' {upgradeStep} -> upgradeStep) (\s@GetUpgradeStatusResponse' {} a -> s {upgradeStep = a} :: GetUpgradeStatusResponse)

-- | One of 4 statuses that a step can go through returned as part of the
-- @ GetUpgradeStatusResponse @ object. The status can take one of the
-- following values:
--
-- -   In Progress
-- -   Succeeded
-- -   Succeeded with Issues
-- -   Failed
getUpgradeStatusResponse_stepStatus :: Lens.Lens' GetUpgradeStatusResponse (Core.Maybe UpgradeStatus)
getUpgradeStatusResponse_stepStatus = Lens.lens (\GetUpgradeStatusResponse' {stepStatus} -> stepStatus) (\s@GetUpgradeStatusResponse' {} a -> s {stepStatus = a} :: GetUpgradeStatusResponse)

-- | The response's http status code.
getUpgradeStatusResponse_httpStatus :: Lens.Lens' GetUpgradeStatusResponse Core.Int
getUpgradeStatusResponse_httpStatus = Lens.lens (\GetUpgradeStatusResponse' {httpStatus} -> httpStatus) (\s@GetUpgradeStatusResponse' {} a -> s {httpStatus = a} :: GetUpgradeStatusResponse)

instance Core.NFData GetUpgradeStatusResponse
