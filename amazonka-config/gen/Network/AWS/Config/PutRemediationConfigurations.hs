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
-- Module      : Network.AWS.Config.PutRemediationConfigurations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates the remediation configuration with a specific AWS Config
-- rule with the selected target or action. The API creates the
-- @RemediationConfiguration@ object for the AWS Config rule. The AWS
-- Config rule must already exist for you to add a remediation
-- configuration. The target (SSM document) must exist and have permissions
-- to use the target.
--
-- If you make backward incompatible changes to the SSM document, you must
-- call this again to ensure the remediations can run.
--
-- This API does not support adding remediation configurations for
-- service-linked AWS Config Rules such as Organization Config rules, the
-- rules deployed by conformance packs, and rules deployed by AWS Security
-- Hub.
module Network.AWS.Config.PutRemediationConfigurations
  ( -- * Creating a Request
    PutRemediationConfigurations (..),
    newPutRemediationConfigurations,

    -- * Request Lenses
    putRemediationConfigurations_remediationConfigurations,

    -- * Destructuring the Response
    PutRemediationConfigurationsResponse (..),
    newPutRemediationConfigurationsResponse,

    -- * Response Lenses
    putRemediationConfigurationsResponse_failedBatches,
    putRemediationConfigurationsResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutRemediationConfigurations' smart constructor.
data PutRemediationConfigurations = PutRemediationConfigurations'
  { -- | A list of remediation configuration objects.
    remediationConfigurations :: [RemediationConfiguration]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutRemediationConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'remediationConfigurations', 'putRemediationConfigurations_remediationConfigurations' - A list of remediation configuration objects.
newPutRemediationConfigurations ::
  PutRemediationConfigurations
newPutRemediationConfigurations =
  PutRemediationConfigurations'
    { remediationConfigurations =
        Core.mempty
    }

-- | A list of remediation configuration objects.
putRemediationConfigurations_remediationConfigurations :: Lens.Lens' PutRemediationConfigurations [RemediationConfiguration]
putRemediationConfigurations_remediationConfigurations = Lens.lens (\PutRemediationConfigurations' {remediationConfigurations} -> remediationConfigurations) (\s@PutRemediationConfigurations' {} a -> s {remediationConfigurations = a} :: PutRemediationConfigurations) Core.. Lens._Coerce

instance Core.AWSRequest PutRemediationConfigurations where
  type
    AWSResponse PutRemediationConfigurations =
      PutRemediationConfigurationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutRemediationConfigurationsResponse'
            Core.<$> (x Core..?> "FailedBatches" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutRemediationConfigurations

instance Core.NFData PutRemediationConfigurations

instance Core.ToHeaders PutRemediationConfigurations where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.PutRemediationConfigurations" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutRemediationConfigurations where
  toJSON PutRemediationConfigurations' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "RemediationConfigurations"
                  Core..= remediationConfigurations
              )
          ]
      )

instance Core.ToPath PutRemediationConfigurations where
  toPath = Core.const "/"

instance Core.ToQuery PutRemediationConfigurations where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutRemediationConfigurationsResponse' smart constructor.
data PutRemediationConfigurationsResponse = PutRemediationConfigurationsResponse'
  { -- | Returns a list of failed remediation batch objects.
    failedBatches :: Core.Maybe [FailedRemediationBatch],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutRemediationConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedBatches', 'putRemediationConfigurationsResponse_failedBatches' - Returns a list of failed remediation batch objects.
--
-- 'httpStatus', 'putRemediationConfigurationsResponse_httpStatus' - The response's http status code.
newPutRemediationConfigurationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutRemediationConfigurationsResponse
newPutRemediationConfigurationsResponse pHttpStatus_ =
  PutRemediationConfigurationsResponse'
    { failedBatches =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns a list of failed remediation batch objects.
putRemediationConfigurationsResponse_failedBatches :: Lens.Lens' PutRemediationConfigurationsResponse (Core.Maybe [FailedRemediationBatch])
putRemediationConfigurationsResponse_failedBatches = Lens.lens (\PutRemediationConfigurationsResponse' {failedBatches} -> failedBatches) (\s@PutRemediationConfigurationsResponse' {} a -> s {failedBatches = a} :: PutRemediationConfigurationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
putRemediationConfigurationsResponse_httpStatus :: Lens.Lens' PutRemediationConfigurationsResponse Core.Int
putRemediationConfigurationsResponse_httpStatus = Lens.lens (\PutRemediationConfigurationsResponse' {httpStatus} -> httpStatus) (\s@PutRemediationConfigurationsResponse' {} a -> s {httpStatus = a} :: PutRemediationConfigurationsResponse)

instance
  Core.NFData
    PutRemediationConfigurationsResponse
