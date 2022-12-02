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
-- Module      : Amazonka.Config.PutRemediationConfigurations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates the remediation configuration with a specific Config
-- rule with the selected target or action. The API creates the
-- @RemediationConfiguration@ object for the Config rule. The Config rule
-- must already exist for you to add a remediation configuration. The
-- target (SSM document) must exist and have permissions to use the target.
--
-- If you make backward incompatible changes to the SSM document, you must
-- call this again to ensure the remediations can run.
--
-- This API does not support adding remediation configurations for
-- service-linked Config Rules such as Organization Config rules, the rules
-- deployed by conformance packs, and rules deployed by Amazon Web Services
-- Security Hub.
--
-- For manual remediation configuration, you need to provide a value for
-- @automationAssumeRole@ or use a value in the @assumeRole@field to
-- remediate your resources. The SSM automation document can use either as
-- long as it maps to a valid parameter.
--
-- However, for automatic remediation configuration, the only valid
-- @assumeRole@ field value is @AutomationAssumeRole@ and you need to
-- provide a value for @AutomationAssumeRole@ to remediate your resources.
module Amazonka.Config.PutRemediationConfigurations
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

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutRemediationConfigurations' smart constructor.
data PutRemediationConfigurations = PutRemediationConfigurations'
  { -- | A list of remediation configuration objects.
    remediationConfigurations :: [RemediationConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.mempty
    }

-- | A list of remediation configuration objects.
putRemediationConfigurations_remediationConfigurations :: Lens.Lens' PutRemediationConfigurations [RemediationConfiguration]
putRemediationConfigurations_remediationConfigurations = Lens.lens (\PutRemediationConfigurations' {remediationConfigurations} -> remediationConfigurations) (\s@PutRemediationConfigurations' {} a -> s {remediationConfigurations = a} :: PutRemediationConfigurations) Prelude.. Lens.coerced

instance Core.AWSRequest PutRemediationConfigurations where
  type
    AWSResponse PutRemediationConfigurations =
      PutRemediationConfigurationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutRemediationConfigurationsResponse'
            Prelude.<$> (x Data..?> "FailedBatches" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutRemediationConfigurations
  where
  hashWithSalt _salt PutRemediationConfigurations' {..} =
    _salt
      `Prelude.hashWithSalt` remediationConfigurations

instance Prelude.NFData PutRemediationConfigurations where
  rnf PutRemediationConfigurations' {..} =
    Prelude.rnf remediationConfigurations

instance Data.ToHeaders PutRemediationConfigurations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.PutRemediationConfigurations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutRemediationConfigurations where
  toJSON PutRemediationConfigurations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "RemediationConfigurations"
                  Data..= remediationConfigurations
              )
          ]
      )

instance Data.ToPath PutRemediationConfigurations where
  toPath = Prelude.const "/"

instance Data.ToQuery PutRemediationConfigurations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutRemediationConfigurationsResponse' smart constructor.
data PutRemediationConfigurationsResponse = PutRemediationConfigurationsResponse'
  { -- | Returns a list of failed remediation batch objects.
    failedBatches :: Prelude.Maybe [FailedRemediationBatch],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  PutRemediationConfigurationsResponse
newPutRemediationConfigurationsResponse pHttpStatus_ =
  PutRemediationConfigurationsResponse'
    { failedBatches =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns a list of failed remediation batch objects.
putRemediationConfigurationsResponse_failedBatches :: Lens.Lens' PutRemediationConfigurationsResponse (Prelude.Maybe [FailedRemediationBatch])
putRemediationConfigurationsResponse_failedBatches = Lens.lens (\PutRemediationConfigurationsResponse' {failedBatches} -> failedBatches) (\s@PutRemediationConfigurationsResponse' {} a -> s {failedBatches = a} :: PutRemediationConfigurationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
putRemediationConfigurationsResponse_httpStatus :: Lens.Lens' PutRemediationConfigurationsResponse Prelude.Int
putRemediationConfigurationsResponse_httpStatus = Lens.lens (\PutRemediationConfigurationsResponse' {httpStatus} -> httpStatus) (\s@PutRemediationConfigurationsResponse' {} a -> s {httpStatus = a} :: PutRemediationConfigurationsResponse)

instance
  Prelude.NFData
    PutRemediationConfigurationsResponse
  where
  rnf PutRemediationConfigurationsResponse' {..} =
    Prelude.rnf failedBatches
      `Prelude.seq` Prelude.rnf httpStatus
