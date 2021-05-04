{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutRemediationConfigurations' smart constructor.
data PutRemediationConfigurations = PutRemediationConfigurations'
  { -- | A list of remediation configuration objects.
    remediationConfigurations :: [RemediationConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
putRemediationConfigurations_remediationConfigurations = Lens.lens (\PutRemediationConfigurations' {remediationConfigurations} -> remediationConfigurations) (\s@PutRemediationConfigurations' {} a -> s {remediationConfigurations = a} :: PutRemediationConfigurations) Prelude.. Prelude._Coerce

instance
  Prelude.AWSRequest
    PutRemediationConfigurations
  where
  type
    Rs PutRemediationConfigurations =
      PutRemediationConfigurationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutRemediationConfigurationsResponse'
            Prelude.<$> ( x Prelude..?> "FailedBatches"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutRemediationConfigurations

instance Prelude.NFData PutRemediationConfigurations

instance
  Prelude.ToHeaders
    PutRemediationConfigurations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StarlingDoveService.PutRemediationConfigurations" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON PutRemediationConfigurations where
  toJSON PutRemediationConfigurations' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "RemediationConfigurations"
                  Prelude..= remediationConfigurations
              )
          ]
      )

instance Prelude.ToPath PutRemediationConfigurations where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutRemediationConfigurations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutRemediationConfigurationsResponse' smart constructor.
data PutRemediationConfigurationsResponse = PutRemediationConfigurationsResponse'
  { -- | Returns a list of failed remediation batch objects.
    failedBatches :: Prelude.Maybe [FailedRemediationBatch],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
putRemediationConfigurationsResponse_failedBatches = Lens.lens (\PutRemediationConfigurationsResponse' {failedBatches} -> failedBatches) (\s@PutRemediationConfigurationsResponse' {} a -> s {failedBatches = a} :: PutRemediationConfigurationsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
putRemediationConfigurationsResponse_httpStatus :: Lens.Lens' PutRemediationConfigurationsResponse Prelude.Int
putRemediationConfigurationsResponse_httpStatus = Lens.lens (\PutRemediationConfigurationsResponse' {httpStatus} -> httpStatus) (\s@PutRemediationConfigurationsResponse' {} a -> s {httpStatus = a} :: PutRemediationConfigurationsResponse)

instance
  Prelude.NFData
    PutRemediationConfigurationsResponse
