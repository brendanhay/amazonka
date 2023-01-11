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
-- Module      : Amazonka.WorkMail.PutEmailMonitoringConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates the email monitoring configuration for a specified
-- organization.
module Amazonka.WorkMail.PutEmailMonitoringConfiguration
  ( -- * Creating a Request
    PutEmailMonitoringConfiguration (..),
    newPutEmailMonitoringConfiguration,

    -- * Request Lenses
    putEmailMonitoringConfiguration_organizationId,
    putEmailMonitoringConfiguration_roleArn,
    putEmailMonitoringConfiguration_logGroupArn,

    -- * Destructuring the Response
    PutEmailMonitoringConfigurationResponse (..),
    newPutEmailMonitoringConfigurationResponse,

    -- * Response Lenses
    putEmailMonitoringConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newPutEmailMonitoringConfiguration' smart constructor.
data PutEmailMonitoringConfiguration = PutEmailMonitoringConfiguration'
  { -- | The ID of the organization for which the email monitoring configuration
    -- is set.
    organizationId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM Role associated with the email
    -- monitoring configuration.
    roleArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the CloudWatch Log group associated
    -- with the email monitoring configuration.
    logGroupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutEmailMonitoringConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'putEmailMonitoringConfiguration_organizationId' - The ID of the organization for which the email monitoring configuration
-- is set.
--
-- 'roleArn', 'putEmailMonitoringConfiguration_roleArn' - The Amazon Resource Name (ARN) of the IAM Role associated with the email
-- monitoring configuration.
--
-- 'logGroupArn', 'putEmailMonitoringConfiguration_logGroupArn' - The Amazon Resource Name (ARN) of the CloudWatch Log group associated
-- with the email monitoring configuration.
newPutEmailMonitoringConfiguration ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'logGroupArn'
  Prelude.Text ->
  PutEmailMonitoringConfiguration
newPutEmailMonitoringConfiguration
  pOrganizationId_
  pRoleArn_
  pLogGroupArn_ =
    PutEmailMonitoringConfiguration'
      { organizationId =
          pOrganizationId_,
        roleArn = pRoleArn_,
        logGroupArn = pLogGroupArn_
      }

-- | The ID of the organization for which the email monitoring configuration
-- is set.
putEmailMonitoringConfiguration_organizationId :: Lens.Lens' PutEmailMonitoringConfiguration Prelude.Text
putEmailMonitoringConfiguration_organizationId = Lens.lens (\PutEmailMonitoringConfiguration' {organizationId} -> organizationId) (\s@PutEmailMonitoringConfiguration' {} a -> s {organizationId = a} :: PutEmailMonitoringConfiguration)

-- | The Amazon Resource Name (ARN) of the IAM Role associated with the email
-- monitoring configuration.
putEmailMonitoringConfiguration_roleArn :: Lens.Lens' PutEmailMonitoringConfiguration Prelude.Text
putEmailMonitoringConfiguration_roleArn = Lens.lens (\PutEmailMonitoringConfiguration' {roleArn} -> roleArn) (\s@PutEmailMonitoringConfiguration' {} a -> s {roleArn = a} :: PutEmailMonitoringConfiguration)

-- | The Amazon Resource Name (ARN) of the CloudWatch Log group associated
-- with the email monitoring configuration.
putEmailMonitoringConfiguration_logGroupArn :: Lens.Lens' PutEmailMonitoringConfiguration Prelude.Text
putEmailMonitoringConfiguration_logGroupArn = Lens.lens (\PutEmailMonitoringConfiguration' {logGroupArn} -> logGroupArn) (\s@PutEmailMonitoringConfiguration' {} a -> s {logGroupArn = a} :: PutEmailMonitoringConfiguration)

instance
  Core.AWSRequest
    PutEmailMonitoringConfiguration
  where
  type
    AWSResponse PutEmailMonitoringConfiguration =
      PutEmailMonitoringConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutEmailMonitoringConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutEmailMonitoringConfiguration
  where
  hashWithSalt
    _salt
    PutEmailMonitoringConfiguration' {..} =
      _salt `Prelude.hashWithSalt` organizationId
        `Prelude.hashWithSalt` roleArn
        `Prelude.hashWithSalt` logGroupArn

instance
  Prelude.NFData
    PutEmailMonitoringConfiguration
  where
  rnf PutEmailMonitoringConfiguration' {..} =
    Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf logGroupArn

instance
  Data.ToHeaders
    PutEmailMonitoringConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.PutEmailMonitoringConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutEmailMonitoringConfiguration where
  toJSON PutEmailMonitoringConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just ("RoleArn" Data..= roleArn),
            Prelude.Just ("LogGroupArn" Data..= logGroupArn)
          ]
      )

instance Data.ToPath PutEmailMonitoringConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery PutEmailMonitoringConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutEmailMonitoringConfigurationResponse' smart constructor.
data PutEmailMonitoringConfigurationResponse = PutEmailMonitoringConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutEmailMonitoringConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putEmailMonitoringConfigurationResponse_httpStatus' - The response's http status code.
newPutEmailMonitoringConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutEmailMonitoringConfigurationResponse
newPutEmailMonitoringConfigurationResponse
  pHttpStatus_ =
    PutEmailMonitoringConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
putEmailMonitoringConfigurationResponse_httpStatus :: Lens.Lens' PutEmailMonitoringConfigurationResponse Prelude.Int
putEmailMonitoringConfigurationResponse_httpStatus = Lens.lens (\PutEmailMonitoringConfigurationResponse' {httpStatus} -> httpStatus) (\s@PutEmailMonitoringConfigurationResponse' {} a -> s {httpStatus = a} :: PutEmailMonitoringConfigurationResponse)

instance
  Prelude.NFData
    PutEmailMonitoringConfigurationResponse
  where
  rnf PutEmailMonitoringConfigurationResponse' {..} =
    Prelude.rnf httpStatus
