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
-- Module      : Amazonka.WorkMail.DescribeEmailMonitoringConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the current email monitoring configuration for a specified
-- organization.
module Amazonka.WorkMail.DescribeEmailMonitoringConfiguration
  ( -- * Creating a Request
    DescribeEmailMonitoringConfiguration (..),
    newDescribeEmailMonitoringConfiguration,

    -- * Request Lenses
    describeEmailMonitoringConfiguration_organizationId,

    -- * Destructuring the Response
    DescribeEmailMonitoringConfigurationResponse (..),
    newDescribeEmailMonitoringConfigurationResponse,

    -- * Response Lenses
    describeEmailMonitoringConfigurationResponse_logGroupArn,
    describeEmailMonitoringConfigurationResponse_roleArn,
    describeEmailMonitoringConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newDescribeEmailMonitoringConfiguration' smart constructor.
data DescribeEmailMonitoringConfiguration = DescribeEmailMonitoringConfiguration'
  { -- | The ID of the organization for which the email monitoring configuration
    -- is described.
    organizationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEmailMonitoringConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'describeEmailMonitoringConfiguration_organizationId' - The ID of the organization for which the email monitoring configuration
-- is described.
newDescribeEmailMonitoringConfiguration ::
  -- | 'organizationId'
  Prelude.Text ->
  DescribeEmailMonitoringConfiguration
newDescribeEmailMonitoringConfiguration
  pOrganizationId_ =
    DescribeEmailMonitoringConfiguration'
      { organizationId =
          pOrganizationId_
      }

-- | The ID of the organization for which the email monitoring configuration
-- is described.
describeEmailMonitoringConfiguration_organizationId :: Lens.Lens' DescribeEmailMonitoringConfiguration Prelude.Text
describeEmailMonitoringConfiguration_organizationId = Lens.lens (\DescribeEmailMonitoringConfiguration' {organizationId} -> organizationId) (\s@DescribeEmailMonitoringConfiguration' {} a -> s {organizationId = a} :: DescribeEmailMonitoringConfiguration)

instance
  Core.AWSRequest
    DescribeEmailMonitoringConfiguration
  where
  type
    AWSResponse DescribeEmailMonitoringConfiguration =
      DescribeEmailMonitoringConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEmailMonitoringConfigurationResponse'
            Prelude.<$> (x Data..?> "LogGroupArn")
            Prelude.<*> (x Data..?> "RoleArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeEmailMonitoringConfiguration
  where
  hashWithSalt
    _salt
    DescribeEmailMonitoringConfiguration' {..} =
      _salt `Prelude.hashWithSalt` organizationId

instance
  Prelude.NFData
    DescribeEmailMonitoringConfiguration
  where
  rnf DescribeEmailMonitoringConfiguration' {..} =
    Prelude.rnf organizationId

instance
  Data.ToHeaders
    DescribeEmailMonitoringConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.DescribeEmailMonitoringConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeEmailMonitoringConfiguration
  where
  toJSON DescribeEmailMonitoringConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Data..= organizationId)
          ]
      )

instance
  Data.ToPath
    DescribeEmailMonitoringConfiguration
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeEmailMonitoringConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEmailMonitoringConfigurationResponse' smart constructor.
data DescribeEmailMonitoringConfigurationResponse = DescribeEmailMonitoringConfigurationResponse'
  { -- | The Amazon Resource Name (ARN) of the CloudWatch Log group associated
    -- with the email monitoring configuration.
    logGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM Role associated with the email
    -- monitoring configuration.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEmailMonitoringConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroupArn', 'describeEmailMonitoringConfigurationResponse_logGroupArn' - The Amazon Resource Name (ARN) of the CloudWatch Log group associated
-- with the email monitoring configuration.
--
-- 'roleArn', 'describeEmailMonitoringConfigurationResponse_roleArn' - The Amazon Resource Name (ARN) of the IAM Role associated with the email
-- monitoring configuration.
--
-- 'httpStatus', 'describeEmailMonitoringConfigurationResponse_httpStatus' - The response's http status code.
newDescribeEmailMonitoringConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEmailMonitoringConfigurationResponse
newDescribeEmailMonitoringConfigurationResponse
  pHttpStatus_ =
    DescribeEmailMonitoringConfigurationResponse'
      { logGroupArn =
          Prelude.Nothing,
        roleArn = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The Amazon Resource Name (ARN) of the CloudWatch Log group associated
-- with the email monitoring configuration.
describeEmailMonitoringConfigurationResponse_logGroupArn :: Lens.Lens' DescribeEmailMonitoringConfigurationResponse (Prelude.Maybe Prelude.Text)
describeEmailMonitoringConfigurationResponse_logGroupArn = Lens.lens (\DescribeEmailMonitoringConfigurationResponse' {logGroupArn} -> logGroupArn) (\s@DescribeEmailMonitoringConfigurationResponse' {} a -> s {logGroupArn = a} :: DescribeEmailMonitoringConfigurationResponse)

-- | The Amazon Resource Name (ARN) of the IAM Role associated with the email
-- monitoring configuration.
describeEmailMonitoringConfigurationResponse_roleArn :: Lens.Lens' DescribeEmailMonitoringConfigurationResponse (Prelude.Maybe Prelude.Text)
describeEmailMonitoringConfigurationResponse_roleArn = Lens.lens (\DescribeEmailMonitoringConfigurationResponse' {roleArn} -> roleArn) (\s@DescribeEmailMonitoringConfigurationResponse' {} a -> s {roleArn = a} :: DescribeEmailMonitoringConfigurationResponse)

-- | The response's http status code.
describeEmailMonitoringConfigurationResponse_httpStatus :: Lens.Lens' DescribeEmailMonitoringConfigurationResponse Prelude.Int
describeEmailMonitoringConfigurationResponse_httpStatus = Lens.lens (\DescribeEmailMonitoringConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeEmailMonitoringConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeEmailMonitoringConfigurationResponse)

instance
  Prelude.NFData
    DescribeEmailMonitoringConfigurationResponse
  where
  rnf DescribeEmailMonitoringConfigurationResponse' {..} =
    Prelude.rnf logGroupArn
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf httpStatus
