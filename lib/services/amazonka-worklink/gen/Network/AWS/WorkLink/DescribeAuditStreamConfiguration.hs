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
-- Module      : Amazonka.WorkLink.DescribeAuditStreamConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the configuration for delivering audit streams to the customer
-- account.
module Amazonka.WorkLink.DescribeAuditStreamConfiguration
  ( -- * Creating a Request
    DescribeAuditStreamConfiguration (..),
    newDescribeAuditStreamConfiguration,

    -- * Request Lenses
    describeAuditStreamConfiguration_fleetArn,

    -- * Destructuring the Response
    DescribeAuditStreamConfigurationResponse (..),
    newDescribeAuditStreamConfigurationResponse,

    -- * Response Lenses
    describeAuditStreamConfigurationResponse_auditStreamArn,
    describeAuditStreamConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkLink.Types

-- | /See:/ 'newDescribeAuditStreamConfiguration' smart constructor.
data DescribeAuditStreamConfiguration = DescribeAuditStreamConfiguration'
  { -- | The ARN of the fleet.
    fleetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAuditStreamConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetArn', 'describeAuditStreamConfiguration_fleetArn' - The ARN of the fleet.
newDescribeAuditStreamConfiguration ::
  -- | 'fleetArn'
  Prelude.Text ->
  DescribeAuditStreamConfiguration
newDescribeAuditStreamConfiguration pFleetArn_ =
  DescribeAuditStreamConfiguration'
    { fleetArn =
        pFleetArn_
    }

-- | The ARN of the fleet.
describeAuditStreamConfiguration_fleetArn :: Lens.Lens' DescribeAuditStreamConfiguration Prelude.Text
describeAuditStreamConfiguration_fleetArn = Lens.lens (\DescribeAuditStreamConfiguration' {fleetArn} -> fleetArn) (\s@DescribeAuditStreamConfiguration' {} a -> s {fleetArn = a} :: DescribeAuditStreamConfiguration)

instance
  Core.AWSRequest
    DescribeAuditStreamConfiguration
  where
  type
    AWSResponse DescribeAuditStreamConfiguration =
      DescribeAuditStreamConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAuditStreamConfigurationResponse'
            Prelude.<$> (x Core..?> "AuditStreamArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeAuditStreamConfiguration

instance
  Prelude.NFData
    DescribeAuditStreamConfiguration

instance
  Core.ToHeaders
    DescribeAuditStreamConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeAuditStreamConfiguration where
  toJSON DescribeAuditStreamConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("FleetArn" Core..= fleetArn)]
      )

instance Core.ToPath DescribeAuditStreamConfiguration where
  toPath =
    Prelude.const "/describeAuditStreamConfiguration"

instance
  Core.ToQuery
    DescribeAuditStreamConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAuditStreamConfigurationResponse' smart constructor.
data DescribeAuditStreamConfigurationResponse = DescribeAuditStreamConfigurationResponse'
  { -- | The ARN of the Amazon Kinesis data stream that will receive the audit
    -- events.
    auditStreamArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAuditStreamConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'auditStreamArn', 'describeAuditStreamConfigurationResponse_auditStreamArn' - The ARN of the Amazon Kinesis data stream that will receive the audit
-- events.
--
-- 'httpStatus', 'describeAuditStreamConfigurationResponse_httpStatus' - The response's http status code.
newDescribeAuditStreamConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAuditStreamConfigurationResponse
newDescribeAuditStreamConfigurationResponse
  pHttpStatus_ =
    DescribeAuditStreamConfigurationResponse'
      { auditStreamArn =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ARN of the Amazon Kinesis data stream that will receive the audit
-- events.
describeAuditStreamConfigurationResponse_auditStreamArn :: Lens.Lens' DescribeAuditStreamConfigurationResponse (Prelude.Maybe Prelude.Text)
describeAuditStreamConfigurationResponse_auditStreamArn = Lens.lens (\DescribeAuditStreamConfigurationResponse' {auditStreamArn} -> auditStreamArn) (\s@DescribeAuditStreamConfigurationResponse' {} a -> s {auditStreamArn = a} :: DescribeAuditStreamConfigurationResponse)

-- | The response's http status code.
describeAuditStreamConfigurationResponse_httpStatus :: Lens.Lens' DescribeAuditStreamConfigurationResponse Prelude.Int
describeAuditStreamConfigurationResponse_httpStatus = Lens.lens (\DescribeAuditStreamConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeAuditStreamConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeAuditStreamConfigurationResponse)

instance
  Prelude.NFData
    DescribeAuditStreamConfigurationResponse
